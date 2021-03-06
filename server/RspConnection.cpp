// ----------------------------------------------------------------------------
// Remote Serial Protocol connection: definition

// Copyright (C) 2009, 2015 Embecosm Limited <www.embecosm.com>

// Contributor Jeremy Bennett <jeremy.bennett@embecosm.com>

// This file is part of the Embecosm AAP GDB server and simulator.

// This program is free software: you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or (at your
// option) any later version.

// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
// License for more details.

// You should have received a copy of the GNU Lesser General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// ----------------------------------------------------------------------------


#include <iostream>
#include <iomanip>

#include <cerrno>
#include <csignal>
#include <cstring>

#include <netdb.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
#include <poll.h>
#include <unistd.h>

#include "RspConnection.h"
#include "Utils.h"

using std::cerr;
using std::cout;
using std::dec;
using std::endl;
using std::flush;
using std::hex;
using std::setfill;
using std::setw;

// Define RSP_TRACE to turn on tracing of packets sent and received
// #define RSP_TRACE


//-----------------------------------------------------------------------------
//! Constructor when using a port number

//! Sets up various parameters

//! @param[in] _portNum   the port number to connect to
//! @param[in] _traceOnP  true if tracing is enabled.
//-----------------------------------------------------------------------------
RspConnection::RspConnection (int   _portNum,
			      bool  _traceOnP) :
  portNum (_portNum),
  traceOnP (_traceOnP),
  clientFd (-1)
{

}	// RspConnection ()


//-----------------------------------------------------------------------------
//! Destructor

//! Close the connection if it is still open
//-----------------------------------------------------------------------------
RspConnection::~RspConnection ()
{
  this->rspClose ();		// Don't confuse with any other close ()

}	// ~RspConnection ()


//-----------------------------------------------------------------------------
//! Get a new client connection.

//! Blocks until the client connection is available.

//! A lot of this code is copied from remote_open in gdbserver remote-utils.c.

//! This involves setting up a socket to listen on a socket for attempted
//! connections from a single GDB instance (we couldn't be talking to multiple
//! GDBs at once!).

//! The service is specified either as a port number in the Or1ksim
//! configuration (parameter rsp_port in section debug, default 51000) or as a
//! service name in the constant OR1KSIM_RSP_SERVICE.

//! If there is a catastrophic communication failure, service will be
//! terminated using sc_stop.

//! The protocol used for communication is specified in OR1KSIM_RSP_PROTOCOL.

//! @return  TRUE if the connection was established or can be retried. FALSE
//!          if the error was so serious the program must be aborted.
//-----------------------------------------------------------------------------
bool
RspConnection::rspConnect ()
{
  // Open a socket on which we'll listen for clients
  int  tmpFd = socket (PF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (tmpFd < 0)
    {
      cerr << "ERROR: Cannot open RSP socket" << endl;
      return  false;
    }

  // Allow rapid reuse of the port on this socket
  int  optval = 1;
  setsockopt (tmpFd, SOL_SOCKET, SO_REUSEADDR, (char *)&optval,
	      sizeof (optval));

  // Bind the port to the socket
  struct sockaddr_in  sockAddr;
  sockAddr.sin_family      = PF_INET;
  sockAddr.sin_port        = htons (portNum);
  sockAddr.sin_addr.s_addr = INADDR_ANY;

  if (bind (tmpFd, (struct sockaddr *) &sockAddr, sizeof (sockAddr)))
    {
      cerr << "ERROR: Cannot bind to RSP socket" << endl;
      return  false;
    }

  // Listen for (at most one) client
  if (listen (tmpFd, 1))
    {
      cerr << "ERROR: Cannot listen on RSP socket" << endl;
      return  false;
    }

  cout << "Listening for RSP on port " <<  portNum << endl << flush;

  // Accept a client which connects
  socklen_t  len;		// Size of the socket address
  clientFd = accept (tmpFd, (struct sockaddr *)&sockAddr, &len);

  if (-1 == clientFd)
    {
      cerr << "Warning: Failed to accept RSP client" << endl;
      return  true;			// OK to retry
    }

  // Enable TCP keep alive process
  optval = 1;
  setsockopt (clientFd, SOL_SOCKET, SO_KEEPALIVE, (char *)&optval,
	      sizeof (optval));

  // Don't delay small packets, for better interactive response (disable
  // Nagel's algorithm)
  optval = 1;
  setsockopt (clientFd, IPPROTO_TCP, TCP_NODELAY, (char *)&optval,
	      sizeof (optval));

  // Socket is no longer needed
  close (tmpFd);			// No longer need this
  signal (SIGPIPE, SIG_IGN);		// So we don't exit if client dies

  cout << "Remote debugging from host " << inet_ntoa (sockAddr.sin_addr)
       << endl;
  return true;

}	// rspConnect ()


//-----------------------------------------------------------------------------
//! Close a client connection if it is open
//-----------------------------------------------------------------------------
void
RspConnection::rspClose ()
{
  if (isConnected ())
    {
      cout << "Closing connection" << endl;
      close (clientFd);
      clientFd = -1;
    }
}	// rspClose ()


//-----------------------------------------------------------------------------
//! Report if we are connected to a client.

//! @return  TRUE if we are connected, FALSE otherwise
//-----------------------------------------------------------------------------
bool
RspConnection::isConnected ()
{
  return -1 != clientFd;

}	// isConnected ()


//-----------------------------------------------------------------------------
//! Get the next packet from the RSP connection

//! Modeled on the stub version supplied with GDB. This allows the user to
//! replace the character read function, which is why we get stuff a character
//! at at time.

//! Unlike the reference implementation, we don't deal with sequence
//! numbers. GDB has never used them, and this implementation is only intended
//! for use with GDB 6.8 or later. Sequence numbers were removed from the RSP
//! standard at GDB 5.0.

//! @param[in] pkt  The packet for storing the result.

//! @return  TRUE to indicate success, FALSE otherwise (means a communications
//!          failure)
//-----------------------------------------------------------------------------
bool
RspConnection::getPkt (RspPacket *pkt)
{
  // Keep getting packets, until one is found with a valid checksum
  while (true)
    {
      int            bufSize = pkt->getBufSize ();
      unsigned char  checksum;		// The checksum we have computed
      int            count;		// Index into the buffer
      int 	     ch;		// Current character


      // Wait around for the start character ('$'). Ignore all other
      // characters
      ch = getRspChar ();
      while (ch != '$')
	{
	  if (-1 == ch)
	    {
	      return  false;		// Connection failed
	    }
	  else
	    {
 	      ch = getRspChar ();
	    }
	}

      // Read until a '#' or end of buffer is found
      checksum =  0;
      count    =  0;
      while (count < bufSize - 1)
	{
	  ch = getRspChar ();

	  if (-1 == ch)
	    {
	      return  false;		// Connection failed
	    }

	  // If we hit a start of line char begin all over again
	  if ('$' == ch)
	    {
	      checksum =  0;
	      count    =  0;

	      continue;
	    }

	  // Break out if we get the end of line char
	  if ('#' == ch)
	    {
	      break;
	    }

	  // Update the checksum and add the char to the buffer
	  checksum         = checksum + (unsigned char)ch;
	  pkt->data[count] = (char)ch;
	  count++;
	}

      // Mark the end of the buffer with EOS - it's convenient for non-binary
      // data to be valid strings.
      pkt->data[count] = 0;
      pkt->setLen (count);

      // If we have a valid end of packet char, validate the checksum. If we
      // don't it's because we ran out of buffer in the previous loop.
      if ('#' == ch)
	{
	  unsigned char  xmitcsum;	// The checksum in the packet

	  ch = getRspChar ();
	  if (-1 == ch)
	    {
	      return  false;		// Connection failed
	    }
	  xmitcsum = Utils::char2Hex (ch) << 4;

	  ch = getRspChar ();
	  if (-1 == ch)
	    {
	      return  false;			// Connection failed
	    }

	  xmitcsum += Utils::char2Hex (ch);

	  // If the checksums don't match print a warning, and put the
	  // negative ack back to the client. Otherwise put a positive ack.
	  if (checksum != xmitcsum)
	    {
	      cerr << "Warning: Bad RSP checksum: Computed 0x"
			<< setw (2) << setfill ('0') << hex
			<< checksum << ", received 0x" << xmitcsum
			<< setfill (' ') << dec << endl;
	      if (!putRspChar ('-'))		// Failed checksum
		{
		  return  false;		// Comms failure
		}
	    }
	  else
	    {
	      if (!putRspChar ('+'))		// successful transfer
		{
		  return  false;		// Comms failure
		}
	      else
		{
		  if (traceOnP)
		    {
		      cout << "RSP trace: getPkt: " << *pkt << endl;
		    }

		  return  true;			// Success
		}
	    }
	}
      else
	{
	  cerr << "Warning: RSP packet overran buffer" << endl;
	}
    }

}	// getPkt ()


//-----------------------------------------------------------------------------
//! Put the packet out on the RSP connection

//! Modeled on the stub version supplied with GDB. Put out the data preceded
//! by a '$', followed by a '#' and a one byte checksum. '$', '#', '*' and '}'
//! are escaped by preceding them with '}' and then XORing the character with
//! 0x20.

//! @param[in] pkt  The Packet to transmit

//! @return  TRUE to indicate success, FALSE otherwise (means a communications
//!          failure).
//-----------------------------------------------------------------------------
bool
RspConnection::putPkt (RspPacket *pkt)
{
  int  len = pkt->getLen ();
  int  ch;				// Ack char

  // Construct $<packet info>#<checksum>. Repeat until the GDB client
  // acknowledges satisfactory receipt.
  do
    {
      unsigned char checksum = 0;	// Computed checksum
      int           count    = 0;	// Index into the buffer

      if (!putRspChar ('$'))		// Start char
	{
	  return  false;		// Comms failure
	}


      // Body of the packet
      for (count = 0; count < len; count++)
	{
	  unsigned char  ch = pkt->data[count];

	  // Check for escaped chars
	  if (('$' == ch) || ('#' == ch) || ('*' == ch) || ('}' == ch))
	    {
	      ch       ^= 0x20;
	      checksum += (unsigned char)'}';
	      if (!putRspChar ('}'))
		{
		  return  false;	// Comms failure
		}

	    }

	  checksum += ch;
	  if (!putRspChar (ch))
	    {
	      return  false;		// Comms failure
	    }
	}

      if (!putRspChar ('#'))		// End char
	{
	  return  false;		// Comms failure
	}

      // Computed checksum
      if (!putRspChar (Utils::hex2Char (checksum >> 4)))
	{
	  return  false;		// Comms failure
	}
      if (!putRspChar (Utils::hex2Char (checksum % 16)))
	{
	  return  false;		// Comms failure
	}

      // Check for ack of connection failure
      ch = getRspChar ();
      if (-1 == ch)
	{
	  return  false;		// Comms failure
	}
    }
  while ('+' != ch);

  if (traceOnP)
    {
      cout << "RSP trace: putPkt: " << *pkt << endl;
    }

  return  true;

}	// putPkt ()


//-----------------------------------------------------------------------------
//! Put a single character out on the RSP connection

//! Utility routine. This should only be called if the client is open, but we
//! check for safety.

//! @param[in] c         The character to put out

//! @return  TRUE if char sent OK, FALSE if not (communications failure)
//-----------------------------------------------------------------------------
bool
RspConnection::putRspChar (char  c)
{
  if (-1 == clientFd)
    {
      cerr << "Warning: Attempt to write '" << c
		<< "' to unopened RSP client: Ignored" << endl;
      return  false;
    }

  // Write until successful (we retry after interrupts) or catastrophic
  // failure.
  while (true)
    {
      switch (write (clientFd, &c, sizeof (c)))
	{
	case -1:
	  // Error: only allow interrupts or would block
	  if ((EAGAIN != errno) && (EINTR != errno))
	    {
	      cerr << "Warning: Failed to write to RSP client: "
			<< "Closing client connection: "
			<<  strerror (errno) << endl;
	      return  false;
	    }

	  break;

	case 0:
	  break;		// Nothing written! Try again

	default:
	  return  true;		// Success, we can return
	}
    }
}	// putRspChar ()


//-----------------------------------------------------------------------------
//! Get a single character from the RSP connection

//! Utility routine. This should only be called if the client is open, but we
//! check for safety.

//! @return  The character received or -1 on failure
//-----------------------------------------------------------------------------
int
RspConnection::getRspChar ()
{
  if (-1 == clientFd)
    {
      cerr << "Warning: Attempt to read from "
	   << "unopened RSP client: Ignored" << endl;
      return  -1;
    }

  // Blocking read until successful (we retry after interrupts) or
  // catastrophic failure.
  while (true)
    {
      unsigned char  c;

      switch (read (clientFd, &c, sizeof (c)))
	{
	case -1:
	  // Error: only allow interrupts
	  if (EINTR != errno)
	    {
	      cerr << "Warning: Failed to read from RSP client: "
		   << "Closing client connection: "
		   <<  strerror (errno) << endl;
	      return  -1;
	    }
	  break;

	case 0:
	  return  -1;

	default:
	  return  c & 0xff;	// Success, we can return (no sign extend!)
	}
    }

}	// getRspChar ()


// Local Variables:
// mode: C++
// c-file-style: "gnu"
// show-trailing-whitespace: t
// End:
