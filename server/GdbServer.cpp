// GDB RSP server: definition

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
#include <cstring>

#include "GdbServer.h"
#include "MemAddr.h"
#include "Utils.h"

using std::cout;
using std::cerr;
using std::dec;
using std::endl;
using std::hex;

//! Constructor for the GDB RSP server.

//! Allocate a packet data structure and a new RSP connection.

//! @param[in] _port  RSP port to use.
//! @param[in] _sim   The simulated CPU
GdbServer::GdbServer (int      _port,
		      AAPSim::AAPSimulator * _sim,
		      int      _debugLevel) :
  mSim (_sim),
  mDebugLevel (_debugLevel),
  mLastException (AAPSim::SimStatus::SIM_OK),
  clock_timeout(0)
{
  // Packet size should allow enough room for every register + 1 byte for an
  // end of string marker (our convenience). Assume worst case uint64_t regs.
  pkt       = new RspPacket ((sizeof (uint64_t) * mSim->getState().getNumRegs())
                             + 1);
  rsp       = new RspConnection (_port, false);
  mpHash    = new MpHash ();

}	// GdbServer ()


//! Destructor

//! Free up data structures
GdbServer::~GdbServer ()
{
  delete  mpHash;
  delete  rsp;
  delete  pkt;

}	// ~GdbServer


//! Main loop to listen for RSP requests

//! This only terminates if there was an error.
void
GdbServer::rspServer ()
{
  // Loop processing commands forever
  while (true)
    {
      // Make sure we are still connected.
      while (!rsp->isConnected ())
	{
	  // Reconnect and stall the processor on a new connection
	  if (!rsp->rspConnect ())
	    {
	      // Serious failure. Must abort execution.
	      cerr << "*** Unable to continue: ABORTING" << endl;
	      return;
	    }
	}

      // Get a RSP client request
      rspClientRequest ();
    }
}	// rspServer ()


//! Deal with a request from the GDB client session

//! In general, apart from the simplest requests, this function replies on
//! other functions to implement the functionality.

//! @note It is the responsibility of the recipient to delete the packet when
//!       it is finished with. It is permissible to reuse the packet for a
//!       reply.

//! @param[in] pkt  The received RSP packet
void
GdbServer::rspClientRequest ()
{
  AAPSim::SimStatus status = AAPSim::SimStatus::SIM_OK;
  if (!rsp->getPkt (pkt))
    {
      rsp->rspClose ();			// Comms failure
      return;
    }

  clock_t timeout_start;

  switch (pkt->data[0])
    {
    case '!':
      // Request for extended remote mode
      pkt->packStr ("OK");
      rsp->putPkt (pkt);
      return;

    case '?':
      // Return last signal ID
      rspReportException (mLastException);
      return;

    case 'A':
      // Initialization of argv not supported
      cerr << "Warning: RSP 'A' packet not supported: ignored" << endl;
      pkt->packStr ("E01");
      rsp->putPkt (pkt);
      return;

    case 'b':
      // Setting baud rate is deprecated
      cerr << "Warning: RSP 'b' packet is deprecated and not "
	   << "supported: ignored" << endl;
      return;

    case 'B':
      // Breakpoints should be set using Z packets
      cerr << "Warning: RSP 'B' packet is deprecated (use 'Z'/'z' "
	   << "packets instead): ignored" << endl;
      return;

    case 'c':
    case 'C':
      // Run.  We don't have signals, so we can
      // do C here as well.
      timeout_start = std::clock();

      while (status == AAPSim::SimStatus::SIM_OK) {
        if (clock_timeout != 0 && ((clock() - timeout_start) > clock_timeout))
          {
            rspReportException (AAPSim::SimStatus::SIM_TIMEOUT);
            return;
          }
        status = mSim->step ();
      }
      rspReportException (status);
      return;

    case 'd':
      // Disable debug using a general query
      cerr << "Warning: RSP 'd' packet is deprecated (define a 'Q' "
	   << "packet instead: ignored" << endl;
      return;

    case 'D':
      // Detach GDB. Do this by closing the client. The rules say that
      // execution should continue, so unstall the processor.
      pkt->packStr("OK");
      rsp->putPkt (pkt);
      rsp->rspClose ();
      return;

    case 'F':
      // File I/O is not currently supported
      cerr << "Warning: RSP file I/O not currently supported: 'F' "
	   << "packet ignored" << endl;
      return;

    case 'g':
      rspReadAllRegs ();
      return;

    case 'G':
      rspWriteAllRegs ();
      return;

    case 'H':
      // Set the thread number of subsequent operations. For now ignore
      // silently and just reply "OK"
      pkt->packStr ("OK");
      rsp->putPkt (pkt);
      return;

    case 'i':
    case 'I':
      // Single cycle step not supported.
      pkt->packStr ("");
      rsp->putPkt (pkt);
      return;

    case 'k':
      // Kill request. Do nothing for now.
      return;

    case 'm':
      // Read memory (symbolic)
      rspReadMem ();
      return;

    case 'M':
      // Write memory (symbolic)
      rspWriteMem ();
      return;

    case 'p':
      // Read a register
      rspReadReg ();
      return;

    case 'P':
      // Write a register
      rspWriteReg ();
      return;

    case 'q':
      // Any one of a number of query packets
      rspQuery ();
      return;

    case 'Q':
      // Any one of a number of set packets
      rspSet ();
      return;

    case 'r':
      // Reset the system. Deprecated (use 'R' instead)
      cerr << "Warning: RSP 'r' packet is deprecated (use 'R' "
 		<< "packet instead): ignored" << endl;
      return;

    case 'R':
      // Restart the program being debugged. Nothing to do in the proxy.
      return;

    case 's':
    case 'S':
      // Single step one machine instruction.  We don't have signals, we can
      // do S here as well.

      rspReportException (mSim->step ());
      return;

    case 't':
      // Search. This is not well defined in the manual and for now we don't
      // support it. No response is defined.
      cerr << "Warning: RSP 't' packet not supported: ignored" << endl;
      return;

    case 'T':
      // Is the thread alive. We are bare metal, so don't have a thread
      // context. The answer is always "OK".
      pkt->packStr ("OK");
      rsp->putPkt (pkt);
      return;

    case 'v':
      // Any one of a number of packets to control execution
      rspVpkt ();
      return;

    case 'X':
      // Write memory (binary)
      rspWriteMemBin ();
      return;

    case 'z':
      // Remove a breakpoint/watchpoint.
      rspRemoveMatchpoint ();
      return;

    case 'Z':
      // Insert a breakpoint/watchpoint.
      rspInsertMatchpoint ();
      return;

    default:
      // Unknown commands are ignored
      cerr << "Warning: Unknown RSP request" << pkt->data << endl;
      return;
    }
}	// rspClientRequest ()


//! Send a packet acknowledging an exception has occurred

//! This is sent immediately we continue or step in any way. The only signal
//! we ever see in this implementation is a proxy trap.

//! @param[in] res  The simulator result
void
GdbServer::rspReportException (AAPSim::SimStatus  res)
{
  GdbServer::GdbSignal sig = GdbServer::GdbSignal::NONE;

  mLastException = res;

  switch (res)
    {
    case AAPSim::SimStatus::SIM_QUIT:
      sig = GdbServer::GdbSignal::EMT;
      break;
    case AAPSim::SimStatus::SIM_BREAKPOINT:
      sig = GdbServer::GdbSignal::TRAP;
      break;
    case AAPSim::SimStatus::SIM_INVALID_INSN:
      sig = GdbServer::GdbSignal::ILL;
      break;
    case AAPSim::SimStatus::SIM_TRAP:
      sig = GdbServer::GdbSignal::ILL;
      break;
    case AAPSim::SimStatus::SIM_OK:
      sig = GdbServer::GdbSignal::NONE;
      break;
    case AAPSim::SimStatus::SIM_TIMEOUT:
      sig = GdbServer::GdbSignal::XCPU;
      break;
    default:
      sig = GdbServer::GdbSignal::ABRT;
      break;
    }

  // Construct a signal received packet
  pkt->data[0] = 'S';
  pkt->data[1] = Utils::hex2Char ((int) sig >> 4);
  pkt->data[2] = Utils::hex2Char ((int) sig % 16);
  pkt->data[3] = '\0';
  pkt->setLen (strlen (pkt->data));

  rsp->putPkt (pkt);

}	// rspReportException ()


//! Handle a RSP read all registers request

//! This means getting the value of each simulated register and packing it
//! into the packet.

//! Each byte is packed as a pair of hex digits.
void
GdbServer::rspReadAllRegs ()
{
  int  pktSize = 0;

  // The 16-bit general registers. GDB client expects them to be packed
  // according to target endianness (which is little).
  bool tracestate = mSim->getState ().getTracing ();
  mSim->getState ().setTracing (false);
  for (unsigned int regNum = 0; regNum < mSim->getState().getNumRegs(); regNum++)
    {
      Utils::val2Hex (mSim->getState ().getReg (regNum), &(pkt->data[pktSize]),
                                                2, true);
      pktSize += 4;	// 2 chars per hex digit
    }
  mSim->getState ().setTracing (tracestate);

  // Add 32-bit PC on the end
  Utils::val2Hex (mSim->getState ().getPC (), &(pkt->data[pktSize]), 4, true);
  pktSize += 8;	// 2 chars per hex digit

  // Finalize the packet and send it
  pkt->data[pktSize] = 0;
  pkt->setLen (pktSize);
  rsp->putPkt (pkt);

}	// rspReadAllRegs ()


//! Handle a RSP write all registers request

//! Each value is written into the simulated register.
void
GdbServer::rspWriteAllRegs ()
{
  int  pktSize = 0;

  // The 16-bit general registers.  Endianness is little-endian.
  for (unsigned int  r = 0; r < mSim->getState ().getNumRegs (); r++)
    {
      mSim->getState ().setReg (r, Utils::hex2Val (&(pkt->data[pktSize]), 2,
                                                   true));
      pktSize += 4;	// 2 chars per hex digit
    }

  mSim->getState ().setPC ((uint32_t) Utils::hex2Val (&(pkt->data[pktSize]), 4,
                                                      true));

  // Acknowledge OK
  pkt->packStr ("OK");
  rsp->putPkt (pkt);

}	// rspWriteAllRegs ()


//! Handle a RSP read memory (symbolic) request

//! Syntax is:

//!   m<addr>,<length>:

//! The response is the bytes, lowest address first, encoded as pairs of hex
//! digits.

//! The length given is the number of bytes to be read.
void
GdbServer::rspReadMem ()
{
  unsigned int    gdbAddr;		// Where to read the memory
  int             len;			// Number of bytes to read

  if (2 != sscanf (pkt->data, "m%x,%x:", &gdbAddr, &len))
    {
      cerr << "Warning: Failed to recognize RSP read memory command: "
		<< pkt->data << endl;
      pkt->packStr ("E01");
      rsp->putPkt (pkt);
      return;
    }

  // Make sure we won't overflow the buffer (2 chars per byte)
  if ((len * 2) >= pkt->getBufSize())
    {
      cerr << "Warning: Memory read " << pkt->data
	   << " too large for RSP packet: truncated" << endl;
      len = (pkt->getBufSize() - 1) / 2;
    }

  int   off;			// Offset into the memory
  MemAddr  addr (getSpace (gdbAddr), getLocation (gdbAddr));

  if (addr.space () == MemAddr::Space::CODE)
    {
      // Refill buffer from code memory
      for (off = 0; off < len; off += 2)
	{
	  uint16_t  w = mSim->getState ().getCodeMem (addr.location() * 2);
          w |= (mSim->getState ().getCodeMem (addr.location() * 2 + 1) << 8);

	  if (/* Read was successful */ true)
	    {
	      // Little endian, so LS byte first
	      pkt->data[off * 2]     = Utils::hex2Char ((w >>  4) & 0xf);
	      pkt->data[off * 2 + 1] = Utils::hex2Char ( w        & 0xf);

	      // Only if we are not an odd byte at the end!
	      if ((off + 1) != len)
		{
		  pkt->data[off * 2 + 2] = Utils::hex2Char ((w >> 12) & 0xf);
		  pkt->data[off * 2 + 3] = Utils::hex2Char ((w >>  8) & 0xf);
		}
	    }
	  else
	    {
	      pkt->packStr ("E02");	// Bad address
	      rsp->putPkt (pkt);
	      return;
	    }

	  addr.location (addr.location () + 1);
	}
    }
  else
    {
      // Refill buffer from data memory
      for (off = 0; off < len; off++)
	{
	  uint8_t  c = mSim->getState ().getDataMem (addr.location ());

          if (/* Read was successful */ true)
	    {
	      pkt->data[off * 2]     = Utils::hex2Char ((c >>  4) & 0xf);
	      pkt->data[off * 2 + 1] = Utils::hex2Char ( c        & 0xf);
	    }
	  else
	    {
	      pkt->packStr ("E01");	// Bad address
	      rsp->putPkt (pkt);
	      return;
	    }

	  addr.location (addr.location () + 1);
	}
    }

  pkt->data[off * 2] = '\0';			// End of string
  pkt->setLen (strlen (pkt->data));
  rsp->putPkt (pkt);

}	// rsp_read_mem ()


//! Handle a RSP write memory (symbolic) request

//! Syntax is:

//!   m<addr>,<length>:<data>

//! The data is the bytes, lowest address first, encoded as pairs of hex
//! digits.

//! The length given is the number of bytes to be written.
void
GdbServer::rspWriteMem ()
{
  uint32_t  gdbAddr;			// Where to write the memory
  int       len;			// Number of bytes to write

  if (2 != sscanf (pkt->data, "M%x,%x:", &gdbAddr, &len))
    {
      cerr << "Warning: Failed to recognize RSP write memory "
		<< pkt->data << endl;
      pkt->packStr ("E01");
      rsp->putPkt (pkt);
      return;
    }

  // Find the start of the data and check there is the amount we expect.
  char *symDat = (char *)(memchr (pkt->data, ':', pkt->getBufSize())) + 1;
  int   datLen = pkt->getLen() - (symDat - pkt->data);

  // Sanity check
  if (len * 2 != datLen)
    {
      cerr << "Warning: Write of " << len * 2 << "digits requested, but "
		<< datLen << " digits supplied: packet ignored" << endl;
      pkt->packStr ("E01");
      rsp->putPkt (pkt);
      return;
    }

  MemAddr  addr (getSpace (gdbAddr), getLocation (gdbAddr));

  if (addr.space () == MemAddr::Space::CODE)
    {
      // Write the bytes to code memory (no check the address is OK here)

      for (int  off = 0; off < len; off += 2)
	{
	  uint16_t  w;

	  if ((off + 1) == len )
	    {
	      // Odd byte at the end

	      uint16_t  nyb1 = Utils::char2Hex (symDat[off * 2]);
	      uint16_t  nyb2 = Utils::char2Hex (symDat[off * 2 + 1]);
	      uint16_t  w = mSim->getState ().getCodeMem (addr.location() * 2);
              w |= (mSim->getState().getCodeMem (addr.location() * 2 + 1) << 8);

	      if (/* Read was unsuccessful */ false)
		{
		  pkt->packStr ("E02");	// Bad address
		  rsp->putPkt (pkt);
		  return;
		}

	      // Little endian byte

	      w &= 0xff00;
	      w |= (nyb1 << 4 | nyb2);
	    }
	  else
	    {
	      // Little endian, so LS byte first.

	      uint16_t  nyb1 = Utils::char2Hex (symDat[off * 2 + 2]);
	      uint16_t  nyb2 = Utils::char2Hex (symDat[off * 2 + 3]);
	      uint16_t  nyb3 = Utils::char2Hex (symDat[off * 2]);
	      uint16_t  nyb4 = Utils::char2Hex (symDat[off * 2 + 1]);

	      w = (nyb1 < 12) | (nyb2 << 8) | (nyb3 << 4) | nyb4;
	    }

	  mSim->getState ().setCodeMem (addr.location() * 2, w & 0xff);
          mSim->getState ().setCodeMem (addr.location() * 2 + 1, w >> 8);

	  if (/* Write was unsuccessful */ false)
	    {
	      pkt->packStr ("E03");	// Bad address
	      rsp->putPkt (pkt);
	      return;
	    }

	  addr.location (addr.location () + 1);
	}
    }
  else
    {
      // Write the bytes to code memory (no check the address is OK here)

      for (int  off = 0; off < len; off++)
	{
	  uint8_t  nyb1 = Utils::char2Hex (symDat[off * 2]);
	  uint8_t  nyb2 = Utils::char2Hex (symDat[off * 2 + 1]);

          mSim->getState ().setDataMem (addr.location(),
                                        (uint8_t) ((nyb1 << 4) | nyb2));

	  if (/* Write was unsuccessful */ false)
	    {
	      pkt->packStr ("E04");	// Bad address
	      rsp->putPkt (pkt);
	      return;
	    }

	  addr.location (addr.location () + 1);
	}
    }

  pkt->packStr ("OK");
  rsp->putPkt (pkt);

}	// rspWriteMem ()


//! Read a single register

//! The registers follow the sequence of general registers followed by PC,
//! followed by status reg. The register is returned as a sequence of bytes in
//! target endian order.

//! Each byte is packed as a pair of hex digits.

void
GdbServer::rspReadReg ()
{
  unsigned int  regNum;
  unsigned int  byteSize;
  uint64_t      value;

  // Break out the fields from the data
  if (1 != sscanf (pkt->data, "p%x", &regNum))
    {
      cerr << "Warning: Failed to recognize RSP read register command: "
		<< pkt->data << endl;
      pkt->packStr ("E01");
      rsp->putPkt (pkt);
      return;
    }

  // Get the relevant register. GDB client expects them to be packed according
  // to target endianness.
  if (regNum > mSim->getState().getNumRegs () + 1)
    {
      pkt->packStr ("E02");
      rsp->putPkt (pkt);
      return;
    }
  else if (regNum == mSim->getState().getNumRegs () + 1)
    {
      // Last reg is status
      byteSize = 1;
      value = mSim->getState ().getOverflow ();
    }
  else if (regNum == mSim->getState().getNumRegs ())
    {
      // Penultimate reg is PC
      byteSize = 4;
      value = mSim->getState ().getPC ();
    }
  else if (regNum == mSim->getState().getNumRegs ())
    {
      // Last reg is PC
      byteSize = 4;
      value = mSim->getState ().getPC ();
    }
  else
    {
      byteSize = 2;
      value = mSim->getState ().getReg (regNum);
    }

  if (/* Read was unsuccessful */ false)
    {
      pkt->packStr ("E03");
      rsp->putPkt (pkt);
      return;
    }

  Utils::val2Hex (value, pkt->data, byteSize, true);

  pkt->setLen (strlen (pkt->data));
  rsp->putPkt (pkt);

}	// rspReadReg ()


//! Write a single register

//! The registers follow the sequence of general registers followed by PC
//! followed by status reg. The register is returned as a sequence of bytes in
//! target endian order.

//! Each byte is packed as a pair of hex digits.

void
GdbServer::rspWriteReg ()
{
  unsigned int  regNum;
  char          valstr[sizeof (uint64_t) + 1];	// Allow for EOS on the string

  // Break out the fields from the data
  if (2 != sscanf (pkt->data, "P%x=%s", &regNum, valstr))
    {
      cerr << "Warning: Failed to recognize RSP write register command "
	   << pkt->data << endl;
      pkt->packStr ("E01");
      rsp->putPkt (pkt);
      return;
    }

  // Set the relevant register. GDB client expects them to be packed according
  // to target endianness.
  if (regNum > mSim->getState ().getNumRegs () + 1)
    {
      pkt->packStr ("E02");
      rsp->putPkt (pkt);
      return;
    }
  else if (regNum == mSim->getState ().getNumRegs () + 1)
    {
      // Do not (yet) support setting status
      pkt->packStr ("E02");
      rsp->putPkt (pkt);
      return;
    }
  else if (regNum == mSim->getState ().getNumRegs ())
    {
      // Penultimate reg is PC
      mSim->getState ().setPC (Utils::hex2Val (valstr, 4, true));
    }
  else
    {
      // General register
      mSim->getState ().setReg (regNum, Utils::hex2Val (valstr, 2, true));
    }

  if (/* Write was successful */ true)
    {
      pkt->packStr ("OK");
      rsp->putPkt (pkt);
    }
  else
    {
      pkt->packStr ("E03");
      rsp->putPkt (pkt);
    }
}	// rspWriteReg ()


//! Handle a RSP query request

void
GdbServer::rspQuery ()
{
  if (0 == strcmp ("qOffsets", pkt->data))
    {
      // Report any relocation. Not used by us
      pkt->packStr ("Text=0;Data=0;Bss=0");
      rsp->putPkt (pkt);
    }
  else if (0 == strncmp ("qRcmd,", pkt->data, strlen ("qRcmd,")))
    {
      // This is used to interface to commands to do "stuff"
      rspCommand ();
    }
  else if (0 == strncmp ("qSupported", pkt->data, strlen ("qSupported")))
    {
      // Report a list of the features we support. For now we just ignore any
      // supplied specific feature queries, but in the future these may be
      // supported as well. Note that the packet size allows for 'G' + all the
      // registers sent to us, or a reply to 'g' with all the registers and an
      // EOS so the buffer is a well formed string.
      sprintf (pkt->data, "PacketSize=%x", pkt->getBufSize());
      pkt->setLen (strlen (pkt->data));
      rsp->putPkt (pkt);
    }
  else
    {
      // Silently ignor anything we don't know
      pkt->packStr ("");
      rsp->putPkt (pkt);
    }
}	// rspQuery ()


//! Handle a RSP qRcmd request

//! The actual command follows the "qRcmd," in ASCII encoded to hex
void
GdbServer::rspCommand ()
{
  char *cmd = new char[pkt->getBufSize ()];
  int   timeout;

  Utils::hex2Ascii (cmd, &(pkt->data[strlen ("qRcmd,")]));

  cout << "RSP trace: qCmd," << cmd << endl;

  if (1 == sscanf (cmd, "timeout %d", &timeout))
    {
      clock_timeout = timeout * CLOCKS_PER_SEC;
    }

  pkt->packStr ("OK");
  rsp->putPkt (pkt);

  delete [] cmd;

}	// rspCommand ()


//! Handle a RSP set request

//! Currently none supported.

void
GdbServer::rspSet ()
{
  // Silently ignore everything.

  pkt->packStr ("");
  rsp->putPkt (pkt);

}	// rspSet ()


//! Handle a RSP 'v' packet

//! Currently none supported

void
GdbServer::rspVpkt ()
{
  // Silently ignore everything.

  pkt->packStr ("");
  rsp->putPkt (pkt);

}	// rspVpkt ()


//! Handle a RSP write memory (binary) request

//! Syntax is:

//!   X<addr>,<length>:

//! Followed by the specified number of bytes as raw binary. Response should be
//! "OK" if all copied OK, E<nn> if error <nn> has occurred.

//! The length given is the number of bytes to be written. The data buffer has
//! already been unescaped, so will hold this number of bytes.

void
GdbServer::rspWriteMemBin ()
{
  uint32_t  gdbAddr;			// Where to write the memory
  int       len;			// Number of bytes to write

  if (2 != sscanf (pkt->data, "X%x,%x:", &gdbAddr, &len))
    {
      cerr << "Warning: Failed to recognize RSP write memory command: %s"
	   << pkt->data << endl;
      pkt->packStr ("E01");
      rsp->putPkt (pkt);
      return;
    }

  // Find the start of the data and "unescape" it.
  uint8_t *bindat = (uint8_t *)(memchr (pkt->data, ':',
					pkt->getBufSize ())) + 1;
  int   off       = (char *)bindat - pkt->data;
  int   newLen    = Utils::rspUnescape ((char *)bindat, pkt->getLen () - off);

  // Sanity check
  if (newLen != len)
    {
      int  minLen = len < newLen ? len : newLen;

      cerr << "Warning: Write of " << len << " bytes requested, but "
	   << newLen << " bytes supplied. " << minLen << " will be written"
	   << endl;
      len = minLen;
    }


  MemAddr  addr (getSpace (gdbAddr), getLocation (gdbAddr));

  if (addr.space () == MemAddr::Space::CODE)
    {
      // Write the bytes to code memory.

      for (int i = 0; i < len; i += 2)
	{
	  uint16_t  w;

	  if ((off + 1) == len )
	    {
	      // Odd byte at the end
	      w = mSim->getState ().getCodeMem (addr.location() * 2);
              w |= mSim->getState ().getCodeMem (addr.location() * 2 + 1) << 8;

	      if (/* Read was unsuccessful */ false)
		{
		  pkt->packStr ("E02");	// Bad address
		  rsp->putPkt (pkt);
		  return;
		}

	      // Little endian byte

	      w &= 0xff00;
	      w |= bindat[i];
	    }
	  else
	    {
	      // Little endian, so LS byte first.
	      w = ((uint16_t) bindat[i + 1] << 8) | bindat[i];
	    }

	  mSim->getState ().setCodeMem(addr.location() * 2, w & 0xff);
          mSim->getState ().setCodeMem(addr.location() * 2 + 1, w >> 8);

	  if (/* Write was unsuccessful */ false)
	    {
	      pkt->packStr ("E03");	// Bad address
	      rsp->putPkt (pkt);
	      return;
	    }

	  addr.location (addr.location () + 1);
	}
    }
  else
    {
      // Write the bytes to data memory

      for (int i = 0; i < len; i++)
	{
	  mSim->getState ().setDataMem (addr.location(), bindat[i]);

	  if (/* Write was unsuccessful */ false)
	    {
	      pkt->packStr ("E04");	// Bad address
	      rsp->putPkt (pkt);
	      return;
	    }

	  addr.location (addr.location () + 1);
	}
    }

  pkt->packStr ("OK");
  rsp->putPkt (pkt);

}	// rspWriteMemBin ()


//! Handle a RSP remove breakpoint or matchpoint request

//! This checks that the matchpoint was actually set earlier. For software
//! (memory) breakpoints, the breakpoint is cleared from memory.

void
GdbServer::rspRemoveMatchpoint ()
{
  MpHash::MpType     type; 		// What sort of matchpoint
  unsigned long int  gdbAddr;		// Address specified
  uint16_t           instr;		// Instruction value found
  unsigned int       len;		// Matchpoint length

  // Break out the instruction

  if (3 != sscanf (pkt->data, "z%1d,%lx,%1u", (int *)&type, &gdbAddr, &len))
    {
      cerr << "Warning: RSP matchpoint deletion request not "
	   << "recognized: ignored" << endl;
      pkt->packStr ("E01");
      rsp->putPkt (pkt);
      return;
    }

  // Sanity checks

  if (len > sizeof (instr))
    {
      cerr << "Warning: RSP remove breakpoint instruction length " << len
	   << " exceeds maximum of " << sizeof (instr) << endl;
      pkt->packStr ("E02");
      rsp->putPkt (pkt);
      return;
    }

  MemAddr  addr (getSpace (gdbAddr), getLocation (gdbAddr));

  if (addr.space () != MemAddr::Space::CODE)
    {
      cerr << "Warning: RSP set breakpoint in data at 0x" << hex << gdbAddr
	   << dec << endl;
      pkt->packStr ("E03");
      rsp->putPkt (pkt);
      return;
    }

  // Sort out the type of matchpoint

  switch (type)
    {
    case MpHash::MpType::BP_MEMORY:

      // Software (memory) breakpoint

      if (mpHash->remove (type, addr, instr))
	{
	  if (debugTraceRsp ())
	    {
	      cout << "RSP trace: software (memory) breakpoint removed from 0x"
		   << hex << addr << dec << endl;
	    }
	}
      else
	{
	  cerr << "Warning: failed to remove software (memory) breakpoint "
	          "from 0x" << hex << addr << dec << endl;
	  pkt->packStr ("E04");
	  rsp->putPkt (pkt);
	}

      // Remove the breakpoint from memory. The endianness of the instruction
      // matches that of the memory.
      mSim->getState ().setCodeMem (addr.location() * 2, instr & 0xff);
      mSim->getState ().setCodeMem (addr.location() * 2 + 1, instr >> 8);

      if (/* Write was successful */ true)
	{
	  pkt->packStr ("OK");
	  rsp->putPkt (pkt);
	}
      else
	{
	  cerr << "Warning: failed to write memory when clearing "
	       << "breakpoint at 0x" << hex << addr << dec << endl;
	  pkt->packStr ("E05");
	  rsp->putPkt (pkt);
	}

      return;

    case MpHash::MpType::BP_HARDWARE:
    case MpHash::MpType::WP_WRITE:
    case MpHash::MpType::WP_READ:
    case MpHash::MpType::WP_ACCESS:

      // Hardware breakpoint and all types of watchpoint not supported
      pkt->packStr ("");
      rsp->putPkt (pkt);
      return;

    default:
      cerr << "Warning: RSP matchpoint type " << type
	   << " not recognized: ignored" << endl;
      pkt->packStr ("E06");
      rsp->putPkt (pkt);
      return;
    }
}	// rspRemoveMatchpoint ()


//! Handle a RSP insert breakpoint or matchpoint request

//! This does nothing with the proxy, but records that the matchpoint has been
//! set up, so it can be correlated with later remove packets.

void
GdbServer::rspInsertMatchpoint ()
{
  MpHash::MpType     type;		// What sort of matchpoint
  unsigned long int  gdbAddr;		// Address specified
  uint16_t           instr;		// Instruction value found
  unsigned int       len;		// Matchpoint length

  // Break out the instruction
  if (3 != sscanf (pkt->data, "Z%1d,%lx,%1u", (int *)&type, &gdbAddr, &len))
    {
      cerr << "Warning: RSP matchpoint insertion request not "
	   << "recognized: ignored" << endl;
      pkt->packStr ("E01");
      rsp->putPkt (pkt);
      return;
    }

  // Sanity checks
  if (len > sizeof (instr))
    {
      cerr << "Warning: RSP set breakpoint instruction length " << len
	   << " exceeds maximum of " << sizeof (instr) << endl;
      pkt->packStr ("E02");
      rsp->putPkt (pkt);
      return;
    }

  MemAddr  addr (getSpace (gdbAddr), getLocation (gdbAddr));

  if (addr.space () != MemAddr::Space::CODE)
    {
      cerr << "Warning: RSP set breakpoint in data at 0x" << hex << gdbAddr
	   << dec << endl;
      pkt->packStr ("E03");
      rsp->putPkt (pkt);
      return;
    }

  // Sort out the type of matchpoint

  switch (type)
    {
    case MpHash::MpType::BP_MEMORY:

      // Software (memory) breakpoint. Extract the instruction.

      instr = mSim->getState ().getCodeMem (addr.location() * 2);
      instr |= mSim->getState ().getCodeMem (addr.location() * 2 + 1) << 8;

      if (/* Read was unsuccessful */ false)
	{
	  cerr << "Warnign: failed to read memory when setting breakpoint at 0x"
	       << hex << addr << dec << endl;
	  pkt->packStr ("E03");
	  rsp->putPkt (pkt);
	}

      // Record the breakpoint and attempt to write a proxy marker (only one
      // byte)

      mpHash->add (type, addr, instr);
      mSim->getState ().setCodeMem (addr.location() * 2, BREAK_INSTR & 0xff);
      mSim->getState ().setCodeMem (addr.location() * 2 + 1, BREAK_INSTR >> 8);

      if (/* Write was successful */ true)
	{
	  if (debugTraceRsp ())
	    {
	      cout << "RSP trace: software (memory) breakpoint inserted at 0x"
		   << hex << addr << dec << endl;
	    }

	  pkt->packStr ("OK");
	  rsp->putPkt (pkt);
	  return;
	}
      else
	{
	  cerr << "Warning: failed to write memory when setting software "
	          "(memory) breakpoint at 0x" << hex << addr << dec << endl;
	  pkt->packStr ("E04");
	  rsp->putPkt (pkt);
	}

    case MpHash::MpType::BP_HARDWARE:
    case MpHash::MpType::WP_WRITE:
    case MpHash::MpType::WP_READ:
    case MpHash::MpType::WP_ACCESS:

      // Hardware breakpoint and all watchpoints unsupported.

      pkt->packStr ("");
      rsp->putPkt (pkt);

      return;

    default:

      cerr << "Warning: RSP matchpoint type " << type
	   << "not recognized: ignored"<< endl;
      pkt->packStr ("E05");
      rsp->putPkt (pkt);
      return;
    }
}	// rspInsertMatchpoint ()


//! Translate GDB flag bits into simulator memory space

//! @param[in] gdbAddr  The GDB byte address with flags.
//! @return  The simulator memory space

MemAddr::Space
GdbServer::getSpace (uint32_t gdbAddr)
{
  switch (uint32_t (gdbAddr & MEMSPACE_MASK))
    {
    case MEMSPACE_CODE:  return  MemAddr::Space::CODE;
    case MEMSPACE_DATA:  return  MemAddr::Space::DATA;

    default:		// This can't happen, but the compiler doesn't realize!

      return  MemAddr::Space::DATA;
    }
}	// getSpace ()


//! Turn a GDB byte address into a simulator location.

//! We make any code address a word location

//! @param[in] gdbAddr  The GDB byte address with flags.
//! @return  The simulator memory location

uint32_t
GdbServer::getLocation (uint32_t gdbAddr)
{
  switch (uint32_t (gdbAddr & MEMSPACE_MASK))
    {
    case MEMSPACE_CODE:  return (gdbAddr & ~MEMSPACE_MASK) >> 1;
    case MEMSPACE_DATA:  return  gdbAddr & ~MEMSPACE_MASK;

    default:		// This can't happen, but the compiler doesn't realize!

      return  0;
    }
}	// getLocation ()


//! Determine particular trace levels

//! @return  TRUE if the debug RSP trace is on

bool
GdbServer::debugTraceRsp (void)
{
  return  (mDebugLevel & 0x0001) == 0x0001;

}	// debugTraceRsp ()


// Local Variables:
// mode: C++
// c-file-style: "gnu"
// show-trailing-whitespace: t
// End:
