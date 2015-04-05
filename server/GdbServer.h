// ----------------------------------------------------------------------------
// GDB RSP server: definition

// Copyright (C) 2009  Embecosm Limited <info@embecosm.com>

// Contributor Jeremy Bennett <jeremy.bennett@embecosm.com>

// This file is part of the Embecosm Proxy RSP server.

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


#ifndef GDB_SERVER__H
#define GDB_SERVER__H

#include <stdint.h>

#include "SimProc.h"
#include "MpHash.h"
#include "RspConnection.h"
#include "RspPacket.h"


//-----------------------------------------------------------------------------
//! Module implementing a GDB RSP server.

//! A loop listens for RSP requests, which are converted to requests to read
//! and write registers, read and write memory, or control the CPU
//-----------------------------------------------------------------------------
class GdbServer
{
public:

  // Constructor and destructor
  GdbServer (int      rspPort,
	     SimProc *_cpu);
  ~GdbServer ();

  // Main loop to listen for and service RSP requests.
  void  rspServer ();


private:

  //! Definition of GDB target signals.

  //! Data taken from the GDB 6.8 source. Only those we use defined here.
  enum TargetSignal {
    TARGET_SIGNAL_NONE =  0,
    TARGET_SIGNAL_TRAP =  5
  };

  //! Constant for a thread id
  static const int  PROXY_TID = 1;

  //! Constant for a trap instruction
  static const uint8_t  PROXY_TRAP_INSTR = 0xa5;

  //! Out associated simulated CPU
  SimProc  *cpu;

  //! Our associated RSP interface (which we create)
  RspConnection *rsp;

  //! The packet pointer. There is only ever one packet in use at one time, so
  //! there is no need to repeatedly allocate and delete it.
  RspPacket *pkt;

  //! Hash table for matchpoints
  MpHash *mpHash;

  // Main RSP request handler
  void  rspClientRequest ();

  // Handle the various RSP requests
  void  rspReportException ();
  void  rspReadAllRegs ();
  void  rspWriteAllRegs ();
  void  rspReadMem ();
  void  rspWriteMem ();
  void  rspReadReg ();
  void  rspWriteReg ();
  void  rspQuery ();
  void  rspCommand ();
  void  rspSet ();
  void  rspRestart ();
  void  rspVpkt ();
  void  rspWriteMemBin ();
  void  rspRemoveMatchpoint ();
  void  rspInsertMatchpoint ();

};	// GdbServer ()

#endif	// GDB_SERVER__H


// Local Variables:
// mode: C++
// c-file-style: "gnu"
// show-trailing-whitespace: t
// End:
