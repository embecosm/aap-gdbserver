// ----------------------------------------------------------------------------
// GDB RSP server: declaration

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


#ifndef GDB_SERVER__H
#define GDB_SERVER__H

#include <cstdint>

#include "AapSim.h"
#include "MemAddr.h"
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
  GdbServer (int      _port,
	     AapSim * _sim,
	     int      _debugLevel);
  ~GdbServer ();

  // Main loop to listen for and service RSP requests.
  void  rspServer ();


private:

  //! Constants for the flag bits
  static const uint32_t MEMSPACE_CODE = 0x00000000;
  static const uint32_t MEMSPACE_DATA = 0x80000000;
  static const uint32_t MEMSPACE_MASK = 0x80000000;

  //! Breakpoint instruction (NOP R0,#3)
  uint16_t BREAK_INSTR = 0x0000;

  //! Definition of GDB target signals.  Data taken from the GDB source. Only
  //! those we use defined here.
  enum class GdbSignal : int {
    NONE =  0,
    ILL  =  4,
    TRAP =  5,
    ABRT =  6,
    EMT  =  7,
    SEGV = 11
  };

  //! Constant for a thread id
  static const int  PROXY_TID = 1;

  //! Constant for a trap instruction
  static const uint8_t  PROXY_TRAP_INSTR = 0xa5;

  //! Our associated simulated CPU
  AapSim  *mSim;

  //! Current debug level
  int  mDebugLevel;

  //! Our associated RSP interface (which we create)
  RspConnection *rsp;

  //! The packet pointer. There is only ever one packet in use at one time, so
  //! there is no need to repeatedly allocate and delete it.
  RspPacket *pkt;

  //! Hash table for matchpoints
  MpHash *mpHash;

  AapSim::Res  mLastException;

  // Main RSP request handler
  void  rspClientRequest ();

  // Handle the various RSP requests
  void  rspReportException (AapSim::Res  res);
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

  // Helper functions
  MemAddr::Space  getSpace (uint32_t gdbAddr);
  uint32_t  getLocation (uint32_t gdbAddr);
  bool  debugTraceRsp (void);
};	// GdbServer ()

#endif	// GDB_SERVER__H


// Local Variables:
// mode: C++
// c-file-style: "gnu"
// show-trailing-whitespace: t
// End:
