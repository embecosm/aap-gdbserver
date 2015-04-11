// Simulated Processor: declaration

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

#ifndef AAP_SIM__H
#define AAP_SIM__H

#include <cstdint>

#include "MemAddr.h"

//! Representation of the AAP simulator

//! AAP consists of
//! - a set of between 4 and 32 16-bit registers
//! - a program counter
//! - a byte addressed data memory of 64 Kbytes
//! - a word addressed code memory of 16 Mwords

//! The core interface to the class is the ability to read and write registers
//! and both memories and to start execution, either continuously or
//! single-stepping.

class AapSim
{
public:

  //! Public type for the run result.  run () will only stop if something
  //! other than NONE has occurred.
  enum class Res {
    EXIT,			// NOP 1
    COUT,			// NOP 2, char to stdout
    CERR,			// NOP 3, char to stderr
    BREAK,			// NOP 4, breakpoint
    ILLINST,			// Illegal instruction
    ILLOPND,			// Illegal operand
    MEMERR,			// Invalid memory access
    REGERR,			// Invalid register
    NONE			// After STEP typically
  };

  //! Default constructor uses maximal num regs and mem size
  AapSim (void);

  //! Custom constructor must set appropriate values
  AapSim (const unsigned int  _numRegs,
	  const unsigned int  _codeWords,
	  const unsigned int  _dataBytes);

  //! Destructor
  ~AapSim ();

  //! Get the value of a general register
  uint16_t  reg (const unsigned int  regNum);

  //! Set the value of a general register
  void  reg (const unsigned int  regNum,
	     uint16_t  val);

  //! Get the PC
  MemAddr  pc (void);

  //! Set the PC
  void pc (const MemAddr  addr);

  //! Read memory location.  Whether the result is 16-bit or 8-bit depends on
  //! whether this is a code or data address.
  uint16_t  mem (const MemAddr  addr);

  //! Write word to memory location.  Use this for code.
  void  mem (const MemAddr  addr,
	     const uint16_t  val);

  //! Write byte to memory location.  Use this for data.
  void  mem (const MemAddr  addr,
	     const uint8_t  val);

  //! Execute a single instruction
  void  step (void);

  //! Execute code continuously
  void  run (void);

  //! Introspection.  Number of regs
  unsigned int  numRegs (void) const;

  //! Accessor to last executed instruction
  uint32_t instr (void) const;

  //! Accessor to status of last operation
  Res  res (void) const;


private:

  //! The registers
  uint16_t *mRegs;

  //! Number of regs (4-32)
  unsigned int  mNumRegs;

  //! The program counter
  MemAddr  mPc;

  //! The code memory
  uint16_t *mCodeMem;

  //! Amount of code memory (up to 2^24 words)
  unsigned int  mCodeWords;

  //! The data memory
  uint8_t *mDataMem;

  //! Amount of data memory (up to 2^16 bytes)
  unsigned int  mDataBytes;

  //! A cycle count
  unsigned long long int  mCycles;

  //! The instruction currently been executed/just executed.  This changes on
  //! a fetch at the start of instruction execution.
  uint32_t  mInstr;

  //! Status of current instruction execution
  Res  mRes;

  //! Step a 16-bit instruction
  void  step16 (void);

  //! Step a 32-bit instruction
  void  step32 (void);

  //! Get the value of a general register without reseting current inst status.
  uint16_t  rawReg (const unsigned int  regNum);

  //! Set the value of a general register without reseting current inst status.
  void  rawReg (const unsigned int  regNum,
		uint16_t  val);

  //! Get the PC without reseting current inst status.
  MemAddr  rawPc (void) const;

  //! Set the PC without reseting current inst status.
  void rawPc (const MemAddr  addr);

  //! Read memory location without reseting current inst status.  Whether the
  //! result is 16-bit or 8-bit depends on whether this is a code or data
  //! address.
  uint16_t  rawMem (const MemAddr  addr);

  //! Write word to memory location without reseting current inst status.  Use
  //! this for code.
  void  rawMem (const MemAddr  addr,
		const uint16_t  val);

  //! Write byte to memory location without reseting current inst status.  Use
  //! this for data.
  void  rawMem (const MemAddr  addr,
		const uint8_t  val);

  //! Helper method to validate a register
  bool  validReg (const unsigned int  regNum) const;

  //! Helper method to validate an address
  bool  validAddr (const MemAddr  addr) const;

  //! Sign extend to 16 bits.
  uint16_t  signExt (uint16_t      val,
		     unsigned int  bit);

  //! Sign extend to 32 bits.
  uint32_t  signExt (uint32_t      val,
		     unsigned int  bit);

  //! Read a double word from data memory
  uint16_t  readDataWord (uint16_t loc);

  //! Write a double word to data memory
  void  writeDataWord (uint16_t loc,
		       uint16_t val);

  //! Jump safely to a location
  void  jump (uint32_t  loc,
	      bool      setStatus = false);

  //! Get the carry bit
  uint16_t  carryBit (void) const;

  //! Set the carry bit
  void  carryBit (uint16_t  c);

  //! Helper to check 16-bit instruction word
  bool is16bit (uint16_t instr);

  //! Helper to check 32-bit instruction word
  bool is16bit (uint32_t instr);

  //! Helper method to get the next instruction.
  void  fetch (void);

};

//! Output operators
std::ostream &
operator<< (std::ostream & s,
	    AapSim::Res  res);

#endif	// AAP_SIM__H


// Local Variables:
// mode: C++
// c-file-style: "gnu"
// show-trailing-whitespace: t
// End:
