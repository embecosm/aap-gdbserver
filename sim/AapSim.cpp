// Simulated Processor: definition

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

#include <cstring>
#include <unistd.h>

#include "AapSim.h"


//! Constructor.

//! Allocate registers and memory space, initializing everything
//!appropriately.

//! @param[in]  _numRegs    Number of registers (4-32);
//! @param[in]  _codeWords  Size of code memory (up to 2^24 words)
//! @param[in]  _dataBytes  Size of data memory (up to 2^16 bytes)

AapSim::AapSim (const unsigned int  _numRegs,
		const unsigned int  _codeWords,
		const unsigned int  _dataBytes) :
  mNumRegs (_numRegs),
  mRegs (NULL),
  mCodeWords (_codeWords),
  mCodeMem (NULL),
  mDataBytes (_dataBytes),
  mDataMem (NULL),
  mCycles (0ULL),
  mRes (AapSim::Res::NONE)
{
  // Validate
  if ((4 > _numRegs) || (_numRegs > 32))
    {
      cerr << "*** ABORT ***: Invalid number of regs " << _numRegs
	   << ": must be between 4 and 32. Exiting." << endl;
      exit (EXIT_FAILURE);
    }

  if (_codeWords > (1 << 24))
    {
      cerr << "*** ABORT ***: Invalid code memory size " << _codeWords
	   << " words: must be no more than 2^24 words. Exiting." << endl;
      exit (EXIT_FAILURE);
    }

  if (_dataBytes > (1 << 16))
    {
      cerr << "*** ABORT ***: Invalid data memory size " << _dataBytes
	   << " words: must be no more than 2^16 bytes. Exiting." << endl;
      exit (EXIT_FAILURE);
    }

  // Allocate space
  mRegs = new uint16_t [mNumRegs];
  mCodeMem = new uint16_t [mCodeWords];
  mDataMem = new uint8_t [mDataBytes];

  // Initialize
  mPc.space (MemAddr::Space::CODE);
  mPc.location (0);

  memset (mRegs, 0, sizeof (*mRegs) * mNumRegs);
  memset (mCodeMem, 0, sizeof (*mCodeMem) * mCodeWords);
  memset (mDataMem, 0, sizeof (*mDataMem) * mDataBytes);

}	// AapSim ()


//! Destructor.

//! Free up any space allocated

AapSim::~AapSim ()
{
  if (NULL != mRegs)
    delete [] mRegs;

  if (NULL != mCodeMem)
    delete [] mCodeMem;

  if (NULL != mDataMem)
    delete [] mDataMem;

}	// ~AapSim ()


//! Get the value of a general register

//! @param[in] regNum  The register to read
//! @return  The value in the register

uint16_t
AapSim::reg (const unsigned int  regNum)
{
  if (validReg (regNum))
    return  regs[regNum];
  else
    {
      mRes = AapSim::Res:REGERR;
      return 0;
    }
}	// reg ()


//! Set the value of a general register

//! @param[in] regNum  The register to write
//! @param[in] val     The value to write

void
AapSim::reg (const unsigned int  regNum,
	     uint16_t  val)
{
  if (validReg (regNum))
    regs [regNum] = val;
  else
    mRes = AapSim::Res:REGERR;

}	// reg ()


//! Get the value of the program counter

//! @return  The value in the register

MemAddr
AapSim::pc () const
{
  return  mPc;

}	// pc ()


//! Set the value of the program counter

//! @param[in] val  The value to write

void
AapSim::pc (mAddr  val)
{
  uint32_t loc = val.location ();

  if (MemAddr::Space::CODE != val.space ())
    // This is a real error, not something to silently handle with a res.
    cerr << "ERROR: Attempt to write data address " << loc
	 << " to PC: Ignored." << endl;
  else if (loc >= mCodeWords)
    mRes = AapSim::Res::MEMERR;
  else
    pc.location (loc);

}	// pc ()


//! Read from memory

//! If reading from memory, the value returned is a byte, if from code, a
//! word.

//! @param[in]  addr   The address to read
//! @return  The value read

uint16_t
AapSim::mem (const MemAddr  addr) const
{
  uint32_t  loc = addr.location ();

  switch (addr.space ())
    {
    case MemAddr::Space::CODE:

      if (!validAddr (addr))
	{
	  mRes = AapSim::Res::MEMERR;
	  return  0;
	}

      return codeMem[loc];

    case MemAddr::Space::DATA:

      if (!validAddr (addr))
	{
	  mRes = AapSim::Res::MEMERR;
	  return  0;
	}

      return dataMem[loc];

    default:

      cerr << "*** ABORT ***:  Unknown code space " << addr.space ()
	   << " when reading memory. Exiting.";
      exit (EXIT_FAILURE);
    }
}	// mem ()


//! Write a word to code memory

//! @note This is only for use with code memory

//! @param[in] addr  The address to write
//! @param[in] val   The word to write

void
AapSim::mem (MemAddr  addr,
	     uint16_t  val)
{
  uint32_t  loc = addr.location ();

  if (addr.space () != MemAddr::Space::CODE)
    {
      // A real error, not something for an error code.
      cerr << "ERROR: Attempt to write word to non-code location " << loc
	   << ": ignored." << endl;
      return;
    }

  if (!validAddr (addr))
    {
      mRes = AapSim::Res::MEMERR;
      return;
    }

  codeMem[loc] = val;

}	// mem () ;


//! Write a byte to data memory

//! @note This is only for use with data memory

//! @param[in] addr  The address to write
//! @param[in] val   The word to write

void
AapSim::mem (MemAddr  addr,
	     uint8_t  val)
{
  uint32_t  loc = addr.location ();

  if (addr.space () != MemAddr::Space::DATA)
    {
      // A real error, not something for an error code.
      cerr << "ERROR: Attempt to write word to non-data location " << loc
	   << ": ignored." << endl;
      return;
    }

  if (!validAddr (addr))
    {
      mRes = AapSim::Res::MEMERR;
      return;
    }

  dataMem[loc] = val;

}	// mem () ;


//! Execute one instruction

//! @return  The reason why we stopped.

AapSim::Res
AapSim::step (void)
{
  mRes = NONE;
  mInstr = fetch ();

  if (mRes != NONE)
    return  mRes;			// Fetch failed

  // Deal with the instruction

  uint8_t  majOp = (uint8_t) (mInstr >> 28) & 0xf;
  uint8_t  minOp = (uint8_t) (mInstr >> 24) & 0xf;
  bool     immOp = (mInstr & 0x00100000) == 0x00100000;
  uint8_t  rd;
  uint8_t  ra;
  uint8_t  rb;
  uint16_t  imm;

  switch (majOp)
    {
    case 0x0:

      // The main ALU operations.  Ensure we use accessor functions for regs
      // and memory, since this will sort out error codes for us.

      switch (minOp)
	{
	case 0x0:			// NOP

	  rd = (uint8_t) (mInstr >> 16) & 0x1f;
	  imm = (uint16_t) (mInstr & 0xffff);

	  switch (imm)
	    {
	    case 0:  mRes = AapSim::Res::NONE;    break;
	    case 1:  mRes = AapSim::Res::COUT;    break;
	    case 2:  mRes = AapSim::Res::CERR;    break;
	    case 3:  mRes = AapSim::Res::BREAK;   break;
	    case 4:  mRes = AapSim::Res::EXIT;    break;
	    default: mRes = AapSim::Res::ILLOPND; break;
	    }

	  mCycles++;
	  return  mRes;

	case 0x1:			// ADD

	  if (immOp)
	    {
	      rd = (uint8_t) (mInstr >> 16) & 0x1f;
	      ra = (uint8_t) (mInstr >> 11) & 0x1f;
	      imm = (uint16_t) (mInstr & 0x07ff);
	      reg (rd, reg (ra) + imm);
	      mCycles++;
	    }
	  else
	    {
	      rd = (uint8_t) (mInstr >> 10) & 0x1f;
	      ra = (uint8_t) (mInstr >> 5) & 0x1f;
	      rb = (uint8_t) mInstr & 0x1f;
	      reg (rd, reg (ra) + reg (rb))
	      mCycles++;
	    }

	  return;

	case 0x2:			// SUB

	  if (immOp)
	    {
	      rd = (uint8_t) (mInstr >> 16) & 0x1f;
	      ra = (uint8_t) (mInstr >> 11) & 0x1f;
	      imm = (uint16_t) (mInstr & 0x07ff);
	      reg (rd, reg (ra) - imm);
	      mCycles++;
	    }
	  else
	    {
	      rd = (uint8_t) (mInstr >> 10) & 0x1f;
	      ra = (uint8_t) (mInstr >> 5) & 0x1f;
	      rb = (uint8_t) mInstr & 0x1f;
	      reg (rd, reg (ra) - reg (rb));
	      mCycles++;
	    }

	  return;

	case 0x3:			// AND

	  if (immOp)
	    {
	      rd = (uint8_t) (mInstr >> 16) & 0x1f;
	      ra = (uint8_t) (mInstr >> 11) & 0x1f;
	      imm = (uint16_t) (mInstr & 0x07ff);
	      reg (rd, reg (ra) & imm);
	      mCycles++;
	    }
	  else
	    {
	      rd = (uint8_t) (mInstr >> 10) & 0x1f;
	      ra = (uint8_t) (mInstr >> 5) & 0x1f;
	      rb = (uint8_t) mInstr & 0x1f;
	      reg (rd, reg (ra) & reg (rb));
	      mCycles++;
	    }

	  return;

	case 0x4:			// OR

	  if (immOp)
	    {
	      rd = (uint8_t) (mInstr >> 16) & 0x1f;
	      ra = (uint8_t) (mInstr >> 11) & 0x1f;
	      imm = (uint16_t) (mInstr & 0x07ff);
	      reg (rd, reg (ra) | imm);
	      mCycles++;
	    }
	  else
	    {
	      rd = (uint8_t) (mInstr >> 10) & 0x1f;
	      ra = (uint8_t) (mInstr >> 5) & 0x1f;
	      rb = (uint8_t) mInstr & 0x1f;
	      reg (rd, reg (ra) | reg (rb));
	      mCycles++;
	    }

	  return;

	case 0x5:			// XOR

	  if (immOp)
	    {
	      rd = (uint8_t) (mInstr >> 16) & 0x1f;
	      ra = (uint8_t) (mInstr >> 11) & 0x1f;
	      imm = (uint16_t) (mInstr & 0x07ff);
	      reg (rd, reg (ra) ^ imm);
	      mCycles++;
	    }
	  else
	    {
	      rd = (uint8_t) (mInstr >> 10) & 0x1f;
	      ra = (uint8_t) (mInstr >> 5) & 0x1f;
	      rb = (uint8_t) mInstr & 0x1f;
	      reg (rd, reg (ra) ^ reg (rb));
	      mCycles++;
	    }

	  return;

	case 0x6:			// ASR

	  // Note that right shift of negative numbers is implementation
	  // dependent in C/C++.  However our values are explicitly unsigned,
	  // so we can guarantee we will by default get LSR.  We need to patch
	  // in the top bit.

	  if (immOp)
	    {
	      rd = (uint8_t) (mInstr >> 16) & 0x1f;
	      ra = (uint8_t) (mInstr >> 11) & 0x1f;
	      imm = (uint16_t) (mInstr & 0x07ff);
	      uint16_t mask = (reg (ra) & 0x8000) == 0x8000
		? 0xffff << (16 - imm) : 0;

	      if (imm > 16)
		mRes = AapSim::Res::ILLOPND;
	      else
		reg (rd, (reg (ra) >> imm) | mask);

	      mCycles++;
	    }
	  else
	    {
	      rd = (uint8_t) (mInstr >> 10) & 0x1f;
	      ra = (uint8_t) (mInstr >> 5) & 0x1f;
	      rb = (uint8_t) mInstr & 0x1f;
	      uint16_t mask = (reg (ra) & 0x8000) == 0x8000
		? 0xffff << (16 - reg (rb)) : 0;

	      if (reg (ra) > 16)
		mRes = AapSim::Res::ILLOPND;
	      else
		reg (rd, (reg (ra) >> reg (rb)) | mask);

	      mCycles++;
	    }

	  return;

	case 0x7:			// LSL

	  if (immOp)
	    {
	      rd = (uint8_t) (mInstr >> 16) & 0x1f;
	      ra = (uint8_t) (mInstr >> 11) & 0x1f;
	      imm = (uint16_t) (mInstr & 0x07ff);

	      if (imm > 16)
		mRes = AapSim::Res::ILLOPND;
	      else
		reg (rd, reg (ra) << imm);

	      mCycles++;
	    }
	  else
	    {
	      rd = (uint8_t) (mInstr >> 10) & 0x1f;
	      ra = (uint8_t) (mInstr >> 5) & 0x1f;
	      rb = (uint8_t) mInstr & 0x1f;

	      if (reg (ra) > 16)
		mRes = AapSim::Res::ILLOPND;
	      else
		reg (rd, reg (ra) << reg (rb));

	      mCycles++;
	    }

	  return;

	case 0x8:			// LSR

	  // Since registers are unsigned, this is guaranteed to be logical
	  // shift.

	  if (immOp)
	    {
	      rd = (uint8_t) (mInstr >> 16) & 0x1f;
	      ra = (uint8_t) (mInstr >> 11) & 0x1f;
	      imm = (uint16_t) (mInstr & 0x07ff);

	      if (imm > 16)
		mRes = AapSim::Res::ILLOPND;
	      else
		reg (rd, reg (ra) >> imm);

	      mCycles++;
	    }
	  else
	    {
	      rd = (uint8_t) (mInstr >> 10) & 0x1f;
	      ra = (uint8_t) (mInstr >> 5) & 0x1f;
	      rb = (uint8_t) mInstr & 0x1f;

	      if (reg (ra) > 16)
		mRes = AapSim::Res::ILLOPND;
	      else
		reg (rd, reg (ra) >> reg (rb));

	      mCycles++;
	    }

	  return;

	case 0x9:			// MOV

	  if (immOp)
	    {
	      rd = (uint8_t) (mInstr >> 16) & 0x1f;
	      imm = (uint16_t) (mInstr & 0x0fff);
	      reg (rd, imm);
	      mCycles++;
	    }
	  else
	    {
	      rd = (uint8_t) (mInstr >> 10) & 0x1f;
	      ra = (uint8_t) (mInstr >> 5) & 0x1f;
	      reg (rd, reg (ra));
	      mCycles++;
	    }

	  return;

	default:			// Unknown

	  mRes = AapSim::Res:ILLINST;
	  mCycles++;
	  return;
	}

    case 0x1:

      // Load store operations.  Minor op is just 2 bits

      minOp = (uint8_t) (mInstr >> 26) & 0x3;

      switch (minOp)
	{
	case 0x0:			// LOAD

	  rd = (uint8_t) (mInstr >> 16) & 0x1f;
	  ra = (uint8_t) (mInstr >> 21) & 0x1f;
	  imm = (uint16_t) (mInstr & 0xffff);
	  reg (rd, mem (MemAddr (MemAddr::Space::DATA, reg (ra) + imm)));
	  mCycles++;

	  return;

	case 0x0:			// LOAD

	  rd = (uint8_t) (mInstr >> 21) & 0x1f;
	  ra = (uint8_t) (mInstr >> 16) & 0x1f;
	  imm = (uint16_t) (mInstr & 0xffff);
	  mem (MemAddr (MemAddr::Space::DATA, reg (rd) + imm), reg (ra));
	  mCycles++;

	  return;

	default:			// Unknown

	  mRes = AapSim::Res:ILLINST;
	  mCycles++;
	  return;
	}

    case 0x4:

      // Jump operations. Minor op is just 1 bit, and immediate bit is moved.

      minOp = (uint8_t) (mInstr >> 27) & 0x1;
      immOp = (mInstr & 0x04000000) == 0x04000000;

      switch (minOp)
	{
	case 0x0:			// JAL

	  if (immOp)
	    {
	      imm = (uint16_t) (mInstr & 0x0fff);
	      reg (0, mPc);
	      pc (imm);
	      mCycles++;
	    }
	  else
	    {
	      // There is a peculiarity here, to do with RETURN being squeezed
	      // in here f bits 23..20 are 1111.  We don't really need this,
	      // since it is the same as JMP R0.

	      if ((mInstr & 0x00f00000) == 0x00f00000)	// RETURN
		{
		  pc (reg (0));
		  mCycles++;
		}
	      else
		{
		  rd = (uint8_t) mInstr & 0x1f;
		  reg (0, pc ());
		  pc (reg (rd));
		  mCycles++;
		}
	    }

	  return;

	case 0x1:			// JMP

	  if (immOp)
	    {
	      imm = (uint16_t) (mInstr & 0x0fff);
	      pc (imm);
	      mCycles++;
	    }
	  else
	    {
	      rd = (uint8_t) mInstr & 0x1f;
	      pc (reg (rd));
	      mCycles++;
	    }

	  return;

	default:			// Unknown

	  // For now this is impossible, but we might have more in the
	  // future.

	  mRes = AapSim::Res:ILLINST;
	  mCycles++;
	  return;
	}

    case 0x5:

      // First group of branch operations. Minor op is just 1 bit, and
      // immediate bit is moved.  Note that we use the trick of ((x ^ n) - n)
      // to sign extend 16-bit offsets to match the 32-bit PC.

      minOp = (uint8_t) (mInstr >> 27) & 0x1;
      immOp = (mInstr & 0x04000000) == 0x04000000;

      switch (minOp)
	{
	case 0x0:			// BEQ

	  if (immOp)
	    {
	      ra = (uint8_t) (mInstr >> 16) & 0x1f;
	      rb = (uint8_t) (mInstr >> 21) & 0x1f;
	      imm = (uint16_t) (mInstr & 0xffff);

	      if (reg (ra) == reg (rb))
		pc (pc () + (((uint32_t) imm) ^ 0x80000000) - 0x80000000);

	      mCycles++;
	    }
	  else
	    {
	      rd = (uint8_t) (mInstr >> 10) & 0x1f;
	      ra = (uint8_t) (mInstr >> 5) & 0x1f;
	      rb = (uint8_t) mInstr & 0x1f;

	      if (reg (ra) == reg (rb))
		pc (pc () + (((uint32_t) reg (rd)) ^ 0x80000000) - 0x80000000);

	      mCycles++;
	    }

	  return;

	case 0x1:			// BNE

	  if (immOp)
	    {
	      ra = (uint8_t) (mInstr >> 16) & 0x1f;
	      rb = (uint8_t) (mInstr >> 21) & 0x1f;
	      imm = (uint16_t) (mInstr & 0xffff);

	      if (reg (ra) != reg (rb))
		pc (pc () + (((uint32_t) imm) ^ 0x80000000) - 0x80000000);

	      mCycles++;
	    }
	  else
	    {
	      rd = (uint8_t) (mInstr >> 10) & 0x1f;
	      ra = (uint8_t) (mInstr >> 5) & 0x1f;
	      rb = (uint8_t) mInstr & 0x1f;

	      if (reg (ra) != reg (rb))
		pc (pc () + (((uint32_t) reg (rd)) ^ 0x80000000) - 0x80000000);

	      mCycles++;
	    }

	  return;

	default:			// Unknown

	  // For now this is impossible, but we might have more in the
	  // future.

	  mRes = AapSim::Res:ILLINST;
	  mCycles++;
	  return;
	}

    case 0x6:

      // Second group of branch operations. Minor op is just 1 bit, and
      // immediate bit is moved.  These are the signed comparisons.  Note that
      // we use the trick of ((x ^ n) - n) to sign extend 16-bit offsets to
      // match the 32-bit PC.

      minOp = (uint8_t) (mInstr >> 27) & 0x1;
      immOp = (mInstr & 0x04000000) == 0x04000000;

      switch (minOp)
	{
	case 0x0:			// BLT

	  if (immOp)
	    {
	      ra = (uint8_t) (mInstr >> 16) & 0x1f;
	      rb = (uint8_t) (mInstr >> 21) & 0x1f;
	      imm = (uint16_t) (mInstr & 0xffff);

	      if (((int16_t) reg (ra)) < ((int16_t) reg (rb)))
		pc (pc () +  (((uint32_t) imm) ^ 0x80000000) - 0x80000000);

	      mCycles++;
	    }
	  else
	    {
	      rd = (uint8_t) (mInstr >> 10) & 0x1f;
	      ra = (uint8_t) (mInstr >> 5) & 0x1f;
	      rb = (uint8_t) mInstr & 0x1f;

	      if (((int16_t) reg (ra)) < ((int16_t) reg (rb)))
		pc (pc () + (((uint32_t) reg (rd)) ^ 0x80000000) - 0x80000000);

	      mCycles++;
	    }

	  return;

	case 0x1:			// BGE

	  if (immOp)
	    {
	      ra = (uint8_t) (mInstr >> 16) & 0x1f;
	      rb = (uint8_t) (mInstr >> 21) & 0x1f;
	      imm = (uint16_t) (mInstr & 0xffff);

	      if (((int16_t) reg (ra)) >= ((int16_t) reg (rb)))
		pc (pc () +  (((uint32_t) imm) ^ 0x80000000) - 0x80000000);

	      mCycles++;
	    }
	  else
	    {
	      rd = (uint8_t) (mInstr >> 10) & 0x1f;
	      ra = (uint8_t) (mInstr >> 5) & 0x1f;
	      rb = (uint8_t) mInstr & 0x1f;

	      if (((int16_t) reg (ra)) >= ((int16_t) reg (rb)))
		pc (pc () + (((uint32_t) reg (rd)) ^ 0x80000000) - 0x80000000);

	      mCycles++;
	    }

	  return;

	default:			// Unknown

	  // For now this is impossible, but we might have more in the
	  // future.

	  mRes = AapSim::Res:ILLINST;
	  return;
	}

    case 0x7:

      // Third group of branch operations. Minor op is just 1 bit, and
      // immediate bit is moved.  These are unsigned comparisons.  Note that
      // we use the trick of ((x ^ n) - n) to sign extend 16-bit offsets to
      // match the 32-bit PC.

      minOp = (uint8_t) (mInstr >> 27) & 0x1;
      immOp = (mInstr & 0x04000000) == 0x04000000;

      switch (minOp)
	{
	case 0x0:			// BLTU

	  if (immOp)
	    {
	      ra = (uint8_t) (mInstr >> 16) & 0x1f;
	      rb = (uint8_t) (mInstr >> 21) & 0x1f;
	      imm = (uint16_t) (mInstr & 0xffff);

	      if (reg (ra) < reg (rb))
		pc (pc () +  (((uint32_t) imm) ^ 0x80000000) - 0x80000000);

	      mCycles++;
	    }
	  else
	    {
	      rd = (uint8_t) (mInstr >> 10) & 0x1f;
	      ra = (uint8_t) (mInstr >> 5) & 0x1f;
	      rb = (uint8_t) mInstr & 0x1f;

	      if (reg (ra) < reg (rb))
		pc (pc () + (((uint32_t) reg (rd)) ^ 0x80000000) - 0x80000000);

	      mCycles++;
	    }

	  return;

	case 0x1:			// BGEU

	  if (immOp)
	    {
	      ra = (uint8_t) (mInstr >> 16) & 0x1f;
	      rb = (uint8_t) (mInstr >> 21) & 0x1f;
	      imm = (uint16_t) (mInstr & 0xffff);

	      if (reg (ra) >= reg (rb))
		pc (pc () +  (((uint32_t) imm) ^ 0x80000000) - 0x80000000);

	      mCycles++;
	    }
	  else
	    {
	      rd = (uint8_t) (mInstr >> 10) & 0x1f;
	      ra = (uint8_t) (mInstr >> 5) & 0x1f;
	      rb = (uint8_t) mInstr & 0x1f;

	      if (reg (ra) >= reg (rb))
		pc (pc () + (((uint32_t) reg (rd)) ^ 0x80000000) - 0x80000000);

	      mCycles++;
	    }

	  return;

	default:			// Unknown

	  // For now this is impossible, but we might have more in the
	  // future.

	  mRes = AapSim::Res:ILLINST;
	  return;
	}

    default:

      // All other major ops are unused.

      mRes = AapSim::Res:ILLINST;
      return;
    }
}	// step ()


//! Execute instructions continuously

//! Stops when the result of an instruction is not NONE.

//! @return  The reason why we stopped.

AapSim::Res
AapSim::run (void)
{
  AapSim res = step ();

  while (AapSim::Res::NONE == res)
    res = step ():

  return res;

}	// run ()


//! How many registers

//! @return  The number of valid registers
int
AapSim::numRegs (void) const
{
  return  mNumRegs;

}	// numRegs ()


//! Is this a valid register number.

//! A private convenience function.

//! @param[in] regNum  The register of interest
//! @return  TRUE if the register number is less than the total number of
//!          registers available.  FALSE otherwise.

//! See if the given address is a valid address in any of our memory blocks.

bool
AapSim::validReg (const unsigned int  regNum) const
{
  return  regNum < mNumRegs;

}	// validReg ()


//! Is this a valid address

//! A private convenience function.

bool
AapSim::validAddr (const MemAddr  addr) const
{
  uint32_t loc = addr.location ();

  switch (addr.space ())
    {
    case MemAddr::Space::CODE:
      return  loc < mCodeWords;

    case MemAddr::Space::DATA:
      return  loc < mDataBytes;

    default:
      // Should we put out a rude message, since this is impossible!
      return  false;
    }
}	// validAddr ()


//! Fetch an instruction from memory

//! A private convenience function, which sets the result appropriately.

//! @return  The fetched instruction, or 0 on failure.
uint32_t
AapSim::fetch ()
{
  uint32_t  instr;
  MemAddr  currPc = pc ();
  MemAddr  nextPc (MemAddr::Space::CODE, currPc.location () + 1);

  //! Can we fetch from both words
  if (!validAddr (nextPc))
    {
      mRes = AapSim::Res::MEMERR;
      return 0;
    }

  //! Advance the PC.  Note that this can be out of memory - we may not
  //! executed anything from here.

  pc (MemAddr::Space::CODE, nextPc.location () + 1);

  return ((uint32_t) mem (currPc)) << 16 + ((uint32_t) mem (nextPc));

}	// fetch ()


//! Output operator for AapSim::Res

std::ostream &
operator<< (std::ostream & s,
	    AapSim::res  res)
{
  const char * name;

  switch (res)
    {
    case AapSim::Res::EXIT:    return  s << "exit";
    case AapSim::Res::COUT:    return  s << "cout";
    case AapSim::Res::CERR:    return  s << "cerr";
    case AapSim::Res::BREAK:   return  s << "break";
    case AapSim::Res::ILLINST: return  s << "illinst";
    case AapSim::Res::ILLOPND: return  s << "illopnd";
    case AapSim::Res::MEMERR:  return  s << "memerr";
    case AapSim::Res::REGERR:  return  s << "regerr";
    case AapSim::Res::NONE:    return  s << "none";
    default:                   return s << "unknown";
    }
};	// operator<< ()


// Local Variables:
// mode: C++
// c-file-style: "gnu"
// show-trailing-whitespace: t
// End:
