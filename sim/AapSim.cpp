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

#include "config.h"

#include <cstring>
#include <unistd.h>

#include "AapSim.h"


using std::cerr;
using std::endl;


//! Default Constructor.

//! Allocate registers and memory space maximally

AapSim::AapSim (void) :
  AapSim (64,			// 64 regs
	  0x1000000,		// 16Mb code
	  0x10000)		// 64kB data
{
}	// AapSim ()


//! Constructor.

//! Allocate registers and memory space, initializing everything
//! appropriately.

//! @param[in]  _numRegs    Number of registers (4-64);
//! @param[in]  _codeWords  Size of code memory (up to 2^24 words)
//! @param[in]  _dataBytes  Size of data memory (up to 2^16 bytes)

AapSim::AapSim (const unsigned int  _numRegs,
		const unsigned int  _codeWords,
		const unsigned int  _dataBytes) :
  mRegs (NULL),
  mNumRegs (_numRegs),
  mCodeMem (NULL),
  mCodeWords (_codeWords),
  mDataMem (NULL),
  mDataBytes (_dataBytes),
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

//! This public version resets the status before attempting to access the register
//! It is intended for external users reading the registers of the simulator.

//! @param[in] regNum  The register to read
//! @return  The value in the register

uint16_t
AapSim::reg (const unsigned int  regNum)
{
  mRes = AapSim::Res::NONE;
  return  rawReg (regNum);

}	// reg ()


//! Set the value of a general register

//! This public version resets the status before attempting to access the register
//! It is intended for external users writing the registers of the simulator.

//! @param[in] regNum  The register to write
//! @param[in] val     The value to write

void
AapSim::reg (const unsigned int  regNum,
	     uint16_t  val)
{
  mRes = AapSim::Res::NONE;
  rawReg (regNum, val);

}	// reg ()


//! Get the value of the program counter

//! This public version resets the status before attempting to access the PC.
//! It is intended for external users reading the PC of the simulator.

//! @return  The value in the register

MemAddr
AapSim::pc ()
{
  mRes = AapSim::Res::NONE;
  return  rawPc ();

}	// pc ()


//! Set the value of the program counter

//! This sets the full 32 bits, including the status bits.  It is up to the
//! caller to ensure these have the desired value.

//! This public version resets the status before attempting to access the PC.
//! It is intended for external users writing the PC of the simulator.

//! @param[in] addr  The address to write, including address bits

void
AapSim::pc (MemAddr  addr)
{
  mRes = AapSim::Res::NONE;
  rawPc (addr);

}	// pc ()


//! Read from memory

//! If reading from memory, the value returned is a byte, if from code, a
//! word.

//! This public version resets the status before attempting to access memory.
//! It is intended for external users reading the memory of the simulator.

//! @param[in]  addr   The address to read
//! @return  The value read

uint16_t
AapSim::mem (const MemAddr  addr)
{
  mRes = AapSim::Res::NONE;
  return rawMem (addr);

}	// mem ()


//! Write a word to code memory

//! @note This is only for use with code memory

//! This public version resets the status before attempting to access memory.
//! It is intended for external users reading the memory of the simulator.

//! @param[in] addr  The address to write
//! @param[in] val   The word to write

void
AapSim::mem (MemAddr  addr,
	     uint16_t  val)
{
  mRes = AapSim::Res::NONE;
  rawMem (addr, val);

}	// mem () ;


//! Write a byte to data memory

//! @note This is only for use with data memory

//! This public version resets the status before attempting to access memory.
//! It is intended for external users reading the memory of the simulator.

//! @param[in] addr  The address to write
//! @param[in] val   The word to write

void
AapSim::mem (MemAddr  addr,
	     uint8_t  val)
{
  mRes = AapSim::Res::NONE;
  rawMem (addr, val);

}	// mem () ;


//! Execute one instruction

//! @return  The reason why we stopped.

void
AapSim::step (void)
{
  mRes = AapSim::Res::NONE;

  fetch ();

  if (mRes != AapSim::Res::NONE)
    return;				// Fetch failed

  // Which size instruction?
  if (is16bit (mInstr))
    {
      mCycles++;
      step16 ();
    }
  else
    {
      mCycles += 2;
      step32 ();
    }
}	// step ()


//! Execute one 16-bit instruction

void
AapSim::step16 (void)
{
  // All uint16_t, to ensure no type problems later.

  uint16_t  cls  = (mInstr & 0x00006000) >> 13;
  uint16_t  opc  = (mInstr & 0x00001e00) >>  9;
  uint16_t  rd   = (mInstr & 0x000001c0) >>  6;
  uint16_t  ra   = (mInstr & 0x00000038) >>  3;
  uint16_t  rb   =  mInstr & 0x00000007;
  uint16_t  imm3 = rb;
  uint16_t  imm6 = (ra << 3) | rb;
  uint16_t  off3 = signExt (imm3, 3);

  // Branch offsets 32-bit, since they work on the PC

  uint32_t  broff3 = signExt ((uint32_t) rd, 3);
  uint32_t  broff6 = signExt ((uint32_t) ((rd << 3) | rb), 6);
  uint32_t  broff9 = signExt ((uint32_t) ((rd << 6) | (ra << 3) | rb), 9);

  switch (cls)
    {
    case 0x0:

      // The main ALU operations.  Ensure we use accessor functions for regs
      // and memory, since this will sort out error codes for us.

      switch (opc)
	{
	case 0x0:			// NOP

	  switch (imm6)
	    {
	    case 0:  mRes = AapSim::Res::BREAK;   break;
	    case 1:  mRes = AapSim::Res::NONE;    break;
	    case 2:  mRes = AapSim::Res::EXIT;    break;
	    case 3:  mRes = AapSim::Res::COUT;    break;
	    case 4:  mRes = AapSim::Res::CERR;    break;
	    default: mRes = AapSim::Res::ILLOPND; break;
	    }

	  return;

	case 0x1:			// Unsigned ADD

	  carryBit (((uint32_t) rawReg (ra) + (uint32_t) rawReg (rb)) > 0xffff ? 1 : 0);
	  rawReg (rd, rawReg (ra) + rawReg (rb));
	  return;

	case 0x2:			// Unsigned SUB

	  carryBit (rawReg (rb) > rawReg (ra) ? 1 : 0);
	  rawReg (rd, rawReg (ra) - rawReg (rb));
	  return;

	case 0x3:			// AND

	  rawReg (rd, rawReg (ra) & rawReg (rb));
	  return;

	case 0x4:			// OR

	  rawReg (rd, rawReg (ra) | rawReg (rb));
	  return;

	case 0x5:			// XOR

	  rawReg (rd, rawReg (ra) ^ rawReg (rb));
	  return;

	case 0x6:			// ASR

	  // Note that right shift of negative numbers is implementation
	  // dependent in C/C++.  However our values are explicitly unsigned,
	  // so we can guarantee we will by default get LSR.  We sign extend
	  // after the shift to make the arithmetic result.

	  if (rawReg (ra) > 16)
	    mRes = AapSim::Res::ILLOPND;
	  else
	    rawReg (rd, signExt ((uint16_t) (rawReg (ra) >> rawReg (rb)),
				 16 - rawReg (rb)));

	  return;

	case 0x7:			// LSL

	  if (rawReg (ra) > 16)
	    mRes = AapSim::Res::ILLOPND;
	  else
	    rawReg (rd, rawReg (ra) << rawReg (rb));

	  return;

	case 0x8:			// LSR

	  // Since registers are unsigned, this is guaranteed to be logical
	  // shift.

	  if (rawReg (ra) > 16)
	    mRes = AapSim::Res::ILLOPND;
	  else
	    rawReg (rd, rawReg (ra) >> rawReg (rb));

	  return;

	case 0x9:			// MOV

	  rawReg (rd, rawReg (ra));
	  return;

	case 0xa:			// Unsigned ADD immediate

	  carryBit (((uint32_t) rawReg (ra) + (uint32_t) imm3) > 0xffff ? 1 : 0);
	  rawReg (rd, rawReg (ra) + imm3);
	  return;

	case 0xb:			// Unsigned SUB immediate

	  carryBit (imm3 > rawReg (ra) ? 1 : 0);
	  rawReg (rd, rawReg (ra) - imm3);
	  return;

	case 0xc:			// ASR immediate

	  // Note that right shift of negative numbers is implementation
	  // dependent in C/C++.  However our values are explicitly unsigned,
	  // so we can guarantee we will by default get LSR.  We sign extend
	  // after the shift to make the arithmetic result.

	  rawReg (rd, signExt ((uint16_t) (rawReg (ra) >> imm3), 16 - imm3));
	  return;

	case 0xd:			// LSL immediate

	  rawReg (rd, rawReg (ra) << imm3);
	  return;

	case 0xe:			// LSR immediate

	  // Since registers are unsigned, this is guaranteed to be logical
	  // shift.

	  rawReg (rd, rawReg (ra) >> imm3);
	  return;

	case 0xf:			// MOV immediate

	  rawReg (rd, imm6);
	  return;

	default:			// Unknown

	  // This should of course be impossible...  But we leave it here in
	  // case in the future we don't have a full set of possibilities.

	  mRes = AapSim::Res::ILLINST;
	  return;
	}

    case 0x1:

      // Load store operations.

      switch (opc)
	{
	case 0x0:			// LDB

	  rawReg (rd, rawMem (MemAddr (MemAddr::Space::DATA, rawReg (ra) + off3)));
	  return;

	case 0x4:			// LDW

	  rawReg (rd, readDataWord (rawReg (ra) + off3));
	  return;

	case 0x1:			// LDB with postincrement

	  rawReg (rd, rawMem (MemAddr (MemAddr::Space::DATA, rawReg (ra) + off3)));
	  rawReg (ra, rawReg (ra) + 1);
	  return;

	case 0x5:			// LDW with post increment

	  rawReg (rd, readDataWord (rawReg (ra) + off3));
	  rawReg (ra, rawReg (ra) + 2);
	  return;

	case 0x2:			// LDB with predecrement

	  rawReg (ra, rawReg (ra) - 1);
	  rawReg (rd, rawMem (MemAddr (MemAddr::Space::DATA, rawReg (ra) + off3)));
	  return;

	case 0x6:			// LDW with predecrement

	  rawReg (ra, rawReg (ra) - 2);
	  rawReg (rd, readDataWord (rawReg (ra) + off3));
	  return;

	case 0x8:			// STB

	  rawMem (MemAddr (MemAddr::Space::DATA, rawReg (ra) + off3), rawReg (rd));
	  return;

	case 0xc:			// STW

	  writeDataWord (rawReg (ra) + off3, rawReg (rd));
	  return;

	case 0x9:			// STB with postincrement

	  rawMem (MemAddr (MemAddr::Space::DATA, rawReg (ra) + off3), rawReg (rd));
	  rawReg (ra, rawReg (ra) + 1);
	  return;

	case 0xd:			// STW with post increment

	  writeDataWord (rawReg (ra) + off3, rawReg (rd));
	  rawReg (ra, rawReg (ra) + 2);
	  return;

	case 0xa:			// STB with predecrement

	  rawReg (ra, rawReg (ra) - 1);
	  rawMem (MemAddr (MemAddr::Space::DATA, rawReg (ra) + off3), rawReg (rd));
	  return;

	case 0xe:			// STW with predecrement

	  rawReg (ra, rawReg (ra) - 2);
	  writeDataWord (rawReg (ra) + off3, rawReg (rd));
	  return;

	default:			// Unknown

	  // This can happen, since we don't use all the load opcode values (yet).

	  mRes = AapSim::Res::ILLINST;
	  return;
	}

    case 0x2:

      // Flow of control operations

      switch (opc)
	{
	case 0x0:			// BRA

	  jump (pc ().location () + broff9);
	  return;

	case 0x1:			// BAL

	  rawReg (ra, pc ().location ());
	  jump (pc ().location () + broff6);
	  return;

	case 0x2:			// BEQ

	  if (rawReg (ra) == rawReg (rb))
	    jump (pc ().location () + broff3);

	  return;

	case 0x3:			// BNE

	  if (rawReg (ra) != rawReg (rb))
	    jump (pc ().location () + broff3);

	  return;

	case 0x4:			// BLTS

	  if (((int16_t) rawReg (ra)) < ((int16_t) rawReg (rb)))
	    jump (pc ().location () + broff3);

	  return;

	case 0x5:			// BGTS

	  if (((int16_t) rawReg (ra)) >((int16_t) rawReg (rb)))
	    jump (pc ().location () + broff3);

	  return;

	case 0x6:			// BLTU

	  if (rawReg (ra) < rawReg (rb))
	    jump (pc ().location () + broff3);

	  return;

	case 0x7:			// BGTU

	  if (rawReg (ra) > rawReg (rb))
	    jump (pc ().location () + broff3);

	  return;

	case 0x8:			// JMP

	  jump (rawReg (rd));
	  return;

	case 0x9:			// JAL

	  rawReg (ra, pc ().location ());
	  jump (rawReg (rd));
	  return;

	case 0xa:			// JEQ

	  if (rawReg (ra) == rawReg (rb))
	    jump (rawReg (rd));

	  return;

	case 0xb:			// JNE

	  if (rawReg (ra) != rawReg (rb))
	    jump (rawReg (rd));

	  return;

	case 0xc:			// JLTS

	  if (((int16_t) rawReg (ra)) < ((int16_t) rawReg (rb)))
	    jump (rawReg (rd));

	  return;

	case 0xd:			// JGTS

	  if (((int16_t) rawReg (ra)) >((int16_t) rawReg (rb)))
	    jump (rawReg (rd));

	  return;

	case 0xe:			// JLTU

	  if (rawReg (ra) < rawReg (rb))
	    jump (rawReg (rd));

	  return;

	case 0xf:			// JGTU

	  if (rawReg (ra) > rawReg (rb))
	    jump (rawReg (rd));

	  return;

	default:			// Unknown

	  // This should of course be impossible...  But we leave it here in
	  // case in the future we don't have a full set of possibilities.

	  mRes = AapSim::Res::ILLINST;
	  return;
	}

    case 0x3:

      // Miscellaneous operations

      switch (opc)
	{
	case 0x0:			// RTE

	  jump (rawReg (rd), true);
	  return;

	default:

	  // This can happen, since we don't use all the misc opcode values (yet).

	  mRes = AapSim::Res::ILLINST;
	  return;
	}

    default:

      // For now this is impossible, but we might have more in the future.

      mRes = AapSim::Res::ILLINST;
      return;
    }
}	// step16 ()


//! Execute one 32-bit instruction

//! Remember the first word fetched is in the lower half of the word, and
//! these form the least significant bits of any fields.

void
AapSim::step32 (void)
{
  // All uint16_t, to ensure no type problems later.

  uint16_t  cls  = ((mInstr & 0x60000000) >> 29) | ((mInstr & 0x00006000) >> 13);
  uint16_t  opc  = ((mInstr & 0x1e000000) >> 25) | ((mInstr & 0x00001e00) >>  9);
  uint16_t  rd   = ((mInstr & 0x01c00000) >> 22) | ((mInstr & 0x000001c0) >>  6);
  uint16_t  ra   = ((mInstr & 0x00380000) >> 19) | ((mInstr & 0x00000038) >>  3);
  uint16_t  rb   = ((mInstr & 0x00070000) >> 16) |  (mInstr & 0x00000007);
  uint16_t  imm6  = rb;
  uint16_t  imm9  = ((mInstr & 0x1c000000) >> 20) | rb;
  uint16_t  imm10 = ((mInstr & 0x1e000000) >> 19) | rb;
  uint16_t  imm12 = (ra << 6) | rb;
  uint16_t  imm16 = ((mInstr & 0x1e000000) >> 13) | imm12;
  uint16_t  off6  = signExt (imm6, 6);

  // Branch offsets 32-bit, since they work on the PC

  uint32_t  broff10 = signExt (((mInstr & 0x1e000000) >> 19) | (uint32_t) rd, 6);
  uint32_t  broff16 = signExt (((mInstr & 0x1e000000) >> 13)
			       | ((uint32_t) rd << 6) | (uint32_t) rb, 16);
  uint32_t  broff22 = signExt (((mInstr & 0x1e000000) >> 7)
			       | ((uint32_t) rd << 12) | ((uint32_t) ra << 6)
			       | (uint32_t) rb, 22);

  // Sanity check - no 48-bit or longer instructions
  if ((mInstr & 0x80000000) == 0x80000000)
    {
      mRes = AapSim::Res::MEMERR;
      return;
    }

  switch (cls)
    {
    case 0x0:

      // The main ALU operations.  Ensure we use accessor functions for regs
      // and memory, since this will sort out error codes for us.

      switch (opc)
	{
	case 0x00:			// NOP

	  switch (imm12)
	    {
	    case 0:  mRes = AapSim::Res::NONE;    break;
	    case 1:  mRes = AapSim::Res::COUT;    break;
	    case 2:  mRes = AapSim::Res::CERR;    break;
	    case 3:  mRes = AapSim::Res::BREAK;   break;
	    case 4:  mRes = AapSim::Res::EXIT;    break;
	    default: mRes = AapSim::Res::ILLOPND; break;
	    }

	  return;

	case 0x01:			// Unsigned ADD

	  carryBit (((uint32_t) rawReg (ra) + (uint32_t) rawReg (rb)) > 0xffff ? 1 : 0);
	  rawReg (rd, rawReg (ra) + rawReg (rb));
	  return;

	case 0x02:			// Unsigned SUB

	  carryBit (rawReg (rb) > rawReg (ra) ? 1 : 0);
	  rawReg (rd, rawReg (ra) - rawReg (rb));
	  return;

	case 0x03:			// AND

	  rawReg (rd, rawReg (ra) & rawReg (rb));
	  return;

	case 0x04:			// OR

	  rawReg (rd, rawReg (ra) | rawReg (rb));
	  return;

	case 0x05:			// XOR

	  rawReg (rd, rawReg (ra) ^ rawReg (rb));
	  return;

	case 0x06:			// ASR

	  // Note that right shift of negative numbers is implementation
	  // dependent in C/C++.  However our values are explicitly unsigned,
	  // so we can guarantee we will by default get LSR.  We sign extend
	  // after the shift to make the arithmetic result.

	  if (rawReg (ra) > 16)
	    mRes = AapSim::Res::ILLOPND;
	  else
	    rawReg (rd, signExt ((uint16_t) (rawReg (ra) >> rawReg (rb)),
				 16 - rawReg (rb)));

	  return;

	case 0x07:			// LSL

	  if (rawReg (ra) > 16)
	    mRes = AapSim::Res::ILLOPND;
	  else
	    rawReg (rd, rawReg (ra) << rawReg (rb));

	  return;

	case 0x08:			// LSR

	  // Since registers are unsigned, this is guaranteed to be logical
	  // shift.

	  if (rawReg (ra) > 16)
	    mRes = AapSim::Res::ILLOPND;
	  else
	    rawReg (rd, rawReg (ra) >> rawReg (rb));

	  return;

	case 0x09:			// MOV

	  rawReg (rd, rawReg (ra));
	  return;

	case 0x0a:			// ADD immediate
	case 0x1a:
	case 0x2a:
	case 0x3a:
	case 0x4a:
	case 0x5a:
	case 0x6a:
	case 0x7a:
	case 0x8a:
	case 0x9a:
	case 0xaa:
	case 0xba:
	case 0xca:
	case 0xda:
	case 0xea:
	case 0xfa:

	  carryBit (((uint32_t) rawReg (ra) + (uint32_t) imm10) > 0xffff ? 1 : 0);
	  rawReg (rd, rawReg (ra) + imm10);
	  return;

	case 0x0b:			// SUB immediate
	case 0x1b:
	case 0x2b:
	case 0x3b:
	case 0x4b:
	case 0x5b:
	case 0x6b:
	case 0x7b:
	case 0x8b:
	case 0x9b:
	case 0xab:
	case 0xbb:
	case 0xcb:
	case 0xdb:
	case 0xeb:
	case 0xfb:

	  carryBit (imm10 > rawReg (ra) ? 1 : 0);
	  rawReg (rd, rawReg (ra) - imm10);
	  return;

	case 0x0c:			// ASR immediate

	  // Note that right shift of negative numbers is implementation
	  // dependent in C/C++.  However our values are explicitly unsigned,
	  // so we can guarantee we will by default get LSR.  We sign extend
	  // after the shift to make the arithmetic result.

	  if (imm6 > 16)
	    mRes = AapSim::Res::ILLINST;
	  else
	    rawReg (rd, signExt ((uint16_t) (rawReg (ra) >> imm6), 16 - imm6));

	  return;

	case 0x0d:			// LSL immediate

	  if (imm6 > 16)
	    mRes = AapSim::Res::ILLINST;
	  else
	    rawReg (rd, rawReg (ra) << imm6);

	  return;

	case 0x0e:			// LSR immediate

	  // Since registers are unsigned, this is guaranteed to be logical
	  // shift.

	  if (imm6 > 16)
	    mRes = AapSim::Res::ILLINST;
	  else
	    rawReg (rd, rawReg (ra) >> imm6);

	  return;

	case 0x0f:			// MOV immediate

	  rawReg (rd, imm16);
	  return;

	case 0x11:			// Unsigned ADD with carry

	  carryBit (((uint32_t) rawReg (ra) + (uint32_t) rawReg (rb) + carryBit ())
		    > 0xffff ? 1 : 0);
	  rawReg (rd, rawReg (ra) + rawReg (rb) + carryBit ());
	  return;

	case 0x12:			// Unsigned SUB with carry

	  carryBit ((rawReg (rb) + carryBit ()) > rawReg (ra) ? 1 : 0);
	  rawReg (rd, rawReg (ra) - rawReg (rb) - carryBit ());
	  return;

	case 0x13:			// AND immediate
	case 0x33:
	case 0x53:
	case 0x73:
	case 0x93:
	case 0xb3:
	case 0xd3:
	case 0xf3:

	  rawReg (rd, rawReg (ra) & imm9);
	  return;

	case 0x14:			// OR immediate
	case 0x34:
	case 0x54:
	case 0x74:
	case 0x94:
	case 0xb4:
	case 0xd4:
	case 0xf4:

	  rawReg (rd, rawReg (ra) | imm9);
	  return;

	case 0x15:			// XOR immediate
	case 0x35:
	case 0x55:
	case 0x75:
	case 0x95:
	case 0xb5:
	case 0xd5:
	case 0xf5:

	  rawReg (rd, rawReg (ra) ^ imm9);
	  return;

	default:			// Unknown

	  // This can happen, since we don't use all the 32-bit ALU opcode
	  // values (yet).

	  mRes = AapSim::Res::ILLINST;
	  return;
	}

    case 0x1:

      // Load store operations.

      switch (opc)
	{
	case 0x00:			// LDB

	  rawReg (rd, rawMem (MemAddr (MemAddr::Space::DATA, rawReg (ra) + off6)));
	  return;

	case 0x04:			// LDW

	  rawReg (rd, readDataWord (rawReg (ra) + off6));
	  return;

	case 0x01:			// LDB with postincrement

	  rawReg (rd, rawMem (MemAddr (MemAddr::Space::DATA, rawReg (ra) + off6)));
	  rawReg (ra, rawReg (ra) + 1);
	  return;

	case 0x05:			// LDW with post increment

	  rawReg (rd, readDataWord (rawReg (ra) + off6));
	  rawReg (ra, rawReg (ra) + 2);
	  return;

	case 0x02:			// LDB with predecrement

	  rawReg (ra, rawReg (ra) - 1);
	  rawReg (rd, rawMem (MemAddr (MemAddr::Space::DATA, rawReg (ra) + off6)));
	  return;

	case 0x06:			// LDW with predecrement

	  rawReg (ra, rawReg (ra) - 2);
	  rawReg (rd, readDataWord (rawReg (ra) + off6));
	  return;

	case 0x08:			// STB

	  rawMem (MemAddr (MemAddr::Space::DATA, rawReg (ra) + off6), rawReg (rd));
	  return;

	case 0x0c:			// STW

	  writeDataWord (rawReg (ra) + off6, rawReg (rd));
	  return;

	case 0x09:			// STB with postincrement

	  rawMem (MemAddr (MemAddr::Space::DATA, rawReg (ra) + off6), rawReg (rd));
	  rawReg (ra, rawReg (ra) + 1);
	  return;

	case 0x0d:			// STW with post increment

	  writeDataWord (rawReg (ra) + off6, rawReg (rd));
	  rawReg (ra, rawReg (ra) + 2);
	  return;

	case 0x0a:			// STB with predecrement

	  rawReg (ra, rawReg (ra) - 1);
	  rawMem (MemAddr (MemAddr::Space::DATA, rawReg (ra) + off6), rawReg (rd));
	  return;

	case 0x0e:			// STW with predecrement

	  rawReg (ra, rawReg (ra) - 2);
	  writeDataWord (rawReg (ra) + off6, rawReg (rd));
	  return;

	case 0x10:			// LDD

	  rawReg (rd,     readDataWord (rawReg (ra) + off6));
	  rawReg (rd + 1, readDataWord (rawReg (ra) + off6 + 2));
	  return;

	case 0x11:			// LDD with post increment

	  rawReg (rd    , readDataWord (rawReg (ra) + off6));
	  rawReg (rd + 1, readDataWord (rawReg (ra) + off6 + 2));
	  rawReg (ra, rawReg (ra) + 4);
	  return;

	case 0x12:			// LDD with predecrement

	  rawReg (ra, rawReg (ra) - 4);
	  rawReg (rd,     readDataWord (rawReg (ra) + off6));
	  rawReg (rd + 1, readDataWord (rawReg (ra) + off6 + 2));
	  return;

	case 0x18:			// STD

	  writeDataWord (rawReg (ra) + off6,     rawReg (rd));
	  writeDataWord (rawReg (ra) + off6 + 2, rawReg (rd + 1));
	  return;

	case 0x19:			// STD with post increment

	  writeDataWord (rawReg (ra) + off6,     rawReg (rd));
	  writeDataWord (rawReg (ra) + off6 + 2, rawReg (rd + 1));
	  rawReg (ra, rawReg (ra) + 4);
	  return;

	case 0x1a:			// STD with predecrement

	  rawReg (ra, rawReg (ra) - 4);
	  writeDataWord (rawReg (ra) + off6,     rawReg (rd));
	  writeDataWord (rawReg (ra) + off6 + 2, rawReg (rd + 1));
	  return;

	default:			// Unknown

	  // This can happen, since we don't use all the load/store opcode
	  // values (yet).

	  mRes = AapSim::Res::ILLINST;
	  return;
	}

    case 0x2:

      // Flow of control operations

      switch (opc)
	{
	case 0x00:			// BRA
	case 0x10:
	case 0x20:
	case 0x30:
	case 0x40:
	case 0x50:
	case 0x60:
	case 0x70:
	case 0x80:
	case 0x90:
	case 0xa0:
	case 0xb0:
	case 0xc0:
	case 0xd0:
	case 0xe0:
	case 0xf0:

	  jump (pc ().location () + broff22);
	  return;

	case 0x01:			// BAL
	case 0x11:
	case 0x21:
	case 0x31:
	case 0x41:
	case 0x51:
	case 0x61:
	case 0x71:
	case 0x81:
	case 0x91:
	case 0xa1:
	case 0xb1:
	case 0xc1:
	case 0xd1:
	case 0xe1:
	case 0xf1:

	  rawReg (ra, pc ().location ());
	  jump (pc ().location () + broff16);
	  return;

	case 0x02:			// BEQ
	case 0x12:
	case 0x22:
	case 0x32:
	case 0x42:
	case 0x52:
	case 0x62:
	case 0x72:
	case 0x82:
	case 0x92:
	case 0xa2:
	case 0xb2:
	case 0xc2:
	case 0xd2:
	case 0xe2:
	case 0xf2:

	  if (rawReg (ra) == rawReg (rb))
	    jump (pc ().location () + broff10);

	  return;

	case 0x03:			// BNE
	case 0x13:
	case 0x23:
	case 0x33:
	case 0x43:
	case 0x53:
	case 0x63:
	case 0x73:
	case 0x83:
	case 0x93:
	case 0xa3:
	case 0xb3:
	case 0xc3:
	case 0xd3:
	case 0xe3:
	case 0xf3:

	  if (rawReg (ra) != rawReg (rb))
	    jump (pc ().location () + broff10);

	  return;

	case 0x04:			// BLTS
	case 0x14:
	case 0x24:
	case 0x34:
	case 0x44:
	case 0x54:
	case 0x64:
	case 0x74:
	case 0x84:
	case 0x94:
	case 0xa4:
	case 0xb4:
	case 0xc4:
	case 0xd4:
	case 0xe4:
	case 0xf4:

	  if (((int16_t) rawReg (ra)) < ((int16_t) rawReg (rb)))
	    jump (pc ().location () + broff10);

	  return;

	case 0x05:			// BGTS
	case 0x15:
	case 0x25:
	case 0x35:
	case 0x45:
	case 0x55:
	case 0x65:
	case 0x75:
	case 0x85:
	case 0x95:
	case 0xa5:
	case 0xb5:
	case 0xc5:
	case 0xd5:
	case 0xe5:
	case 0xf5:

	  if (((int16_t) rawReg (ra)) >((int16_t) rawReg (rb)))
	    jump (pc ().location () + broff10);

	  return;

	case 0x06:			// BLTU
	case 0x16:
	case 0x26:
	case 0x36:
	case 0x46:
	case 0x56:
	case 0x66:
	case 0x76:
	case 0x86:
	case 0x96:
	case 0xa6:
	case 0xb6:
	case 0xc6:
	case 0xd6:
	case 0xe6:
	case 0xf6:

	  if (rawReg (ra) < rawReg (rb))
	    jump (pc ().location () + broff10);

	  return;

	case 0x07:			// BGTU
	case 0x17:
	case 0x27:
	case 0x37:
	case 0x47:
	case 0x57:
	case 0x67:
	case 0x77:
	case 0x87:
	case 0x97:
	case 0xa7:
	case 0xb7:
	case 0xc7:
	case 0xd7:
	case 0xe7:
	case 0xf7:

	  if (rawReg (ra) > rawReg (rb))
	    jump (pc ().location () + broff10);

	  return;

	case 0x08:			// JMP

	  jump (rawReg (rd));
	  return;

	case 0x09:			// JAL

	  rawReg (ra, pc ().location ());
	  jump (rawReg (rd));
	  return;

	case 0x0a:			// JEQ

	  if (rawReg (ra) == rawReg (rb))
	    jump (rawReg (rd));

	  return;

	case 0x0b:			// JNE

	  if (rawReg (ra) != rawReg (rb))
	    jump (rawReg (rd));

	  return;

	case 0x0c:			// JLTS

	  if (((int16_t) rawReg (ra)) < ((int16_t) rawReg (rb)))
	    jump (rawReg (rd));

	  return;

	case 0x0d:			// JGTS

	  if (((int16_t) rawReg (ra)) >((int16_t) rawReg (rb)))
	    jump (rawReg (rd));

	  return;

	case 0x0e:			// JLTU

	  if (rawReg (ra) < rawReg (rb))
	    jump (rawReg (rd));

	  return;

	case 0x0f:			// JGTU

	  if (rawReg (ra) > rawReg (rb))
	    jump (rawReg (rd));

	  return;

	case 0x18:			// JMPL

	  jump ((((uint32_t) rawReg (rd + 1)) << 16) | (uint32_t) rawReg (rd));
	  return;

	case 0x19:			// JALL

	  rawReg (ra,     pc ().location ());
	  rawReg (ra + 1, pc ().location () >> 16);
	  jump ((((uint32_t) rawReg (rd + 1)) << 16) | (uint32_t) rawReg (rd));
	  return;

	case 0x1a:			// JEQL

	  if (rawReg (ra) == rawReg (rb))
	    jump ((((uint32_t) rawReg (rd + 1)) << 16) | (uint32_t) rawReg (rd));

	  return;

	case 0x1b:			// JNEL

	  if (rawReg (ra) != rawReg (rb))
	    jump ((((uint32_t) rawReg (rd + 1)) << 16) | (uint32_t) rawReg (rd));

	  return;

	case 0x1c:			// JLTSL

	  if (((int16_t) rawReg (ra)) < ((int16_t) rawReg (rb)))
	    jump ((((uint32_t) rawReg (rd + 1)) << 16) | (uint32_t) rawReg (rd));

	  return;

	case 0x1d:			// JGTSL

	  if (((int16_t) rawReg (ra)) >((int16_t) rawReg (rb)))
	    jump ((((uint32_t) rawReg (rd + 1)) << 16) | (uint32_t) rawReg (rd));

	  return;

	case 0x1e:			// JLTUL

	  if (rawReg (ra) < rawReg (rb))
	    jump ((((uint32_t) rawReg (rd + 1)) << 16) | (uint32_t) rawReg (rd));

	  return;

	case 0x1f:			// JGTUL

	  if (rawReg (ra) > rawReg (rb))
	    jump ((((uint32_t) rawReg (rd + 1)) << 16) | (uint32_t) rawReg (rd));

	  return;

	default:			// Unknown

	  // This can happen, because we don't use all the 32-bit
	  // flow-of-control operations.

	  mRes = AapSim::Res::ILLINST;
	  return;
	}

    case 0x3:

      // Miscellaneous operations

      switch (opc)
	{
	default:

	  // This will always happen because we don't use any 32-bit misc
	  // operations.
	  mRes = AapSim::Res::ILLINST;
	  return;
	}

    default:

      // This can happen because we don't use all the instruction classes.
      mRes = AapSim::Res::ILLINST;
      return;
    }
}	// step32 ()


//! Execute instructions continuously

//! Stops when the result of an instruction is not NONE.

//! @return  The reason why we stopped.

void
AapSim::run (void)
{
  step ();

  while (AapSim::Res::NONE == mRes)
    step ();

  return;

}	// run ()


//! Raw read of the value of a general register

//! Unlike the public version, this function does not reset the status.  If
//! it already has a bad value the function just returns.  This is the
//! behavior we want for internal use.

//! @param[in] regNum  The register to read
//! @return  The value in the register

uint16_t
AapSim::rawReg (const unsigned int  regNum)
{
  if (mRes != AapSim::Res::NONE)
    return 0;

  if (validReg (regNum))
    return  mRegs[regNum];
  else
    {
      mRes = AapSim::Res::REGERR;
      return 0;
    }
}	// rawReg ()


//! Raw write of the value of a general register

//! Unlike the public version, this function does not reset the status.  If
//! it already has a bad value the function just returns.  This is the
//! behavior we want for internal use.

//! @param[in] regNum  The register to write
//! @param[in] val     The value to write

void
AapSim::rawReg (const unsigned int  regNum,
		uint16_t  val)
{
  if (mRes != AapSim::Res::NONE)
    return;

  if (validReg (regNum))
    mRegs [regNum] = val;
  else
    mRes = AapSim::Res::REGERR;

}	// rawReg ()


//! Raw read of the value of the program counter

//! Unlike the public version, this function does not reset the status.  If
//! it already has a bad value the function just returns.  This is the
//! behavior we want for internal use.

//! This is a bit different from the other routines in that the PC can always
//! be read, so we return its value.  We just don't reset the status.

//! @return  The value in the register

MemAddr
AapSim::rawPc () const
{
  return  mPc;

}	// rawPc ()


//! Raw write of the value of the program counter

//! This sets the full 32 bits, including the status bits.  It is up to the
//! caller to ensure these have the desired value.

//! Unlike the public version, this function does not reset the status.  If
//! it already has a bad value the function just returns.  This is the
//! behavior we want for internal use.

//! @param[in] addr  The address to write, including address bits

void
AapSim::rawPc (const MemAddr  addr)
{
  if (mRes != AapSim::Res::NONE)
    return;

  uint32_t loc = addr.location ();

  if (MemAddr::Space::CODE != addr.space ())
    {
      // This is a real error, not something to silently handle with a res.
      cerr << "ERROR: Attempt to write data address " << loc
	   << " to PC: Ignored." << endl;
      return;
    }

  if (validAddr (addr))
    mPc.location (loc);
  else
    mRes = AapSim::Res::MEMERR;

}	// rawPc ()


//! Raw read from memory

//! If reading from data  memory, the value returned is a byte, if from code, a
//! word.

//! Unlike the public version, this function does not reset the status.  If
//! it already has a bad value the function just returns.  This is the
//! behavior we want for internal use.

//! @param[in]  addr   The address to read
//! @return  The value read

uint16_t
AapSim::rawMem (const MemAddr  addr)
{
  if (mRes != AapSim::Res::NONE)
    return 0;

  uint32_t  loc = addr.location ();

  switch (addr.space ())
    {
    case MemAddr::Space::CODE:

      if (!validAddr (addr))
	{
	  mRes = AapSim::Res::MEMERR;
	  return  0;
	}

      return mCodeMem[loc];

    case MemAddr::Space::DATA:

      if (!validAddr (addr))
	{
	  mRes = AapSim::Res::MEMERR;
	  return  0;
	}

      return mDataMem[loc];

    default:

      cerr << "*** ABORT ***:  Unknown code space " << addr.space ()
	   << " when reading memory. Exiting.";
      exit (EXIT_FAILURE);
    }
}	// rawMem ()


//! Write a word to code memory

//! @note This is only for use with code memory

//! Unlike the public version, this function does not reset the status.  If
//! it already has a bad value the function just returns.  This is the
//! behavior we want for internal use.

//! @param[in] addr  The address to write
//! @param[in] val   The word to write

void
AapSim::rawMem (MemAddr  addr,
	     uint16_t  val)
{
  if (mRes != AapSim::Res::NONE)
    return;

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

  mCodeMem[loc] = val;

}	// rawMem () ;


//! Write a byte to data memory

//! @note This is only for use with data memory

//! Unlike the public version, this function does not reset the status.  If
//! it already has a bad value the function just returns.  This is the
//! behavior we want for internal use.

//! @param[in] addr  The address to write
//! @param[in] val   The word to write

void
AapSim::rawMem (MemAddr  addr,
	     uint8_t  val)
{
  if (mRes != AapSim::Res::NONE)
    return;

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

  mDataMem[loc] = val;

}	// rawMem () ;


//! How many registers

//! @return  The number of valid registers
unsigned int
AapSim::numRegs (void) const
{
  return  mNumRegs;

}	// numRegs ()


//! Get the last instruction

//! @return  The last instruction executed.
uint32_t
AapSim::instr (void) const
{
  return  mInstr;

}	// instr ()


//! Get the result of the last action

//! @return  The last result of the last action
AapSim::Res
AapSim::res (void) const
{
  return  mRes;

}	// res ()


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
      return  (loc &0xffffff) < mCodeWords;	// Ignore status bits

    case MemAddr::Space::DATA:
      return  loc < mDataBytes;

    default:
      // Should we put out a rude message, since this is impossible!
      return  false;
    }
}	// validAddr ()


//! Sign-extend to 16 bits

//! Take a number of "bit" bits and sign extend it to 16 bits.

//! We use the trick from the Aggregate Magic Algorithms
//! (http://aggregate.org/MAGIC/).  If "n" is the value of the top bit
//! position, then we can extend by doing:

//!     (x ^ n) - n;

//! @param[in] val  The value to sign extend
//! @param[in] bit  The number of bits in the value to be extended

//! @return  The sign extended value.

uint16_t
AapSim::signExt (uint16_t      val,
		 unsigned int  bit)
{
  int16_t  maxbit = 1 << (bit - 1);
  return  (uint16_t) ((int16_t) val ^ maxbit) - maxbit;

}	// signExt ()


//! Read a word from data memory

//! A convenience wrapper when reading two bytes from memory

//! @param[in] loc  A location in data memory

uint16_t
AapSim::readDataWord (uint16_t loc)
{
  if (mRes != AapSim::Res::NONE)
    return 0;

  uint16_t  lo = rawMem (MemAddr (MemAddr::Space::DATA, loc));

  if (mRes != AapSim::Res::NONE)
    return 0;

  uint16_t  hi = rawMem (MemAddr (MemAddr::Space::DATA, loc));

  if (mRes != AapSim::Res::NONE)
    return 0;

  return (hi << 8) | (lo & 0xff);

}	// readDataWord ()


//! Write a word two data memory

//! A convenience wrapper when writing two bytes to memory

//! @param[in] loc  A location in data memory

void
AapSim::writeDataWord (uint16_t loc,
		       uint16_t val)
{
  if (mRes != AapSim::Res::NONE)
    return;

  rawMem (MemAddr (MemAddr::Space::DATA, loc), (uint8_t) (val & 0xff));

  if (mRes != AapSim::Res::NONE)
    return;

  rawMem (MemAddr (MemAddr::Space::DATA, loc), (uint8_t) (val >> 8));
  return;

}	// writeDataWord ()


//! Jump to a new location

//! A convenience wrapper to only jump if we have a good address

//! @param[in] loc        Location to jump to.
//! @param[in] setStatus  Set TRUE to also set top 8 status bits.  Defaults to
//! FALSE.

void
AapSim::jump (uint32_t  loc,
	      bool      setStatus)
{
  if (mRes != AapSim::Res::NONE)
    return;

  MemAddr dest (MemAddr::Space::CODE, loc);

  if (!validAddr (dest))
    {
      mRes = AapSim::Res::MEMERR;
      return;
    }

  if (!setStatus)
    dest.location ((loc & 0x00ffffff) | (pc ().location () & 0xff000000));

  pc (dest);
  return;

}	// jump ()


//! Return the carry bit

//! This is the top bit of the PC

//! @return  1 If the carry bit is set, 0 othterwise

uint16_t
AapSim::carryBit (void) const
{
  return  mPc.location () >> 31;

}	// carryBit ()


//! Set the carry bit

//! This is the top bit of the PC

//! @param[i] c  The value of the carry bit

void
AapSim::carryBit (uint16_t  c)
{
  uint32_t loc = mPc.location ();

  loc &= 0x7fffffff;
  loc |= ((uint32_t) c) << 31;

  mPc.location (loc);

}	// carryBit ()


//! Sign-extend to 32 bits

//! Take a number of "bit" bits and sign extend it to 32 bits.

//! We use the trick from the Aggregate Magic Algorithms
//! (http://aggregate.org/MAGIC/).  If "n" is the value of the top bit
//! position, then we can extend by doing:

//!     (x ^ n) - n;

//! @param[in] val  The value to sign extend
//! @param[in] bit  The number of bits in the value to be extended

//! @return  The sign extended value.

uint32_t
AapSim::signExt (uint32_t      val,
		 unsigned int  bit)
{
  int32_t  maxbit = 1 << (bit - 1);
  return  (uint32_t) ((int32_t) val ^ maxbit) - maxbit;

}	// signExt ()


//! Is this a 16-bit instruction

//! Version for a 16-bit arg.

//! @param instr  The instruction to check

bool
AapSim::is16bit (uint16_t instr)
{
  return ((instr & 0x8000) == 0x0000);

}	// is16bit ()


//! Is this a 16-bit instruction

//! Version for a 32-bit arg.

//! @param instr  The instruction to check

bool
AapSim::is16bit (uint32_t instr)
{
  return ((instr & 0x00008000) == 0x00000000);

}	// is16bit ()


//! Fetch an instruction from memory

//! A private convenience function, which sets the result appropriately.

void
AapSim::fetch ()
{
  uint16_t  instr16;
  uint32_t  instr32;
  MemAddr  currPc = pc ();

  //! Can we get a word
  if (validAddr (currPc))
    {
      instr16 = rawMem (currPc);
    }
  else
    {
      mRes = AapSim::Res::MEMERR;
      return;
    }

  //! Advance the PC.  Note that this can be out of memory - we may not
  //! executed anything from here.
  pc (MemAddr (MemAddr::Space::CODE, currPc.location () + 1));
  currPc = pc ();

  // All done if a 16-bit instruction.
  if (is16bit (instr16))
    return;

  // Get a second word
  instr32 = instr16;

  if (validAddr (currPc))
    {
      instr16 = rawMem (currPc);
    }
  else
    {
      mRes = AapSim::Res::MEMERR;
      return;
    }

  instr32 |= ((uint32_t) instr16) << 16;

  //! Advance the PC.  Note that this can be out of memory - we may not
  //! executed anything from here.
  pc (MemAddr (MemAddr::Space::CODE, currPc.location () + 1));

  return;

}	// fetch ()


//! Output operator for AapSim::Res

std::ostream &
operator<< (std::ostream & s,
	    AapSim::Res  res)
{
  switch (res)
    {
    case AapSim::Res::EXIT:    return s << "exit";
    case AapSim::Res::COUT:    return s << "cout";
    case AapSim::Res::CERR:    return s << "cerr";
    case AapSim::Res::BREAK:   return s << "break";
    case AapSim::Res::ILLINST: return s << "illinst";
    case AapSim::Res::ILLOPND: return s << "illopnd";
    case AapSim::Res::MEMERR:  return s << "memerr";
    case AapSim::Res::REGERR:  return s << "regerr";
    case AapSim::Res::NONE:    return s << "none";
    default:                   return s << "unknown";
    }
};	// operator<< ()


// Local Variables:
// mode: C++
// c-file-style: "gnu"
// show-trailing-whitespace: t
// End:
