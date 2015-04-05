// ----------------------------------------------------------------------------
// Memory block: definition

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


#ifndef MEMORY_BLOCK__H
#define MEMORY_BLOCK__H

#include <stdint.h>


//-----------------------------------------------------------------------------
//! Representation of a region of memory.

//! The entire simulated region is represented as a linked list of these
//! blocks.
//-----------------------------------------------------------------------------
class MemoryBlock
{
public:

  // Constructors and destructor
  MemoryBlock (MemoryBlock *_nextBlock,
	       uint32_t     _startAddr,
	       int          numBytes,
	       uint8_t     *byteArray);
  MemoryBlock (MemoryBlock *_nextBlock,
	       uint32_t     _startAddr,
	       int          numWords,
	       uint32_t    *wordArray,
	       bool         targetIsLittleEndianP);
  ~MemoryBlock ();

  // Various accessor functions
  MemoryBlock *next ();
  bool         isValidAddress (uint32_t  addr);
  bool         write (uint32_t  addr,
		      uint8_t   value);
  bool         read (uint32_t  addr,
		     uint8_t  &value);


private:

  //! Pointer to the next block (NULL if there is none)
  MemoryBlock *nextBlock;

  //! Start address of this memory block
  uint32_t  startAddr;

  //! Size (in bytes) of this memory block
  uint32_t  byteSize;

  //! Dynamically allocated memory array
  uint8_t *memory;

};

#endif	// MEMORY_BLOCK__H


// Local Variables:
// mode: C++
// c-file-style: "gnu"
// show-trailing-whitespace: t
// End:
