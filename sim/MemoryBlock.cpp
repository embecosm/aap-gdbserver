// ----------------------------------------------------------------------------
// Memory block: implementation

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


#include <cstdlib>

#include "RspProxyMain.h"
#include "MemoryBlock.h"


//-----------------------------------------------------------------------------
//! Constructor with byte sized initialization.

//! @note This does not need to know the endianness of the target

//! @param[in] _nextBlock  pointer to the next memory block (NULL if none)
//! @param[in] _startAddr  start address of the region of memory represented.
//! @param[in] numWords    Number of words in the array supplied
//! @param[in] wordArray   Array of words with which to initialize the memory.
//-----------------------------------------------------------------------------
MemoryBlock::MemoryBlock (MemoryBlock *_nextBlock,
			  uint32_t     _startAddr,
			  int          numBytes,
			  uint8_t     *byteArray) :
  nextBlock (_nextBlock),
  startAddr (_startAddr),
  byteSize (numBytes)
{
  memory = new uint8_t [byteSize];

  for (int b = 0 ; b < numBytes ; b++ )
    {
      memory[b] = byteArray[b];
    }
}	// MemoryBlock ()


//-----------------------------------------------------------------------------
//! Constructor with word sized initialization.

//! @note This needs to know the endianness of the target

//! @param[in] _nextBlock             pointer to the next memory block (NULL if
//!                                   none) 
//! @param[in] _startAddr             start address of the region of memory
//!                                   represented. 
//! @param[in] numWords               number of words in the array supplied
//! @param[in] wordArray              array of words with which to initialize
//!                                   the memory.
//! @param[in] targetIsLittleEndianP  true if this is a little endian target

//-----------------------------------------------------------------------------
MemoryBlock::MemoryBlock (MemoryBlock *_nextBlock,
			  uint32_t     _startAddr,
			  int          numWords,
			  uint32_t    *wordArray,
			  bool         targetIsLittleEndianP) :
  nextBlock (_nextBlock),
  startAddr (_startAddr),
  byteSize (numWords * BYTES_PER_WORD)
{
  memory = new uint8_t [byteSize];

  for (int w = 0 ; w < numWords ; w++ )
    {
      int       b   = w * BYTES_PER_WORD;
      uint32_t  val = wordArray[w];

      if (targetIsLittleEndianP)
	{
	  // Little endian target
	  for (int  i = 0; i < BYTES_PER_WORD; i++)
	    {
	      memory[b + i]   = (uint8_t)(val & 0xff);
	      val           >>= 8;
	    }
	}
      else
	{
	  // Big endian target
	  for (int  i = BYTES_PER_WORD - 1; i >=0; i--)
	    {
	      memory[b + i]   = (uint8_t)(val & 0xff);
	      val           >>= 8;
	    }
	}
    }
}	// MemoryBlock ()

  
//-----------------------------------------------------------------------------
//! Destructor

//! Give back the memory. Since we are a linked list, we also delete any other
//! memory blocks to which we are linked.
//-----------------------------------------------------------------------------
MemoryBlock::~MemoryBlock ()
{
  if (NULL != nextBlock)
    {
      delete  nextBlock;
    }

  delete [] memory;

}	// ~MemoryBlock ()


//-----------------------------------------------------------------------------
//! Return a pointer to the next memory block

//! @return  The pointer to the next memory block (or NULL if there is none).
//-----------------------------------------------------------------------------
MemoryBlock *
MemoryBlock::next ()
{
  return  nextBlock;

}	// next ()


//-----------------------------------------------------------------------------
//! Indicate if an address is valid for this memory block

//! @return  true if this is a valid address, false otherwise.
//-----------------------------------------------------------------------------
bool
MemoryBlock::isValidAddress (uint32_t  addr)
{
  uint32_t byteOffset = addr - startAddr;

  return (0 <= byteOffset) & (byteOffset < byteSize);

}


//-----------------------------------------------------------------------------
//! Write a byte to memory

//! @note This does not need to know the endianness of the target

//! @param[in] addr   The address to write
//! @param[in] value  The byte to write

//! @return  true if the write was successful (address was valid), false
//!          otherwise.
//-----------------------------------------------------------------------------
bool
MemoryBlock::write (uint32_t  addr,
		    uint8_t   value)
{
  if (isValidAddress (addr))
    {
      memory[addr - startAddr] = value;
      return  true;
    }
  else
    {
      return  false;
    }
}	// write ()


//-----------------------------------------------------------------------------
//! Read a byte from memory

//! @note This does not need to know the endianness of the target

//! @param[in]  addr   The address to read
//! @param[out] value  The byte read

//! @return  true if the read was successful (address was valid), false
//!          otherwise.
//-----------------------------------------------------------------------------
bool
MemoryBlock::read (uint32_t  addr,
		   uint8_t  &value)
{
  if (isValidAddress (addr))
    {
      value = memory[addr - startAddr];
      return  true;
    }
  else
    {
      return  false;
    }
}


// Local Variables:
// mode: C++
// c-file-style: "gnu"
// show-trailing-whitespace: t
// End:
