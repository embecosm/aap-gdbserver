// ----------------------------------------------------------------------------
// Matchpoint hash table: declaration

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


#ifndef MP_HASH__H
#define MP_HASH__H

#include <cstdint>

#include "MemAddr.h"


//! A hash table for matchpoints

//! We do this as our own open hash table. Our keys are a pair of entities
//! (address and type), so STL map is not trivial to use.

class MpHash
{
public:

  //! Enumeration of different types of matchpoint.  These have explicit
  //! values matching the second digit of 'z' and 'Z' packets.

  enum class MpType : int {
    BP_MEMORY   = 0,
    BP_HARDWARE = 1,
    WP_WRITE    = 2,
    WP_READ     = 3,
    WP_ACCESS   = 4
  };

  // Constructor and destructor. Default size is largest prime < 2^10

  MpHash (int  _size = 1021);
  ~MpHash ();

  // Accessor methods

  void  add (MpType    type,
	     MemAddr   addr,
	     uint16_t  instr);
  bool  remove (MpType     type,
		MemAddr    addr,
		uint16_t & instr);

private:

  //! A structure for hash table entries
  struct MpEntry
  {
  public:

    MpType    type;		//!< Type of matchpoint
    MemAddr   addr;		//!< Address with the matchpoint
    uint16_t  instr;		//!< Substituted instruction
    MpEntry  *next;		//!< Next in this slot
  };

  //! The hash table
  MpEntry **mHashTab;

  //! Size of the hash table
  int  mSize;

};


//! Output operators
std::ostream &
operator<< (std::ostream & s,
	    MpHash::MpType  t);


#endif	// MP_HASH__H


// Local Variables:
// mode: C++
// c-file-style: "gnu"
// show-trailing-whitespace: t
// End:
