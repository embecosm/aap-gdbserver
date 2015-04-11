// Memory address: declaration

// Copyright (C) 2015 Embecosm Limited <www.embecosm.com>

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

#ifndef MEM_ADDR_H
#define MEM_ADDR_H

#include <cstdint>
#include <iomanip>
#include <iostream>
#include <string>


//! A class to represent a memory address in AAP architecture

//! This is a Harvard architecture, with word adddressed code memory.  So we
//! need to indicate whether the address is for code or memory.

class MemAddr
{
public:

  //! Public type for the address space
  enum class Space {
    CODE,
    DATA
  };

  // Constructors
  MemAddr (void);
  MemAddr (const Space _space,
	   const uint32_t  _location);

  // Accessors
  Space  space (void) const;
  void space (const Space _space);
  uint32_t location () const;
  void location (const uint32_t _location);

  virtual bool operator == (const MemAddr &pair ) const;
  virtual bool operator != (const MemAddr &pair ) const;


private:

  //! Which code space
  Space  mSpace;

  //! The memory location.  Remember this may be a double word address for code
  uint32_t   mLocation;
};


//! Output operators
std::ostream &
operator<< (std::ostream & s,
	    MemAddr::Space  space);

std::ostream &
operator<< (std::ostream & s,
	    MemAddr  addr);


#endif //MEM_ADDR_H


// Local Variables:
// mode: C++
// c-file-style: "gnu"
// show-trailing-whitespace: t
// End:
