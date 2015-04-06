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

#include "MemAddr.h"
#include <sstream>


//! Default constructor

//! Address zero in the code space (which is the reset location).

MemAddr::MemAddr (void) :
  mSpace (MemAddr::Space::CODE),
  mLocation (0)
{
}	// MemAddr ()


//! Constructor

//! Explicitly set each field

//! @param[in] _space     The address space to which this address relates
//! @param[in] _location  The location associated with this address, and whose
//!                       meaning is dependent on whether this is code or data.

MemAddr::MemAddr (const Space _space
		  const uint32_t  _location) :
  mSpace (_space),
  mLocation (_location)
{
}	// MemAddr ()


//! Accessor to get the address space

//! @return  The address space
MemAddr::Space
MemAddr::space (void) const
{
  return mSpace;

}	// space ()


//! Accessor to set the address space

//! @param[in] _space  The address space
void
MemAddr::space (const MemAddr::Space _space)
{
  mSpace = _space;

}	// space ()


//! Accessor to get the location associated with the address

//! The addressibility is defined by the address class.

//! @return  The location
uint32_t
MemAddr::location (void) const
{
  return mLocation;

}	// location ()


//! Accessor to set the location (code or data)

//! The addressibility is defined by whether we are code or data.

//! @param[in] _location  The location
void
MemAddr::location (const uint32_t  _location)
{
  mLocation = _location;

}	// location ()


//! overload == operator for comparison of addresses

bool
MemAddr::operator==  (const MemAddr &pair) const
{
  if((pair.space() == mSpace) &&
     (pair.location() == mLocation) )
    return true;

  return false;

}	// operator== ()


//! overload != operator for comparison of addresses
bool
MemAddr::operator!=  (const MemAddr &pair) const
{
	return !(*this == pair);
}


//! Output operator for MemAddr::Space

std::ostream &
operator<< (std::ostream & s,
	    MemAddr::Space  space)
{
  const char * name;

  switch (c)
    {
    case MemAddr::Space::CODE: name = "code";    break;
    case MemAddr::Space::DATA: name = "data";    break;
    default:                   name = "unknown"; break;
    }

  return  s << name;

};	// operator<< ()


//! Output operator for MemAddr

//! Print a pair of space and location.  Data locations are max 16 bits, so
//! need 4 digits, code locations are max 24 bits, so need 6 digits.

std::ostream &
operator<< (std::ostream & s,
	    MemAddr addr)
{
  int  w = addr.space () == MemAddr::Space::CODE ? 6 : 4;
  return  s << "{" << addr.space () << ", " << ", 0x" << std::hex
	    << std::setw (w) << std::setfill ('0') << addr.location ()
	    << std::setfill (' ') << std::setw (0) << std::dec << "}";

};	// operator<< ()


// Local Variables:
// mode: C++
// c-file-style: "gnu"
// show-trailing-whitespace: t
// End:
