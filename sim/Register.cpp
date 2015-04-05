// ----------------------------------------------------------------------------
// Register: implementation

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



#include <iostream>

#include "RspProxyMain.h"
#include "Register.h"

using std::cerr;
using std::dec;
using std::endl;
using std::hex;


//-----------------------------------------------------------------------------
//! Constructor

//! There are no arguments, since the norm is to allocate an array, and then
//! subsequently set up individual registers.

//! Give the state arbitrary values
//-----------------------------------------------------------------------------
Register::Register ()
{
  name  = "Unset";
  size  = -1;
  value = 0;

}	// Register ()


//-----------------------------------------------------------------------------
//! Set the state fields of the register

//! The size and value are validated. An invalid size will be adjusted with a
//! warning. An invalid value will cause a warning and the value will be
//! truncated to fit.

//! @param[in] _name   The name of the register
//! @param[in] _size   The size (in bits) of the register
//! @param[in] _value  The value ofthe register
//-----------------------------------------------------------------------------
void
Register::set (const char *_name,
	       const int   _size,
	       uint64_t    _value)
{
  // Set the fields, validating the size and value as we do so.
  name  = _name;
  size  = validateSize (_size);
  value = validateValue (_value);

}	// set ()


//-----------------------------------------------------------------------------
//! Get the name of the register

//! @return  The name of the register
//-----------------------------------------------------------------------------
const char *
Register::getName ()
{
  return  name;

}	// getName ()


//-----------------------------------------------------------------------------
//! Get the size of the register

//! @return  The size of the register
//-----------------------------------------------------------------------------
int
Register::getSize ()
{
  return  size;

}	// getSize ()


//-----------------------------------------------------------------------------
//! Set the value of the register.

//! The value is checked to ensure it is consistent with the size, truncating
//! if necessary.

//! @param[in] _value  The value ofthe register
//-----------------------------------------------------------------------------
void
Register::setValue (uint64_t  _value)
{
  // Set the field, validating as we do so.
  value = validateValue (_value);

}	// setValue ()


//-----------------------------------------------------------------------------
//! Get the value of the register.

//! @note No need to validate the value. That is always done when the value is
//!       set, so it must be correct here.

//! @return  The value of the register
//-----------------------------------------------------------------------------
uint64_t
Register::getValue ()
{
  return  value;

}	// getValue ()


//-----------------------------------------------------------------------------
//! Utility to validate the size of the register.

//! An impossible value will cause an error and the register will be limited
//! to the maximum permitted.

//! @param[in] _size  The proposed size of the register
//-----------------------------------------------------------------------------
int
Register::validateSize (int  _size)
{
  if ((_size < 1) || (MAX_REG_BITSIZE < _size))
    {
      int  newSize = (_size < 1) ? 1 : MAX_REG_BITSIZE;

      cerr << "Warning: register " << name << ": size " << _size
	   << " bits is not valid - set to " << newSize << endl;
      return  newSize;
    }
  else
    {
      return  _size;
    }
}	// validateSize ()


//-----------------------------------------------------------------------------
//! Utility to validate the value of a the register

//! The value is checked to ensure it is consistent with the size.  If the
//! register is small and being used to hold signed values, it should only be
//! sign extended to the size of the register or it will hit a warning.
//! However we only truncate, so things will behave correctly.

//! @param[in] _value  The proposed value of the register

//! @return  The value, truncated if necessary.
//-----------------------------------------------------------------------------
uint64_t
Register::validateValue (uint64_t  _value)
{
  // Validate the value and truncate if necessary.
  if ((size < MAX_REG_BITSIZE) && (_value >= ((uint64_t)1 << size)))
    {
      cerr << "Warning: register " << name  << ": value " << hex
	   << _value << " does not fit in " << dec << size
	   << " bits: truncated." << endl;
      return  _value & (((uint64_t)1 << size) - 1);
    }
  else
    {
      return  _value;
    }
}	// validateValue ()


// Local Variables:
// mode: C++
// c-file-style: "gnu"
// show-trailing-whitespace: t
// End:
