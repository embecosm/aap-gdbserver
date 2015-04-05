// ----------------------------------------------------------------------------
// Register: definition

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


#ifndef REGISTER__H
#define REGISTER__H

#include <stdint.h>


//-----------------------------------------------------------------------------
//! Representation of a register

//! The registers of the target will be represented as an array of this
//! class. It allows for the possibility that not all registers are the same
//! size.
//-----------------------------------------------------------------------------
class Register
{
public:

  // Constructor
  Register ();

  // Various accessor functions
  void        set (const char *_name,
		   const int   _size,
		   uint64_t    _value);
  const char *getName ();
  int         getSize ();
  void        setValue (uint64_t  _value);
  uint64_t    getValue ();


private:

  //! Our name
  const char *name;

  //! Our size in bits
  int  size;

  //! Our value. 64-bit allows for any size of register.
  uint64_t  value;

  // Utilities to validate the size and value of the register
  int       validateSize (int  _size);
  uint64_t  validateValue (uint64_t  _value);

};

#endif	// REGISTER__H


// Local Variables:
// mode: C++
// c-file-style: "gnu"
// show-trailing-whitespace: t
// End:
