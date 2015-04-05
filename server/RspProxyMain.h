// ----------------------------------------------------------------------------
// System wide declarations

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

// SystemC declarations that should be visible anywhere.


#ifndef ORPSOC_MAIN__H
#define ORPSOC_MAIN__H


//! Maximum size of a register in bits
#define  MAX_REG_BITSIZE  64

//! Number of bytes in a word
#define  BYTES_PER_WORD  (sizeof (uint32_t) / sizeof (uint8_t))

#endif	// ORPSOC_MAIN__H


// Local Variables:
// mode: C++
// c-file-style: "gnu"
// show-trailing-whitespace: t
// End:
