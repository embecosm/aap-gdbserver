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


#include <cstdlib>

#include "MpHash.h"


//! Constructor

//! Allocate the hash table
//! @param[in] size  Number of slots in the  hash table. Defaults to
//!                  1021, the largest prime less than 2^10

MpHash::MpHash (int  _size) :
  mSize (_size)
{
  // Allocate and clear the hash table
  mHashTab = new MpHash::MpEntry *[mSize];

  for (int  i = 0; i < mSize; i++)
    mHashTab[i] = NULL;

}	// MpHash ()


//! Destructor

//! Free the hash table

//! @todo Free the space allocated for hash table entries.

MpHash::~MpHash ()
{
  delete [] mHashTab;

}	// ~MpHash ()


//! Add an entry to the hash table

//! Add the entry if it wasn't already there. If it was there do nothing. The
//! match just be on type and addr. The instr need not match, since if this is
//! a duplicate insertion (perhaps due to a lost packet) they will be
//! different.

//! @note This method allocates memory. Care must be taken to delete it when
//! done.

//! @param[in] type   The type of matchpoint
//! @param[in] addr   The address of the matchpoint
//! @para[in]  instr  The instruction to associate with the address

void
MpHash::add (MpType    type,
	     MemAddr   addr,
	     uint16_t  instr)
{
  int              hv    = addr.location () % mSize;
  MpHash::MpEntry *curr;

  // See if we already have the entry

  for(curr = mHashTab[hv]; NULL != curr; curr = curr->next)
    if ((type == curr->type) && (addr == curr->addr))
      return;

  // Insert the new entry at the head of the chain
  curr = new MpHash::MpEntry ();

  curr->type  = type;
  curr->addr  = addr;
  curr->instr = instr;
  curr->next  = mHashTab[hv];

  mHashTab[hv] = curr;

}	// add ()


//! Delete an entry from the matchpoint hash table

//! If it is there the entry is deleted from the hash table. If it is not
//! there, no action is taken. The match must be on type AND addr. The entry
//! (MpEntry::) is itself deleted

//! The usual fun and games tracking the previous entry, so we can delete
//! things.

//! @param[in]  type   The type of matchpoint
//! @param[in]  addr   The address of the matchpoint
//! @param[out] instr  Location to place the instruction found.
//! @return  TRUE if an entry was found and deleted

bool
MpHash::remove (MpType     type,
		MemAddr    addr,
		uint16_t & instr)
{
  int              hv   = addr.location () % mSize;
  MpHash::MpEntry *prev = NULL;
  MpHash::MpEntry *curr;

  // Search
  for (curr  = mHashTab[hv]; NULL != curr; curr = curr->next)
    {
      if ((type == curr->type) && (addr == curr->addr))
	{
	  // Found - delete. Method depends on whether we are the head of
	  // chain.

	  if (NULL == prev)
	    mHashTab[hv] = curr->next;
	  else
	    prev->next = curr->next;

	  instr = curr->instr;	// Return the found instruction

	  delete curr;
	  return true;			// Success
	}

      prev = curr;
    }

  return  false;			// Not found

}	// remove ()


//! Output operator for MpHash::MpType

//! @param[in] s  The output stream
//! @param[in] t  The type to put on the stream
//! @return  The updated stream

std::ostream &
operator<< (std::ostream & s,
	    MpHash::MpType  t)
{
  const char * name;

  switch (t)
    {
    case MpHash::MpType::BP_MEMORY:   name = "sw break";     break;
    case MpHash::MpType::BP_HARDWARE: name = "hw break";     break;
    case MpHash::MpType::WP_WRITE:    name = "write watch";  break;
    case MpHash::MpType::WP_READ:     name = "read watch";   break;
    case MpHash::MpType::WP_ACCESS:   name = "access watch"; break;

    default:                          name = "unknown";      break;
    }

  return  s << name;

};	// operator<< ()


// Local Variables:
// mode: C++
// c-file-style: "gnu"
// show-trailing-whitespace: t
// End:
