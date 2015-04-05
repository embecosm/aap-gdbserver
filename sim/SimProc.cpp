// ----------------------------------------------------------------------------
// Simulated Processor: implementation

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



#include <cstring>

#include "RspProxyMain.h"
#include "SimProc.h"
#include "MemoryBlock.h"


using std::cerr;
using std::endl;
using std::ifstream;
using std::va_list;

#ifdef PARSE_DEBUG
using std::cout;
#endif


//-----------------------------------------------------------------------------
//! Constructor.

//! At this stage we have no state. That will be set up by ::init ()
//! later. Set the memory and register pointers to sensible values.
//-----------------------------------------------------------------------------
SimProc::SimProc (bool  _traceOnP) :
  name (NULL),
  numRegs (-1),
  regArray (NULL),
  memList (NULL),
  traceOnP (_traceOnP)
{
  // Nothing to do

}	// SimProc ()


//-----------------------------------------------------------------------------
//! Destructor.

//! Free up any space allocated
//-----------------------------------------------------------------------------
SimProc::~SimProc ()
{
  if (NULL != name)
    {
      delete [] name;
    }

  if (NULL != regArray)
    {
      delete [] regArray;
    }

  if (NULL != memList)
    {
      delete  memList;			// This will recurse to delete
    }
}	// ~SimProc ()


//-----------------------------------------------------------------------------
//! Report if tracing is enabled.

//! @return  true if tracing is enabled, false otherwise.
//-----------------------------------------------------------------------------
bool
SimProc::isTraceOn ()
{
  return  traceOnP;

}	// isTraceOn ()


//-----------------------------------------------------------------------------
//! Initialize the simulated processor from a file.

//! @param[in] filename  The file to initialize from.
//! @param[in] traceOnP  true if we should enable tracing

//! @return  true if initialization succeeds, false otherwise.
//-----------------------------------------------------------------------------
bool
SimProc::init (const char *filename)
{
  fh = new ifstream (filename);

  parse ();

  if (parseWarningCount > 0)
    {
      const char *warn = (1 == parseWarningCount) ? "warning" : "warnings";
      cerr << parseWarningCount << " " << warn << " found" << endl;
    }

  if (parseErrorCount > 0)
    {
      const char *err = (1 == parseErrorCount) ? "error" : "errors";
      cerr << parseErrorCount << " " << err << " found" << endl;
      cerr << "Terminating execution" << endl;

      return  false;
    }

  return  true;

}	// init ()


//-----------------------------------------------------------------------------
//! Identify whether this is a little endian architecture.

//! @return  true if this is a little endian architecture, false otherwise.
//-----------------------------------------------------------------------------
bool
SimProc::isLittleEndian ()
{
  return  isLittleEndianP;

}	// isLittleEndian ()


//-----------------------------------------------------------------------------
//! Return the number of registers

//! @return  The number of registers
//-----------------------------------------------------------------------------
int
SimProc::getNumRegs ()
{
  return  numRegs;

}	// getNumRegs ()


//-----------------------------------------------------------------------------
//! Check if a register number is valid

//! @param[in] regNum  The register number to check.

//! @return  true if this is a valid register, false otherwise.
//-----------------------------------------------------------------------------
bool
SimProc::isValidReg (int  regNum)
{
  return  (0 <= regNum) && (regNum <= numRegs);

}	// isValidReg ()


//-----------------------------------------------------------------------------
//! Get the size in bits of a register

//! @param[in]   regNum  The register of interest
//! @param[out]  size    The size of the register

//! @return  true if this is a valid register to read, false otherwise.
//-----------------------------------------------------------------------------
bool
SimProc::getRegSize (int  regNum,
		     int &size)
{
  if (isValidReg (regNum))
    {
      size = regArray[regNum].getSize ();
      return  true;
    }
  else
    {
      cerr << "ERROR: Attempt to get size of invalid register " << regNum
	   << endl;
    }
}	// getRegSize ()


//-----------------------------------------------------------------------------
//! Read the value of a register.

//! @param[in]  regNum  The register to read
//! @param[out] value   The value read

//! @return  true if this is a valid register to read, false otherwise.
//-----------------------------------------------------------------------------
bool
SimProc::readReg (int       regNum,
		  uint64_t &value)
{
  if (isValidReg (regNum))
    {
      value = regArray[regNum].getValue ();
      return  true;
    }
  else
    {
      cerr << "ERROR: Attempt to read invalid register " << regNum << endl;
      return  false;
    }
}	// readReg ()


//-----------------------------------------------------------------------------
//! Write the value of a register

//! @param[in]  regNum  The register to write
//! @param[out] value   The value to write

//! @return  true if this is a valid register to write, false
//!           otherwise. Writing an invalid value to a valid register will
//!           provoke a rude message, but will still return true.
//-----------------------------------------------------------------------------
bool
SimProc::writeReg (int       regNum,
		   uint64_t  value)
{
  if (isValidReg (regNum))
    {
      regArray[regNum].setValue (value);
      return  true;
    }
  else
    {
      cerr << "ERROR: Attempt to write invalid register " << regNum << endl;
      return  false;
    }
}	// writeReg ()


//-----------------------------------------------------------------------------
//! See if the given address is a valid address in any of our memory blocks.
//-----------------------------------------------------------------------------
bool
SimProc::isValidAddr (uint32_t  addr)
{
  MemoryBlock *mb = memList;

  for (mb = memList; NULL != mb; mb = mb->next())
    {
      if (mb->isValidAddress (addr))
	{
	  return  true;
	}
    }

  return  false;			// Not found
	
}


//-----------------------------------------------------------------------------
//! Read a byte from memory

//! @note This does not need to know the endianness of the target

//! @param[in]  addr   The address to read
//! @param[out] value  The byte read

//! @return  true if the read was successful (address was valid), false
//!          otherwise.

//-----------------------------------------------------------------------------
bool
SimProc::readMem (uint32_t  addr,
		  uint8_t  &value)
{
  MemoryBlock *mb = memList;

  for (mb = memList; NULL != mb; mb = mb->next())
    {
      if (mb->isValidAddress (addr))
	{
	  return  mb->read (addr, value);
	}
    }

  return  false;			// Not found

}	// readMem ()


//-----------------------------------------------------------------------------
//! Write a byte to memory

//! @note This does not need to know the endianness of the target

//! @param[in] addr   The address to write
//! @param[in] value  The byte to write

//! @return  true if the write was successful (address was valid), false
//!          otherwise.
//-----------------------------------------------------------------------------
bool
SimProc::writeMem (uint32_t  addr,
		   uint8_t   value)
{
  MemoryBlock *mb = memList;

  for (mb = memList; NULL != mb; mb = mb->next())
    {
      if (mb->isValidAddress (addr))
	{
	  return  mb->write (addr, value);
	}
    }

  return  false;			// Not found

}	// writeMem ()


//-----------------------------------------------------------------------------
//! Parse the definition file

//! The file has already been opened. This is a simple recursive descent
//! parser.

//! The general approach is that recursing functions always recover errors to
//! a point where the calling function could continue.
//-----------------------------------------------------------------------------
void
SimProc::parse ()
{
#ifdef PARSE_DEBUG
  cout << "-- parse" << endl;
#endif

  lineNumber        = 1;
  parseWarningCount = 0;
  parseErrorCount   = 0;

  ch     = -1;				// Init the char reader
  nextCh = -1;				// No call to unreadChar () yet
  readChar ();

  scan ();				// Get the first lexeme

  parseDescription ();

}	// parse ()


//-----------------------------------------------------------------------------
//! Parse the description rule

//! The rule is:

//!   description -> nameClause registerClause memoryClauseList
//-----------------------------------------------------------------------------
void
SimProc::parseDescription ()
{
#ifdef PARSE_DEBUG
  cout << "-- parseDescription" << endl;
#endif

  parseNameClause ();
  parseRegisterClause ();
  parseMemoryClauseList ();

}	// parseDescription ()


//-----------------------------------------------------------------------------
//! Parse the name clause rule

//! The rule is:

//!   nameClause -> NAME '(' nameParams ')'
//-----------------------------------------------------------------------------
void
SimProc::parseNameClause ()
{
#ifdef PARSE_DEBUG
  cout << "-- parseNameClause" << endl;
#endif

  // Is the NAME keyword there?
  if (SO_NAME == lexeme)
    {
      scan ();				// Skip the NAME token
    }
  else
    {
      // Error recovery time. If the next token is '(' we can carry
      // on. Otherwise we give up on the NAME clause and scan for a REGISTERS,
      // BYTE, WORD or EOF.
      if ('(' == lexeme)
	{
	  parseError ("Missing NAME keyword");
	}
      else
	{
	  parseError ("Unrecognized NAME specification. Skipping");
	  skipBlock ();
	  return;
	}
    }

  // Should now be '('
  if ('(' == lexeme)
    {
      scan ();				// Skip the '('
    }
  else
    {
      // Error recovery time. If the next token is a string, assume that the
      // user just forgot the brackets. Otherwise we give up on the NAME
      // clause and scan for a REGISTERS, BYTE, WORD or EOF.
      if (SO_STRING == lexeme)
	{
	  parseError ("Missing NAME opening parentheses");
	  parseNameParams ();
	  return;
	}
      else
	{
	  parseError ("Unrecognized NAME specification. Skipping");
	  skipBlock ();
	  return;
	}
    }

  // Recurse to find the name parameters
  parseNameParams ();

  // We should now have a ')'
  if (')' == lexeme)
    {
      scan ();				// Skip the ')'
    }
  else
    {
      // Error recovery time. If the next token is REGISTERS, we presumably
      // misssed a closing bracket. Otherwise we give up on the NAME
      // clause and scan for a REGISTERS, BYTE, WORD or EOF.
      if (SO_REGISTERS == lexeme)
	{
	  parseError ("Missing closing parenthesis for NAME specification");
	}
      else
	{
	  parseError ("Unrecognized NAME specification. Skipping");
	  skipBlock ();
	  return;
	}
    }
}	// parseNameClause ()


//-----------------------------------------------------------------------------
//! Parse the nameParams rule

//! The rule is:

//!   nameParams -> STRING ',' endianness
//-----------------------------------------------------------------------------
void
SimProc::parseNameParams ()
{
#ifdef PARSE_DEBUG
  cout << "-- parseNameParams" << endl;
#endif

  bool  nameMissingP = false;		// Flag if the name was missing

  // Look for the string
  if (SO_STRING == lexeme)
    {
      scan ();				// Skip the string
    }
  else
    {
      // Error recovery time. If the next token is ',' we assume the string
      // was missing and carry on. If the next token is BIG or LITTLE, we
      // assume the name and comma are missing, but we do nothing here, since
      // it will get swept up later. Otherwise we give up, but we still return
      // true, since the next level up will worry about whether it can read
      // to the closing ')'.
      if ((',' == lexeme) || (SO_BIG == lexeme) || (SO_LITTLE == lexeme))
	{
	  nameMissingP = true;
	  parseError ("Missing name");
	}
      else
	{
	  parseError ("Cannot recognize name parameters");
	  return;
	}
    }

  // Look for the comma
  if (',' == lexeme)
    {
      scan ();				// Skip the comma
    }
  else
    {
      // Error recovery time. If the next token is BIG or LITTLE, then we are
      // fine. We only report an error if we have not already reported the
      // name being missing. Otherwise we are confused, but we still return
      // true, since the next level up will worry about whether it can read
      // to the closing ')'.
      if ((SO_BIG == lexeme) || (SO_LITTLE == lexeme))
	{
	  if (!nameMissingP)
	    {
	      parseError ("Missing ',' in name specification");
	    }
	}
      else
	{
	  parseError ("Cannot recognize name parameters");
	  return;
	}
    }

  // Look for the endianness
  parseEndianness ();

}	// parseNameParams ()


//-----------------------------------------------------------------------------
//! Parse the endianness rule

//! The rule is:

//!   endianness = BIG ENDIAN | LITTLE ENDIAN
//-----------------------------------------------------------------------------
void
SimProc::parseEndianness ()
{
#ifdef PARSE_DEBUG
  cout << "-- parseEndianness" << endl;
#endif

  bool  bigLittleMissingP = false;

  // Look for BIG or LITTLE
  if ((SO_BIG == lexeme) || (SO_LITTLE == lexeme))
    {
      isLittleEndianP = (SO_LITTLE == lexeme);
      scan ();				// Skip the BIG/LITTLE
    }
  else
    {
      // Error recovery time. Just note that the keyword is missing
      parseError ("Endianness not specified");
      bigLittleMissingP = true;
    }

  // Look for ENDIAN
  if (SO_ENDIAN == lexeme)
    {
      scan ();				// Skip ENDIAN
    }
  else
    {
      // Error recovery time. Note the keyword is missing, unless we have
      // already noted that BIG or LITTLE is missing. Actually we can live
      // with this, so it is just a warning.
      if (!bigLittleMissingP)
	{
	  parseWarning ("ENDIAN keyword missing");
	}
    }
}	// parseEndianness ()


//-----------------------------------------------------------------------------
//! Parse the registerClause rule

//! The rule is:

//! registerClause -> REGISTERS '(' NUMBER ')' registerList

//! Once we have the number we can allocate the register array.
//-----------------------------------------------------------------------------
void
SimProc::parseRegisterClause ()
{
#ifdef PARSE_DEBUG
  cout << "-- parseRegisterClause" << endl;
#endif

  // Look for the REGISTERS keyword
  if (SO_REGISTERS == lexeme)
    {
      scan ();				// Skip the REGISTERS keyword
    }
  else
    {
      // Error recovery time. If the next token is '(' we can carry
      // on. Otherwise we give up on the REGISTERS clause and scan for a BYTE,
      // WORD or EOF.
      if ('(' == lexeme)
	{
	  parseError ("Missing REGISTERS keyword");
	}
      else
	{
	  parseError ("Unrecognized REGISTERS specification. Skipping");
	  skipBlock ();
	  return;
	}
    }

  bool  parenthesesMissingP = false;	// Flag missing parentheses

  // Look for the opening '('
  if ('(' == lexeme)
    {
      scan ();				// Skip '('
    }
  else
    {
      // Error recovery time. If the next token is a number, then the user
      // probably forgot the opening parenthesis. Otherwise we give up on the
      // REGISTERS clause and scan for a BYTE, WORD or EOF.
      if (SO_NUMBER == lexeme)
	{
	  parseError ("Missing open parenthesis in REGISTERS specification");
	  parenthesesMissingP = true;
	}
      else
	{
	  parseError ("Unrecognized REGISTERS specification. Skipping");
	  skipBlock ();
	  return;
	}
    }

  // Look for the number
  if (SO_NUMBER == lexeme)
    {
      numRegs = numberLval;		// Capture the number of registers
      scan ();				// Skip the number
    }
  else
    {
      // Error recovery time. If we find a closing parenthesis, then we are
      // probably just missing the register size. Otherwise we give up on the
      // REGISTERS clause and scan for a BYTE, WORD or EOF. Note that we
      // cannot have both opening parenthesis and number missing - that would
      // have been caught earlier.
      if (')' == lexeme)
	{
	  parseError ("Missing register count in REGISTERS specification");
	}
      else
	{
	  parseError ("Unrecognized REGISTERS specification. Skipping");
	  skipBlock ();
	  return;
	}
    }

  // Look for the closing parenthesis
  if (')' == lexeme)
    {
      scan ();				// Skip the ')'
    }
  else
    {
      // Error recovery time. We are OK if the next token is '{'. We only
      // print a message if we had an opening parenthesis. Otherwise we give
      // up on the REGISTERS clause and scan for a BYTE, WORD or EOF.
      if ('{' == lexeme)
	{
	  if (!parenthesesMissingP)
	    {
	      parseError ("Missing close parenthesis in REGISTERS "
			  "specification");
	    }
	}
      else
	{
	  parseError ("Unrecognized REGISTERS specification. Skipping");
	  skipBlock ();
	  return;
	}
    }

  // Allocate the register array and set the counter for populating it.
  regArray = new Register [numRegs];

  // Parse the register list
  parseRegisterList ();

}	// parseRegisterClause ()


//-----------------------------------------------------------------------------
//! Parse the registerList rule

//! The rule is:

//!   registerList -> register | register registerList

//! This is parsed in a loop, rather than recursing.
//-----------------------------------------------------------------------------
void
SimProc::parseRegisterList ()
{
#ifdef PARSE_DEBUG
  cout << "-- parseRegisterList" << endl;
#endif

  int  nextReg = 0;			// Next register to process

  // Parse loop (must be at least one)
  do
    {
      parseRegister (nextReg);
      nextReg++;
    }
  while ('{' == lexeme);

}	// parseRegisterList ()


//-----------------------------------------------------------------------------
//! Parse the register rule

//! The rule is:

//!    register -> '{' STRING ',' NUMBER ',' NUMBER '}'

//! Use the information to populate the register array.

//! @param[in] regNum  The register to populate in the array.
//-----------------------------------------------------------------------------
void
SimProc::parseRegister (int  regNum)
{
#ifdef PARSE_DEBUG
  cout << "-- parseRegister" << endl;
#endif

  char     *regName;		// The name of this register
  int       regBits;		// Number of bits in this register
  uint64_t  regVal;		// Initial value of the register

  // Look for the opening parenthesis
  if ('{' == lexeme)
    {
      scan ();				// Skip the '{'
    }
  else
    {
      // Error recovery time. The only time we can be called without an
      // opening parenthesis, is the very first time, which suggests we have a
      // corrupted REGISTERS specification. We allow recovery by skipping to
      // the start of a memory block if possible.
      parseError ("Badly formed REGISTERS data. Skipping.");
      skipBlock ();
      return;
    }
  
  // Look for the string representing the name of this register
  if (SO_STRING == lexeme)
    {
      // Copy the name of this register, removing the opening and closing
      // quotes.
      regName                       = strdup (&(lval[1]));
      regName[strlen (regName) - 1] = '\0';
  
      scan ();				// Skip the string
    }
  else
    {
      // Error recovery time. If we get a failure between braces, we just scan
      // to the end brace and return.
      parseError ("Register name missing. Skipping register specification");
      skipBrace ();
      return;
    }

  // Look for the comma
  if (',' == lexeme)
    {
      scan ();				// Skip the comma
    }
  else
    {
      // Error recovery time. If we get a failure between braces, we just scan
      // to the end brace and return.
      parseError ("comma missing after name. Skipping register specification");
      skipBrace ();
      return;
    }

  // Look for the number representing the size of this register
  if (SO_NUMBER == lexeme)
    {
      regBits = numberLval;		// Capture the bit size
      scan ();				// Skip the number
    }
  else
    {
      // Error recovery time. If we get a failure between braces, we just scan
      // to the end brace and return.
      parseError ("Register size missing. Skipping register specification");
      skipBrace ();
      return;
    }

  // Look for the comma
  if (',' == lexeme)
    {
      scan ();				// Skip the comma
    }
  else
    {
      // Error recovery time. If we get a failure between braces, we just scan
      // to the end brace and return.
      parseError ("comma missing after size. Skipping register specification");
      skipBrace ();
      return;
    }

  // Look for the number representing the initial value of this register
  if (SO_NUMBER == lexeme)
    {
      regVal = numberLval;		// Capture the initial value
      scan ();				// Skip the number
    }
  else
    {
      // Error recovery time. If we get a failure between braces, we just scan
      // to the end brace and return.
      parseError ("Register value missing. Skipping register specification");
      skipBrace ();
      return;
    }

  // Look for the closing brace
  if ('}' == lexeme)
    {
      scan ();				// Skip the brace
    }
  else
    {
      // Error recovery time. Scan until we find an opening brace, BYTE, WORD
      // or EOF, all of which are valid restart positions
      parseError ("Closing brace missing on register specification");

      while (('{' != lexeme) && (SO_BYTE != lexeme) &&
	     (SO_WORD != lexeme) && (SO_EOF != lexeme))
	{
	  scan ();			// Skip unwanted symbol
	}

      return;
    }

  // Set up the register, unless we already have too many
  if (regNum < numRegs)
    {
      regArray[regNum].set (regName, regBits, regVal);
    }
  else
    {
      parseError ("Too many registers specified: %d", regNum);
    }
}	// parseRegister ()


//-----------------------------------------------------------------------------
//! Parse the memoryClauseList rule

//! The rule is:

//!   memoryClauseList -> memoryClause | memoryClause memoryClauseList

//! This is parsed in a loop, rather than recursing.
//-----------------------------------------------------------------------------
void
SimProc::parseMemoryClauseList ()
{
#ifdef PARSE_DEBUG
  cout << "-- parseMemoryClauseList" << endl;
#endif

  // Parse loop (must be at least one)
  do
    {
      parseMemoryClause ();
    }
  while ((SO_BYTE == lexeme) || (SO_WORD == lexeme));

}	// parseMemoryClauseList ()


//-----------------------------------------------------------------------------
//! Parse the memoryClause rule

//! The rule is:

//!   memoryClause -> byteMemoryClause | wordMemoryClause
//-----------------------------------------------------------------------------
void
SimProc::parseMemoryClause ()
{
#ifdef PARSE_DEBUG
  cout << "-- parseMemoryClause" << endl;
#endif

  // Should start with BYTE or WORD
  switch (lexeme)
    {
    case SO_BYTE:
    case SO_WORD:
      parseSpecificMemoryClause (lexeme);
      break;

    default:
      // Error recovery time. We have rubbish. Scan until we find either the
      // start of a new memory clause or EOF.
      parseError ("Memory specification expected, but not found. Skipping.");
      skipBlock ();
      break;
    }
}	// parseMemoryClause ()


//-----------------------------------------------------------------------------
//! Parse the byteMemoryClause and wordMemoryClause rules

//! The rules are:

//!   byteMemoryClause -> BYTE MEMORY '(' NUMBER ',' NUMBER ')' memoryValues
//!   wordMemoryClause -> WORD MEMORY '(' NUMBER ',' NUMBER ')' memoryValues

//! Once we have the two numbers (which are the memory base address and memory
//! size in bytes respectively), we can create a MemoryBlock object, with a
//! byte array for the memory block, which can be populated by
//! parseMemoryValues ().

//! @param[in] memoryType  SO_BYTE if this is a byteMemoryClause, SO_WORD if
//!                        it is a wordMemoryClause.
//-----------------------------------------------------------------------------
void
SimProc::parseSpecificMemoryClause (ScannerObject  memoryType)
{
#ifdef PARSE_DEBUG
  cout << "-- parseSpecificMemoryClause" << endl;
#endif

  uint32_t  baseAddr;
  uint32_t  byteSize = 0;		// Zero indicates failure
  
  // Look for BYTE or WORD
  if (memoryType == lexeme)
    {
      scan ();				// Skip BYTE or WORD
    }
  else
    {
      // Error recovery time. We should never get here, so if we do scan until
      // we find either the start ofa new memory clause or EOF.
      parseError ("Memory specification expected, but not found. Skipping.");
      skipBlock ();
      return;
    }

  // Look for MEMORY
  if (SO_MEMORY == lexeme)
  {
    scan ();				// Skip MEMORY
  }
  else
    {
      // Error recovery time. If the next token is '(' we assume that the word
      // was just omitted, which is something we can live with. Otherwise we
      // skip until we find the start of the memory clause.
      if ('(' == lexeme)
	{
	  parseWarning ("MEMORY keyword omitted from memory specification");
	}
      else
	{
	  parseError ("Memory specification garbled. Skipping.");
	  skipBlock ();
	  return;
	}
    }

  // Look for opening parenthesis
  if ('(' == lexeme)
    {
      scan ();				// Skip the '('
    }
  else
    {
      // Error recovery time. If the next token is a number, assume that the
      // user just forgot the brackets. Otherwise we give up on the BYE MEMORY
      // clause and scan for a BYTE, WORD or EOF.
      if (SO_NUMBER == lexeme)
	{
	  parseError ("Missing MEMORY opening parentheses");
	  parseMemoryParams (baseAddr, byteSize);
	  byteSize = 0;			// So we know there was an error
	  return;
	}
      else
	{
	  parseError ("Unrecognized MEMORY specification. Skipping");
	  skipBlock ();
	  return;
	}
    }

  // Recurse to find the memory parameters. This will also populate the
  // startAddr and byteSize, but using a zero value of byteSize to indicate
  // failure.
  parseMemoryParams (baseAddr, byteSize);

  // We should now have a ')'
  if (')' == lexeme)
    {
      scan ();				// Skip the ')'
    }
  else
    {
      // Error recovery time. If the next token is a NUMBER, we presumably
      // misssed a closing bracket. Otherwise we give up on the BYTE MEMORY
      // clause and scan for a BYTE, WORD or EOF.
      if (SO_NUMBER == lexeme)
	{
	  parseError ("Missing closing parenthesis for MEMORY specification");
	}
      else
	{
	  parseError ("Unrecognized MEMORY specification. Skipping");
	  skipBlock ();
	  return;
	}
    }

  // Allocate a new array of the appropriate type for the memory, parse the
  // memory block and create a new MemoryBlock object with the result.
  if (0 != byteSize)
    {
      if (SO_BYTE == memoryType)
	{
	  uint8_t *byteArray = new uint8_t [byteSize];
	  parseMemoryValues (memoryType, (int)byteSize, (void *)byteArray);
	  memList = new MemoryBlock (memList, baseAddr, byteSize, byteArray);
	  delete [] byteArray;
	}
      else
	{
	  uint32_t  wordSize  = byteSize / BYTES_PER_WORD;
	  uint32_t *wordArray = new uint32_t [wordSize];
	  parseMemoryValues (memoryType, (int)wordSize, (void *)wordArray);
	  memList = new MemoryBlock (memList, baseAddr, byteSize, wordArray,
				     isLittleEndianP);
	  delete [] wordArray;
	}
    }
}	// parseSpecificMemoryClause ()


//-----------------------------------------------------------------------------
//! Parse the memoryParams rule

//! The rule is:

//!   memoryParams -> NUMBER ',' NUMBER

//! The numbers are respectively the base address and size in bytes of the
//! memory block. Their values are returned through the arguments

//! @param[out] baseAddr  Base address of the memory.
//! @param[out] byteSize  Size of the memory in bytes, or zero if there was a
//!                       failure.
//-----------------------------------------------------------------------------
void
SimProc::parseMemoryParams (uint32_t &baseAddr,
			    uint32_t &byteSize)
{
#ifdef PARSE_DEBUG
  cout << "-- parseMemoryParams" << endl;
#endif

  bool  nameMissingP = false;

  byteSize = 0;				// Assume failure

  // Look for the first number (the memory base address)
  if (SO_NUMBER == lexeme)
    {
      baseAddr = numberLval;		// Capture the base address
      scan ();				// Skip the number
    }
  else
    {
      // Error recovery time. If the next token is ',' we assume the number
      // was missing and carry on. If the next token is a number, we
      // assume the number and comma are missing, but we do nothing here, since
      // it will get swept up later. Otherwise we give up, but we still return
      // true, since the next level up will worry about whether it can read
      // to the closing ')'.
      if ((',' == lexeme) || (SO_NUMBER == lexeme))
	{
	  nameMissingP = true;
	  parseError ("Missing memory base address");
	}
      else
	{
	  parseError ("Cannot recognize memory parameters");
	  return;
	}
    }

  // Look for the comma
  if (',' == lexeme)
    {
      scan ();				// Skip the comma
    }
  else
    {
      // Error recovery time. If the next token is a number, then we are
      // fine. We only report an error if we have not already reported the
      // first number being missing. Otherwise we are confused, but we still
      // return true, since the next level up will worry about whether it can
      // read to the closing ')'.
      if (SO_NUMBER == lexeme)
	{
	  if (!nameMissingP)
	    {
	      parseError ("Missing ',' in memory specification");
	    }
	}
      else
	{
	  parseError ("Cannot recognize memory parameters");
	  return;
	}
    }

  // Look for the second number (the memory size)
  if (SO_NUMBER == lexeme)
    {
      byteSize = numberLval;		// Non-zero marks success
      scan ();				// Skip the number
    }
  else
    {
      // Error recovery time. In this case we just note the error and leave
      // the level above to sort out the problem.
      parseError ("Missing memory size");
    }
}	// parseMemoryParams ()


//-----------------------------------------------------------------------------
//! Parse the memoryValues rule

//! The rule is:

//!   memoryValues -> NUMBER | NUMBER ',' memoryValues

//! This is parsed in a loop, rather than recursing.

//! @param[in] memoryType  SO_BYTE if this is a byteMemoryClause, SO_WORD if
//!                        it is a wordMemoryClause.
//-----------------------------------------------------------------------------
void
SimProc::parseMemoryValues (ScannerObject  memoryType,
			    int            arraySize,
			    void          *memArray)
{
  uint8_t  *byteArray = (uint8_t *)memArray;
  uint32_t *wordArray = (uint32_t *)memArray;

  int  nextMem = 0;

#ifdef PARSE_DEBUG
  cout << "-- parseMemoryvalues" << endl;
#endif

  // Parse loop (must be at least one)
  do
    {
      if (SO_NUMBER == lexeme)
	{
	  // Save the element in the array. The value saved depends on the
	  // type of memory. Note that this is a host array, so we don't need
	  // to worry about endianness here.
	  if (nextMem < arraySize)
	    {
	      if (SO_BYTE == memoryType)
		{
		  byteArray[nextMem] = (uint32_t)numberLval;
		}
	      else
		{
		  wordArray[nextMem] = (uint32_t)numberLval;
		}
	    }
	  else
	    {
	      parseError ("Too many memory elements specified: %d", nextMem);
	    }

	  nextMem++;				// One more slot used
	  scan ();				// Skip the number
	}
      else
	{
	  // Error recovery time. If we don't have a number, then we scan
	  // until we find a NUMBER, BYTE, WORD or EOF
	  parseError ("Memory value expected");
	  skipNumberOrBlock ();

	  // Carry on if it was a NUMBER, give up otherwise
	  if (SO_NUMBER == lexeme)
	    {
	      scan ();				// Skip the number
	    }
	  else
	    {
	      return;
	    }
	}

      // If the next value is BYTE, WORD or EOF, we have come to the end.
      if ((SO_BYTE == lexeme) || (SO_WORD == lexeme) || (SO_EOF == lexeme))
	{
	  return;
	}

      // Look for the comma
      if (',' == lexeme)
	{
	  scan ();				// Skip the comma
	}
      else
	{
	  // Error recover time. Scan until we find a NUMBER, BYTE, WORD or
	  // EOF. Unless it's a number we'll then drop out of the loop.
	  parseError ("Comma missing between memory values");
	  skipNumberOrBlock ();
	}
    }
  while (SO_NUMBER == lexeme);

}	// parseMemoryValues ()


//-----------------------------------------------------------------------------
//! Skip to the start of a block after a parse error

//! A simple utility which calls the scanner until the next lexeme is SO_NAME,
//! REGISTERS, SO_BYTE, WORD or EOF.
//-----------------------------------------------------------------------------
void
SimProc::skipBlock ()
{
#ifdef PARSE_DEBUG
  cout << "-- skipBlock" << endl;
#endif

  while ((SO_NAME != lexeme) && (SO_REGISTERS != lexeme) &&
	 (SO_BYTE != lexeme) && (SO_WORD != lexeme) && (SO_EOF != lexeme))
    {
      scan ();				// Skip unknown stuff
    }
}	// skipBlock ()


//-----------------------------------------------------------------------------
//! Skip past a register closing brace.

//! A simple utility to avoid repetition in error checking register rules.
//-----------------------------------------------------------------------------
void
SimProc::skipBrace ()
{
#ifdef PARSE_DEBUG
  cout << "-- skipBrace" << endl;
#endif

  while ('}' != lexeme)
    {
      scan ();			// Skip unwanted symbol
    }

  scan ();			// Skip the '}'

}	// skipBrace ()


//-----------------------------------------------------------------------------
//! Skip to the start of a block or a new number

//! A simple utility to avoid repetition in error checking memory rules.
//-----------------------------------------------------------------------------
void
SimProc::skipNumberOrBlock ()
{
#ifdef PARSE_DEBUG
  cout << "-- skipNumberOrBlock" << endl;
#endif

  while ((SO_NUMBER != lexeme) && (SO_BYTE != lexeme) &&
	 (SO_WORD != lexeme) && (SO_EOF != lexeme))
    {
      scan ();				// Skip unknown stuff
    }
}	// skipNumberOrBlock ()


//-----------------------------------------------------------------------------
//! Handle a parser warning

//! This puts out a message and ups the warning count.

//! @param[in] format  Warning message, possibly with additional printf style
//!                    arguments.
//-----------------------------------------------------------------------------
void
SimProc::parseWarning (const char *format,
		       ...)
{
  va_list  args;

  va_start (args, format);
  parseMessage ("Warning: ", format, args);

  parseWarningCount++;

}	// parserWarning ()


//-----------------------------------------------------------------------------
//! Handle a parser error

//! @param[in] format  Error message, possibly with additional printf style
//!                    arguments.
//-----------------------------------------------------------------------------
void
SimProc::parseError (const char *format,
		     ...)
{
  va_list  args;

  va_start (args, format);
  parseMessage ("ERROR: ", format, args);

  parseErrorCount++;

}	// parserError ()


//-----------------------------------------------------------------------------
//! Handle a parser message

//! Print out a message with a preamble and followed by newline. A convenience
//! function for warning and error messages.

//! @param[in] preamble  Preamble to the message (Warning, ERROR, etc)
//! @param[in] format  Warning message, possibly with additional printf style
//!                    arguments.
//-----------------------------------------------------------------------------
void
SimProc::parseMessage (const char *preamble,
		       const char *format,
			...)
{
  char     mess[MAX_PARSER_ERR_SIZE];
  va_list  args;

  // Construct the message
  va_start (args, format);
  snprintf (mess, MAX_PARSER_ERR_SIZE, format, args);

  cerr << lineNumber << ": " << preamble << mess << endl;

}	// parserMessage ()


//-----------------------------------------------------------------------------
//! Scanner (lexical analyser)

//! Read the next lexeme. Associated text (for strings) is in stringLval and
//! value (for numbers) is in numberLval.

//! @return  The lexeme found
//-----------------------------------------------------------------------------
void
SimProc::scan ()
{
  //! The most recent word read in.
  char  word[LEXEME_MAX];

  // Get the next word from the input
  readWord ();

  // EOF has a zero length lval
  if (0 == strlen (lval))
    {
#ifdef SCANNER_DEBUG
      cout << "-- scanner SO_EOF" << endl;
#endif

      lexeme = SO_EOF;				// No more input
      return;
    }

  // Decide what lexeme we have.
  switch (lval[0])
    {
    case '"':				// String literal
#ifdef SCANNER_DEBUG
      cout << "-- scanner SO_STRING " << lval << endl;
#endif
      
      lexeme = SO_STRING;
      return;
      
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
#ifdef SCANNER_DEBUG
      cout << "-- scanner SO_NUMBER " << lval << endl;
#endif
      
      extractNumberLval ();			// Numeric literal
      lexeme = SO_NUMBER;
      return;
      
    case 'A': case 'B': case 'C': case 'D': case 'E':
    case 'F': case 'G': case 'H': case 'I': case 'J':
    case 'K': case 'L': case 'M': case 'N': case 'O':
    case 'P': case 'Q': case 'R': case 'S': case 'T':
    case 'U': case 'V': case 'W': case 'X': case 'Y':
    case 'Z': case 'a': case 'b': case 'c': case 'd':
    case 'e': case 'f': case 'g': case 'h': case 'i':
    case 'j': case 'k': case 'l': case 'm': case 'n':
    case 'o': case 'p': case 'q': case 'r': case 's':
    case 't': case 'u': case 'v': case 'w': case 'x':
    case 'y': case 'z':
      matchKeyword ();
      return;
      
    default:
      if (1 == strlen (lval))		// Single char token
	{
#ifdef SCANNER_DEBUG
	  cout << "-- scanner '" << lval[0] << "'" << endl;
#endif

	  lexeme = (ScannerObject)(lval[0]); 
	  return;
	}
      else
	{
	  // Not recognized
	  parseError ("Unrecognized token %s", lval);
	  lexeme = SO_UNKNOWN;
	}
    }

}	// scan ()


//-----------------------------------------------------------------------------
//! Extract the value associated with a numeric literal lexeme

//! The string scanned began with a digit. We accept the C/C++ prefixes for a
//! string (0x for hex, 0 for octal and extend then with 0h for hex, 0d for
//! decimal, 0o for octal and 0b for binary). The extracted value is placed in
//! the numberLval variable. The base indicator is case insensitive.

//! The string to convert is in ::lval.
//-----------------------------------------------------------------------------
void
SimProc::extractNumberLval ()
{
  if ('0' == lval[0])
    {
      // Deal with just plain zero.
      if (1 == strlen (lval))
	{
	  numberLval = 0;
	  return;
	}

      // We have a base
      switch (tolower (lval[1]))
	{
	case 'x':
	case 'h':
	  numberLval = str2base (2, 16);
	  return;

	case 'd':
	  numberLval = str2base (2, 10);
	  return;
	  
	case 'o':
	  numberLval = str2base (2, 8);
	  return;
	  
	case '0': case '1': case '2': case '3':
	case '4': case '5': case '6': case '7':
	  numberLval = str2base (1, 8);
	  return;
	  
	case 'b':
	  numberLval = str2base (2, 1);
	  return;
	  
	default:
	  // We don't recognize the number. This is a syntax error. Print out
	  // the error message, and carry on.
	  parseError ("Unrecognized non-decimal number %s", lval);
	}
    }
  else
    {
      // Plain decimal
      numberLval = str2base (0, 10);
    }
}	// extractNumberLval ()


//-----------------------------------------------------------------------------
//! Convert a string representation of a number to a particular base

//! Bases 2, 8, 10 and 16 are supported. Other bases may work, but this is not
//! guaranteed. Digits above 9 are case insensitive.

//! An invalid digit is a syntax error and will be reported, but we carry on
//! without converting the number.

//! The string to convert is in ::lval, starting at the offset specified.

//! @param[in] base      The base to use for conversion

//! @return  The value represented. Undefined if an error was encountered.
//-----------------------------------------------------------------------------
uint64_t
SimProc::str2base (int  offset,
		   int  base)
{
  uint64_t  valSoFar = 0;

  for (int  i = offset; i < strlen (lval); i++)
    {
      char  c = tolower (lval[i]);	// Current digit as char
      int   d;				// Value of current digit

      // Convert to value. -1 if it can't possibly be a digit.
      d = (('0' <= c) && (c <= '9')) ? c - '0' :
	  (('a' <= c) && (c <= 'z')) ? c - 'a' + 10 : -1;

      if ((-1 == d) || (d >= base))
	{
	  parseError ("Digit %d (%c) invalid in base %d number %s",
		      i, c, base, lval);
	  return  0;
	}
      else
	{
	  valSoFar = valSoFar * base + d;
	}
    }

  return  valSoFar;

}	// str2base ()


//-----------------------------------------------------------------------------
//! Work out which keyword we have

//! An unrecognized keyword is a syntax error and will be reported, but we
//! carry on and return.

//! There are a small enough number that a simple linear search is fast
//! enough.

//! The word to be recognized is in ::lval. The resulting keyword is placed in
//! ::lexeme.
//-----------------------------------------------------------------------------
void
SimProc::matchKeyword ()
{
  if (0 == strcasecmp ("name", lval))
    {
#ifdef SCANNER_DEBUG
      cout << "-- scanner SO_NAME" << endl;
#endif
      lexeme = SO_NAME;
    }
  else if (0 == strcasecmp ("little", lval))
    {
#ifdef SCANNER_DEBUG
      cout << "-- scanner SO_LITTLE" << endl;
#endif
      lexeme = SO_LITTLE;
    }
  else if (0 == strcasecmp ("big", lval))
    {
#ifdef SCANNER_DEBUG
      cout << "-- scanner SO_BIG" << endl;
#endif
      lexeme = SO_BIG;
    }
  else if (0 == strcasecmp ("endian", lval))
    {
#ifdef SCANNER_DEBUG
      cout << "-- scanner SO_ENDIAN" << endl;
#endif
      lexeme = SO_ENDIAN;
    }
  else if (0 == strcasecmp ("registers", lval))
    {
#ifdef SCANNER_DEBUG
      cout << "-- scanner SO_REGISTERS" << endl;
#endif
      lexeme = SO_REGISTERS;
    }
  else if (0 == strcasecmp ("byte", lval))
    {
#ifdef SCANNER_DEBUG
      cout << "-- scanner SO_BYTE" << endl;
#endif
      lexeme = SO_BYTE;
    }
  else if (0 == strcasecmp ("word", lval))
    {
#ifdef SCANNER_DEBUG
      cout << "-- scanner SO_WORD" << endl;
#endif
      lexeme = SO_WORD;
    }
  else if (0 == strcasecmp ("memory", lval))
    {
#ifdef SCANNER_DEBUG
      cout << "-- scanner SO_MEMORY" << endl;
#endif
      lexeme = SO_MEMORY;
    }
  else
    {
      // Not recognized
      parseError ("Unrecognized keyword %s", lval);
      lexeme = SO_UNKNOWN;
    }
}	// matchKeyword ()


//-----------------------------------------------------------------------------
//! Read the next word from the input.

//! There is some intelligence
//! - we recognize // as marking a comment to the  end of line.
//! - we recognize strings as being bounded by " or ' at each end
//! - everything else is bounded by whitespace (space, tab or newline)

//! ::ch holds the next character to consider.
//! ::lval holds the word we are building up.
//-----------------------------------------------------------------------------
void
SimProc::readWord ()
{
  enum
  {
    LEADING_SPACE,
    COMMENT_FIRST_CHAR,
    IN_COMMENT,
    STRING_STARTING,
    IN_STRING,
    NORMAL
  } state = LEADING_SPACE;

  int   i          = 0;		// Index into the word array
  char  strStartCh;		// The char starting a string (' or ")

  // Read chars until we have a complete word
  while (true)
    {
      // EOF is always the end of things. However it should not be the end of
      // a string! That is a syntax error which should be reported
      if (-1 == ch)
	{
	  if (IN_STRING == state)
	    {
	      parseError ("EOF encountered in mid-string");
	    }

	  lval[i] = '\0';
	  return;
	}

      // Advance the state
      switch (state)
	{
	case LEADING_SPACE:		// Looking for leading white space
	  if ('/' == ch)
	    {
	      state = COMMENT_FIRST_CHAR;
	    }
	  else if ((('\'' == ch) || ('"' == ch)) &&
		   (0 == i))
	    {
	      state = STRING_STARTING;	// Start of new string
	    }
	  else if (!isspace (ch))
	    {
	      state = NORMAL;		// Reading a token
	    }

	  break;
	  
	case COMMENT_FIRST_CHAR:	// We've seen the first '/'
	  if ('/' == ch)
	    {
	      // Have two '/' chars, so this is a comment to EOL
	      state = IN_COMMENT;
	    }
	  else
	    {
	      // Wasn't a comment after all. Put the char back and treat the
	      // previous char as a single '/'.
	      fh->putback (ch);
	      ch = '/';
	      state = NORMAL;
	    }

	  break;

	case IN_COMMENT:
	  if ('\n' == ch)
	    {
	      state = LEADING_SPACE;	// Newline ends a comment
	    }

	  break;

	case STRING_STARTING:		// Just had opening quote.
	  state = IN_STRING;
	  break;

	case IN_STRING:
	case NORMAL:
	  break;
	}

      // Now process the char according to the state we are in.
      switch (state)
	{
	case LEADING_SPACE:		// Just get the next char
	case COMMENT_FIRST_CHAR:
	case IN_COMMENT:
	  readChar ();			// Get the next character
	  break;

	case STRING_STARTING:		// String just started
	  strStartCh = ch;		// Record what we must match
	  lval[0]    = '"';		// Strings always saved with "
	  i = 1;
	  readChar ();			// Get the next character
	  break;

	case IN_STRING:			// We've started a string
	  if (ch == strStartCh)
	    {
	      if ((i + 1) >= LEXEME_MAX)
		{
		  lval[LEXEME_MAX - 1] = '\0';	// For safety
		  parseError ("Out of space (%d chars) to read string %s",
			      LEXEME_MAX, lval);
		  return;
		}

	      lval[i]     = '"';	// Strings always saved with "
	      lval[i + 1] = '\0';	// Terminate it

	      readChar ();		// Get the next character
	      return;			// All done
	    }
	  else
	    {
	      if (i >= LEXEME_MAX)
		{
		  lval[LEXEME_MAX - 1] = '\0';	// For safety
		  parseError ("Out of space (%d chars) to read string %s",
			      LEXEME_MAX, lval);
		  return;
		}

	      lval[i] = ch;		// Save the ch
	      i++;
	      readChar ();		// Get the next character
	    }

	  break;

	case NORMAL:			// Keyword or number
	  // Keywords or numbers can only start with or contain digits,
	  // letters, '.' or '_'. Anything else is a single char keyword.
	  if ((0 == i) &&
	      !isalnum (ch) && ('.' != ch) && ('_' != ch))
	    {
	      lval[0] = ch;
	      lval[1] = '\0';
	      readChar ();		// Get the next character
	      return;
	    }

	  // Otherwise multi-char are terminated by anything that is not a a
	  // digit, letter, '.' or '_'.
	  if (i >= LEXEME_MAX)
	    {
	      lval[LEXEME_MAX - 1] = '\0';	// For safety
	      parseError ("Out of space (%d chars) to read token %s",
			  LEXEME_MAX, lval);
	      return;
	    }

	  if (!isalnum (ch) && ('.' != ch) && ('_' != ch))
	    {
	      lval[i] = '\0';
	      return;
	    }

	  lval[i] = ch;			// Save the char
	  i++;
	  readChar ();			// Get the next character

	  break;
	}
    }
}	// readWord ()


//-----------------------------------------------------------------------------
//! Update the next character to be analysed.

//! Updates the line number count
//-----------------------------------------------------------------------------
void
SimProc::readChar ()
{
  char  c;				// Character size I/O

  prevCh = ch;

  if (-1 != nextCh)
    {
      ch     = nextCh;			// We had an unreadch
      nextCh = -1;
    }

  if (fh->get (c))
    {
      ch = c;				// Char to int
    }
  else
    {
      ch = -1;				// Marks EOF
    }

  if ('\n' == ch)			// Up the line number count
    {
      lineNumber++;
    }
}	// readChar ()

//-----------------------------------------------------------------------------
//! Revert the next character to be analysed.

//! Cannot affect the line number count.

//! @note This can only be called once between calls to readChar (). It is an
//!       error if readChar () has yet to be called (when ::prevCh will be -1).
//-----------------------------------------------------------------------------
void
SimProc::unreadChar ()
{
  if (-1 == prevCh)			// Marks start of file
    {
      parseError ("Attempt to unread at start of file");
    }
  else
    {
      nextCh = ch;			// Save to use again
      ch     = prevCh;			// Restore old ch
    }

  if ('\n' == nextCh)			// Reduce the line number count
    {
      lineNumber--;
    }
}	// unreadChar ()


// Local Variables:
// mode: C++
// c-file-style: "gnu"
// show-trailing-whitespace: t
// End:
