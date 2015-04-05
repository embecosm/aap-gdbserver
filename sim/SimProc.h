// ----------------------------------------------------------------------------
// Simulated Processor: definition

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


#ifndef SIM_PROC__H
#define SIM_PROC__H

#include <stdint.h>
#include <cstdarg>
#include <iostream>
#include <fstream>

#include "Register.h"
#include "MemoryBlock.h"


//-----------------------------------------------------------------------------
//! Representation of a simulated processor

//! A processor consists of a bank of registers and one or more blocks of
//! memory.

//! The bulk of this class is the LR(1) recursive descent parser for the
//! processor config file from which the simulated processor is initialized.
//-----------------------------------------------------------------------------
class SimProc
{
public:

  // Constructor and destructor
  SimProc (bool _traceOnP);
  ~SimProc ();

  // Initializer
  bool  init (const char *filename);

  // Various public accessor functions
  bool  isLittleEndian ();
  bool  isTraceOn ();
  int   getNumRegs ();
  bool  isValidReg (int  regNum);
  bool  getRegSize (int  regNum,
		    int &size);
  bool  readReg (int       regNum,
		 uint64_t &value);
  bool  writeReg (int       regNum,
		  uint64_t  value);
  bool  isValidAddr (uint32_t  addr);
  bool  readMem (uint32_t  addr,
		 uint8_t  &value);
  bool  writeMem (uint32_t  addr,
		  uint8_t   value);


private:

  //! All the lexemes the scanner can find. Single chars are themselves, so we
  //! start at 256.
  enum ScannerObject
    {
      SO_NAME      = 257,		// 'NAME' keyword
      SO_LITTLE    = 258,		// 'LITTLE' keyword
      SO_BIG       = 259,		// 'BIG' keyword
      SO_ENDIAN    = 260,		// 'ENDIAN' keyword
      SO_REGISTERS = 261,		// 'REGISTERS' keyword
      SO_BYTE      = 262,		// 'BYTE' keyword
      SO_WORD      = 263,		// 'WORD' keyword
      SO_MEMORY    = 264,		// 'MEMORY' keyword
      SO_STRING    = 265,		// A string
      SO_NUMBER    = 266,		// A number
      SO_EOF       = 267,		// End of file
      SO_UNKNOWN   = 268		// When we don't recognize the keyword
    };

  //! Are we tracing?
  bool  traceOnP;

  //! Our name
  char *name;

  //! Number of registers
  int  numRegs;

  //! The register array
  Register *regArray;

  //! The memory blocks
  MemoryBlock *memList;

  //! Endianness flag
  bool  isLittleEndianP;

  //! The next character to consider. -1 is EOF.
  int  ch;

  //! The previous ch considered. -1 is start of file.
  int  prevCh;

  //! The saved ch if we have just done an unreadChar (). Otherwise -1.
  int  nextCh;

  //! The line number of ch
  int  lineNumber;

  //! Largest size of a lexeme (including EOS character)
  static const int  LEXEME_MAX = 256;

  //! Maximum size of a parser error message.
  static const int  MAX_PARSER_ERR_SIZE = 256;

  //! The next lexeme to consider
  ScannerObject  lexeme;

  //! Text associated with a lexeme
  char  lval[LEXEME_MAX];

  //! Value associated with a number lexeme
  uint64_t  numberLval;

  //! The number of warnings found while parsing
  int  parseWarningCount;

  //! The number of errors found while parsing
  int  parseErrorCount;

  //! File handle for the file being parsed
  std::ifstream *fh;

  // Recursive descent parsing functions
  void  parse ();

  void  parseDescription ();
  void  parseNameClause ();
  void  parseNameParams ();
  void  parseEndianness ();
  void  parseRegisterClause ();
  void  parseRegisterList ();
  void  parseRegister (int  regNum);
  void  parseMemoryClauseList ();
  void  parseMemoryClause ();
  void  parseSpecificMemoryClause (ScannerObject  memoryType);
  void  parseMemoryParams (uint32_t &baseAddr,
			   uint32_t &byteSize);
  void  parseMemoryValues (ScannerObject  memoryType,
			   int            arraySize,
			   void          *memArray);

  // Parser support functions
  void  skipBlock ();
  void  skipBrace ();
  void  skipNumberOrBlock ();

  void  parseWarning (const char *format,
		      ...);
  void  parseError (const char *format,
		    ...);
  void  parseMessage (const char *preamble,
		      const char *format,
		      ...);

  // Scanner (lexical analyser) for the processor description
  void  scan ();

  // Extract the value associated with a numeric literal.
  void  extractNumberLval ();

  // Convert a string representation of a number
  uint64_t  str2base (int  offset,
		      int  base);

  // Determine which keyword is represented by a particular string
  void  matchKeyword ();

  // Read a single word from the input
  void  readWord ();

  // Read a character from the input
  void  readChar ();

  // Revert a character from the input
  void  unreadChar ();

};

#endif	// SIM_PROC__H


// Local Variables:
// mode: C++
// c-file-style: "gnu"
// show-trailing-whitespace: t
// End:
