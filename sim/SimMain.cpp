// Main program.

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

#include <iostream>
#include <cstdlib>
#include <unistd.h>

#include "AapSim.h"


using std::atoi;
using std::cerr;
using std::cout;
using std::endl;


//! Load an elf file

//! @todo For now this is a dummy routine.  We'll use libelf to do this in due
//!       course.

//! @param[in] sim      The simulator instance
//! @param[in] objfile  The objectfile

static void
loadElf (AapSim      sim,
	 const char *objfile)
{
}	// loadElf ()


//! Main routine.

//! Process the arguments, instantiate the simulator, load the program, run
//! until exit, return the result.

//! @param[in] argc  Number of args
//! @param[in] argv  The args
//! @return  The return code of the program.

int
main (int   argc,
      char *argv[] )
{
  // Argument handling.  We need to be given an optional debug flag followed
  // by an object file.  There are also the standard help and version flags.

  int debugLevel = 0;
  const string usage (
    "Usage: aap-sim [-d | --debug [<level>] [--version] [-h | --help] <file>");

  while (true)
    {
      int c;
      int longOptind = 0;
      static struct option longOptions[] = {
	{"debug",   optional_argument, 0,  'd' },
	{"help",    no_argument,       0,  'h' },
	{"version", no_argument,       0,  'v' },
	{0,         0,                 0,  0 }
      };

      if ((c = getopt_long (argc, argv, "d::h", longOptions, &longOptind))
	  == -1)
	break;

      switch (c)
	{
	case 'd':				// Debug level

	  if (optarg)
	    debugLevel = atoi (optarg);
	  else
	    debugLevel = 3;			// Default value

	  break;

	case 'v':				// Version

	  cout << "Unknown version" << endl;
	  return  EXIT_SUCCESS;
	  break;

	case 'h':				// Help

	  cout << usage << endl;
	  return  EXIT_SUCCESS;

	case '?':				// Bad arg or arg value
	case ':':

	  cerr << usage << endl;
	  return  EXIT_FAILURE;

	default:				// Oops!

	  cerr << "ERROR: getopt_long returned character code " << c << endl;
	}
    }

  // 1 positional arg
  if ((argc - optind) != 1)
    {
      cerr << usage << endl;
      return  EXIT_FAILURE;
    }

  const char *objFile  = argv[optind];

  // Instantiate the simulator, load the object file, and start running.
  AapSim sim ();
  loadElf (sim, objfile);

  // Run until we get the result (which is in R0)
  while (AapSim::Res::EXIT != sim.run ())
    continue;

  return  (int) sim.reg (0);

}	/* sc_main() */


// Local Variables:
// mode: C++
// c-file-style: "gnu"
// show-trailing-whitespace: t
// End:
