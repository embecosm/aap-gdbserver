// ----------------------------------------------------------------------------
// Main program.

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


#include "config.h"

#include <cstdlib>
#include <cstring>
#include <getopt.h>
#include <iostream>
#include <unistd.h>

#include "AapSim.h"
#include "GdbServer.h"


using std::atoi;
using std::cout;
using std::cerr;
using std::endl;
using std::string;


// Things to make getopt work
extern char *optarg;
extern int optind, opterr, optopt;


//! Main routine.

//! Process the arguments, instantiate the simulator and server, run until
//! exit.

//! @param[in] argc  Number of args
//! @param[in] argv  The args
//! @return  The return code of the program.

int
main (int   argc,
      char *argv[] )
{
  // Argument handling.  We need to be given an optional debug flag and an
  // optional port to listen on.  There are also the standard help and version
  // flags.

  int debugLevel = 0;
  int port = 51000;
  const string usage (
    "Usage: aap-sim [-d | --debug [<level>]  [-p | --port <port> [--version] [-h | --help]");

  while (true)
    {
      int c;
      int longOptind = 0;
      static struct option longOptions[] = {
	{"debug",   optional_argument, 0,  'd' },
	{"port",    optional_argument, 0,  'p' },
	{"help",    no_argument,       0,  'h' },
	{"version", no_argument,       0,  'v' },
	{0,         0,                 0,  0 }
      };

      if ((c = getopt_long (argc, argv, "d::p:h", longOptions, &longOptind))
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

	case 'p':				// Port

	  port = atoi (optarg);
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

	default:				// Oops

	  cerr << "ERROR: getopt_long returned character code " << c << endl;
	}
    }

  // No positional args
  if ((argc - optind) != 0)
    {
      cerr << usage << endl;
      return  EXIT_FAILURE;
    }

  // The simulator and server.  If we return we hit some sort of problem

  AapSim *sim = new AapSim ();
  GdbServer *gdbServer = new GdbServer (port, sim, debugLevel);

  gdbServer->rspServer ();

  delete  gdbServer;
  delete  sim;

  return EXIT_FAILURE;			// If we return it's a failure!

}	/* sc_main() */


// Local Variables:
// mode: C++
// c-file-style: "gnu"
// show-trailing-whitespace: t
// End:
