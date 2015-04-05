// ----------------------------------------------------------------------------
// Main program.

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
#include <cstdlib>
#include <cstring>

#include "RspProxyMain.h"
#include "SimProc.h"
#include "GdbServer.h"


using std::atoi;
using std::cerr;
using std::endl;
using std::strcmp;


int
main (int   argc,
      char *argv[] )
{
  // Argument handling. There are two arguments, the config file and the RSP
  // port. Both must be specified.
  char *configFile;
  int   port;
  bool  traceOnP;

  if ((argc < 3) || (argc > 4))
    {
      cerr << "Usage: rsp-proxy <config-file> <rsp-port> [trace]" << endl;
    }

  configFile = argv[1];
  port       = atoi (argv[2]);

  if (4 == argc)
    {
      traceOnP = (0 == strcmp ("trace", argv[3]));
    }
  else
    {
      traceOnP = false;
    }

  // The simulator. Give up if initialization fails
  SimProc *cpu = new SimProc (traceOnP);

  if(!cpu->init (configFile))
    {
      exit (127);
    }

  // The RSP server
  GdbServer *gdbServer = new GdbServer (port, cpu);

  // Run the GDB server. If we return we hit some sort of problem.

  gdbServer->rspServer ();

  // Free memory
  delete  gdbServer;
  delete  cpu;

  return 255;			// If we return it's a failure!

}	/* sc_main() */


// Local Variables:
// mode: C++
// c-file-style: "gnu"
// show-trailing-whitespace: t
// End:
