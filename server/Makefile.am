# Makefile.am -- Program automake configuration file
#
# Copyright (C) 2009, 2015 Embecosm Limited
#
# Contributor Jeremy Bennett <jeremy.bennett@embecosm.com>
#
# This file is part of the Embecosm AAP GDB server and simulator.
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3 of the License, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
# more details.
#
# You should have received a copy of the GNU General Public License along
# with this program.  If not, see <http://www.gnu.org/licenses/>.

bin_PROGRAMS = aap-server

aap_server_SOURCES = GdbServer.cpp     \
                     GdbServer.h       \
                     MpHash.cpp        \
                     MpHash.h          \
                     RspConnection.cpp \
                     RspConnection.h   \
                     RspPacket.cpp     \
                     RspPacket.h       \
                     ServerMain.cpp    \
                     Utils.cpp         \
                     Utils.h

aap_server_LDADD = libaapsim.la
