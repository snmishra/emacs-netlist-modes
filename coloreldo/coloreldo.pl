#!/usr/bin/perl
#
# coloreldo.pl 0.1
# (based on colormake.pl 0.3)
#
# Copyright: (C) 2001, Emmanuel Rouat <emmanuel.rouat@st.com>
# Copyright: (C) 1999, Bjarni R. Einarsson <bre@netverjar.is>
#                      http://bre.klaki.net/
# 
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#

# Some useful color codes, see end of file for more.
#
$col_norm =	    "\033[00m";
$col_brighten =     "\033[01m";
$col_underline =    "\033[04m";
$col_blink = 	    "\033[05m";
$col_ltgray =       "\033[37m";
$col_purple =       "\033[35m";
$col_green =        "\033[32m";
$col_red =          "\033[31m";
$col_cyan =         "\033[36m";
$col_brown =        "\033[33m";
$col_background =   "\033[07m";

# Customize colors here...
#
$col_default =      $col_ltgray;
$col_eldo =         $col_cyan;
$col_result =       $col_cyan . $col_brighten;
$col_simulation =   $col_purple;
$col_status =       $col_purple . $col_brighten ;
$col_warning =      $col_green;
$col_error =        $col_red . $col_brighten;



$in = 'unknown';
while (<>)
{
	$orgline = $thisline = $_;

	# Warning:
	if ($thisline =~ s/^(Warning)/$col_warning$1/x)
	{
		$in = 'warning';
	}
	# Error:
	if ($thisline =~ s/^(ERROR)/$col_error$1/x)
	{
		$in = 'error';
	}
	# DC voltages:
	if ($thisline =~ s/^(DC:)/$col_result$1/x)
	{
		$in = 'DC';
	}
	elsif ($thisline =~ s/^(\*\*\*.*)$/$col_eldo$1$col_norm/) 
	{
		$in = 'eldo';
	}
	elsif ($thisline =~ s/^(\.\.\..*)$/$col_simulation$1$col_norm/) 
	{
		$in = 'simulation';
	}
	elsif ($thisline =~ s/^(---*)$/$col_simulation$1$col_norm/) 
	{
		$in = 'simulation';
	}
	elsif ($in eq 'simulation')
	{
		$thisline =~ s|^(Performing *)(\d+)|$col_status$1$col_default$2|;
		$thisline =~ s|^([^:]+)|$col_brown$1$col_default|;
	}
	elsif ($in eq 'DC')
	{
		$thisline =~ s|^([^ \t]+)|$col_result$1$col_default|;
	}	
	elsif ($in eq 'eldo')
	{
		$thisline =~ s|^([^:=]+)|$col_result$1$col_default|;
	}	
	if ($thisline !~ /^\s+/) 
	{
		print $col_norm, $col_default;
	}	
	print $thisline;	
}

print $col_norm;



