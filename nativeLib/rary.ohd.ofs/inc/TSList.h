//------------------------------------------------------------------------------
// TSList - Container for as list of TS objects.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 01 Apr 1998	Daniel K. Weiler, Riverside Technology, inc
//					Created initial version.
// 03 Jun 1998	DKW		Overloaded to take input or output flags
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef TSList_INCLUDED
#define TSList_INCLUDED

#include "resj/TS.h"

class TSList 
{
public:

	static int	addTSToList( TS* );	
					// Adds a TS pointer to the 
					// maintained list.

	static int	addTSToList( TS*, char* );	
					// Adds a TS pointer to the 
					// maintained list and interprets
					// "input", "in", "output", or
					// "out" strings into flags
	static void 	DeleteTSList();		
					// frees the memory associated
					// with the _ts_list.

	static int	getNumTS();	// returns _num_ts.

	static TS*	getTSFromList( char* );
					// Get the TS pointer from _ts_list
					// for the char* identifier or alias

	static TS	**_ts_list;	// List of TS object pointers.

	static int	_num_ts;	// number of TS pointers
					// in _ts_list.
	static int* 	_io_flags;

};

#endif
