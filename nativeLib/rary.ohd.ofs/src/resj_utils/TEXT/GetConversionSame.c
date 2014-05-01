/*------------------------------------------------------------------------------
** GetConversionSame - get conversion factors for NWS data types assuming
**			    that data types are within the same group
**------------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
**------------------------------------------------------------------------------
** returns:	0 if success
**		1 if unable to read DATAUNIT file
**		2 if not enough space to read all units
**		3 if first set of units is unknown
**		4 if second set of units is unknown
**		5 if units are not of the same base type (i.e. if not both
**		  lengths)
**------------------------------------------------------------------------------
** Notes:	(1)	This routine depends on on the values in the DATAUNIT
**			file orignally supplied by the NWS.  Typical contents
**			for this file are as follows:

*   11/8/90   'HYD.RFS.SYSTEM(DATAUNIT)'                                00000010
*                                                                       00000020
* LENGTH                                                                00000230
L    BASE MM    MILLIMETER                          1 1.        .       00000240
L    OTHR CM    CENTIMETER                          2 10.       .       00000250
L    OTHR M     METER                               2 1000.     .       00000260
L    OTHR KM    KILOMETER                           1 1000000.  .       00000270
L    OTHR IN    INCH                                2 25.4      .       00000280
L    OTHR FT    FOOT                                2 304.8     .       00000290
L    OTHR MI    MILE (STATUTE)                      1 1609344.  .       00000300
L    OTHR NM    MILE (NAUTICAL)                     1 1853248.  .       00000310
* TEMPERATURE                                                           00000760
TEMP BASE DEGC  DEGREE CENTIGRADE                   1 1.        0.000   00000770
TEMP OTHR DEGK  DEGREE KELVIN                       1 1.        -273.   00000780
TEMP OTHR DEGF  DEGREE FAHRENHEIT                   1 .555556   -17.8   00000790
TEMP OTHR DEGR  DEGREE RANKINE                      1 .555556   -273.   00000800
* END DATAUNIT                                                          00000860

**------------------------------------------------------------------------------
** History:
**
** 06 Sep 1996	Steven A. Malers, RTi	Split code out of HMTS.c file.
** 07 Oct 1996	SAM, RTi		Add <string.h> to prototyp string
**					functions.
**------------------------------------------------------------------------------
** Variables	I/O	Description
**
** afac		L	All values of "add".
** add		O	Factor to add to value to be converted.
** count	L	Number of units in file.
** dfile	L	Path to DATAUNIT file.
** fp		L	Pointer to input file.
** from		L	Position in arrays for "u1".
** group0	L	The current group that is being read.
** group	L	The group for the current units line.
** i		L	Loop counter.
** iend		L	Limiting value for "i".
** key		L	Key to lock file for DATAUNIT file.
** mfac		L	All values of "mult".
** mult		O	Factor to multiply value to be converted.
** name		L	Units names.
** string	L	Character string used for reading lines of file.
** sysdir	L	Location of NWSRFS system files.
** tfac1, tfac2	L	Used in calculating "add" for temperatures.
** to		L	Position in arrays for "u2".
** type		L	Type of units (e.g. "L", "L3", etc.) - the last
**			character is either "b" for base or "o" for other.
** u1		I	Source units.
** u2		I	Destination units.
**------------------------------------------------------------------------------
*/

#include <string.h>

#include "ResJ.h"

#ifndef MAXUNIT
#define MAXUNIT 100
#endif

int GetConversionSame ( char *u1, char *u2, float *mult, float *add )
{	FILE	*fp;
	float	afac[MAXUNIT], mfac[MAXUNIT], tfac1, tfac2;
	char	dfile[256], group[5], group0[5], key[256],
		name[MAXUNIT][5], routine[] = "GetConversionSame", string[128],
		sysdir[128], type[MAXUNIT][6];
	int	count = 0, from, i, iend, to;

	*mult	= 1.0;
	*add	= 0.0;
	if ( !strcmp(u1,u2) ) {
		/*
		** No conversion necessary...
		*/
		return STATUS_SUCCESS;
	}
	/*
	** Get the path to the DATAUNIT file...
	*/
	if ( GetDef("rfs_sys_dir",sysdir) ) {
		PrintWarning ( 2, routine,
		"Unable to get definition for \"rfs_sys_dir\"" );
		return STATUS_FAILURE;
	}
	sprintf ( dfile, "%s/DATAUNIT", sysdir );
	if ( !(fp = fopen(dfile,"r")) )	return STATUS_FAILURE;
	while ( fgets(string,128,fp) ) {
		if ( string[0] == '*' ) {
			/*
			** A comment line...
			*/
			if ( !strncmp(string,"* END",5) )
				break;
		}
		else {	/*
			** A line with conversion factors...
			*/
			sscanf ( string, "%s", group );
			sscanf ( &string[10], "%s", name[count] );
			sscanf ( &string[54], "%f", &mfac[count] );
			if ( !strcmp(group,"TEMP") )
				sscanf ( &string[64], "%f", &afac[count] );
			else	afac[count] = 0.0;
			if ( count == 0 ) {
				/*
				** First item read...skip the check...
				*/
				++count;
				strcpy ( group0, group );
				continue;
			}
			else	++count;
			if ( strcmp(group,group0) ) {
				/*
				** The group for the current line is different
				** from "group0".  See if the units that we want
				** to convert are in "group0"...
				*/
				iend	= count - 1;
				from	= -1;
				to	= -1;
				for ( i = 0; i < iend; i++ ) {
					/*
					** DON'T use "else" because it is
					** possible that "u1" and "u2" are the
					** same...
					*/
					if ( !strcmp(u1,name[i]) )
						from = i;
					if ( !strcmp(u2,name[i]) )
						to = i;
				}
				if ( (from >= 0) && (to >= 0) ) {
					/*
					** Calculate conversion factors...
					*/
					*mult	= mfac[from]/mfac[to];
					if ( !strcmp(group0,"TEMP") ) {
						/*
						** For temperatures, need to get
						** an offset (add) value also...
						*/
						tfac1 = afac[from];
						tfac2 = -1.0*afac[to]/mfac[to];
						*add  = tfac2 +tfac1/mfac[to];
					}
					else	*add = 0.0;

					if( fp ){
						fclose( fp );
					}
					return STATUS_SUCCESS;
				}
				else if ( from >= 0 ) {
					/*
					** Only "u1" can be found...
					*/
					if( fp ){
						fclose( fp );
					}
					return 4;
				}
				else if ( to >= 0 ) {
					/*
					** Only "u2" can be found...
					*/

					if( fp ){
						fclose( fp );
					}

					return 3;
				}
				else {	/*
					** Save the current entry as the first
					** entry of the next group...
					*/
					strcpy ( group0, group );
					strcpy ( name[0], name[count - 1] );
					afac[0]	= afac[count - 1];
					mfac[0]	= mfac[count - 1];
					count	= 1;
				}
			}
		}
		if ( count == MAXUNIT ) {
			fclose ( fp );
			return 2;
		}
	}
	fclose ( fp );
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/GetConversionSame.c,v $";
 static char rcs_id2[] = "$Id: GetConversionSame.c,v 1.1 1999/02/18 15:16:42 dws Exp $";}
/*  ===================================================  */

}
