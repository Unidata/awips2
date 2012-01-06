/*------------------------------------------------------------------------------
** GetConversion - get conversion factors for NWS data types
**------------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
**------------------------------------------------------------------------------
** Notes:	(1)	This routine depends on on the values in the DATAUNIT
**			file orignally supplied by the NWS.  Typical contents
**			for this file are shown in the notes for
**			GetConversionSame.
**		(2)	This routine caches conversion factors.  This allows
**			lookups to be faster.  A program will typically only
**			use a few conversions so some simple caching improves
**			performance and allows this routine to be called without
**			concern for significant performance degradation.  Note
**			that the caching currently implemented still requires
**			that the units file be read for new conversion
**			combinations.  It may be desirable at some point to
**			store the entire conversions file in memory but this
**			is not currently done.
**------------------------------------------------------------------------------
** Returns:	0 if success
**		3 if first set of units is unknown or of wrong type
**		4 if second set of units is unknown or of wrong type
**		5 if auxiliary data units is unknown or of wrong type
**------------------------------------------------------------------------------
** History:
**
** 15 Jun 1996	Steven A. Malers, RTi	Add caching based on successful lookups
**					of conversion factors.
** 06 Sep 1996	SAM, RTi		Split out of the HMTS.c file.
**------------------------------------------------------------------------------
** Variable	I/O	Description
**
** add		O	Factor to add to value to be converted.
** afac		L	Internal addition value (used for intermediate
**			conversions).
** area		L	Area to be used in calculations.
** atype	L	Data group for "aunits".
** aunits	I	Units for "aux".
** aux		I	Additional data needed for a conversion (see notes).
** cache	L	Cached units conversion data.
** caching	L	Indicates if we are caching units conversions (right
**			now this is the only option).
** i		L	Loop counter for cached conversions.
** message	L	Message string.
** mfac		L	Internal multiplication factor (used for intermediate
**			conversions).
** ncache	L	Number of "cache".
** u1		I	Source units.
** u1type	L	Data group (dimension) for "u1".
** u2		I	Destination units.
** u2type	I	Data group (dimension) for "u2".
** vol		L	Volume used in calculations.
**------------------------------------------------------------------------------
*/

#include "ResJ.h"

#define MAX_UNITS_CACHE	100	/* number of units conversions to cache */

int GetConversion (	char *u1, char *u2, float *mult,
			float *add, float *aux, char *aunits )
{	int	atype, caching = 1, i, u1type, u2type;
	float	area, mfac, afac, vol;
	char	message[256], routine[] = "GetConversion";
	static struct {
		char	units_from[16],		/* units converting from */
			units_to[16];		/* units converting to */
		float	add,			/* add factor to convert */
			mult;			/* mult factor to convert */
	} cache[MAX_UNITS_CACHE];
	static int	ncache = 0;

	mfac	= 1.0;
	afac	= 0.0;

	/*
	** First thing we do is see if the units are the same.  If so, we are
	** done...
	*/

	if ( !strcmp(u1,u2) ) {
		*mult	= 1.0;
		*add	= 0.0;
		return STATUS_SUCCESS;
	}

	/*
	** Next see if we have cached the conversion.  This only works for
	** straight conversions.
	*/

	if ( caching && !*aunits) {
		for ( i = 0; i < ncache; i++ ) {
			if ( !strcmp(u1,cache[i].units_from) ) {
				/*
				** "From" units match.  Check the "to" units...
				*/
				if ( !strcmp(u2,cache[i].units_to) ) {
					/*
					** "To" units match.  Return the saved
					** factors...
					*/
					*add	= cache[i].add;
					*mult	= cache[i].mult;
					return STATUS_SUCCESS;
				}
			}
		}
	}

	/*
	** Now do a full search...
	*/
	if ( GetUnitsType ( u1, &u1type ) ) {
		sprintf ( message,
		"Unable to get units type for \"%s\"", u1 );
		PrintWarning ( 1, routine, message );
		return STATUS_FAILURE;
	}
	if ( GetUnitsType ( u2, &u2type ) ) {
		sprintf ( message,
		"Unable to get units type for \"%s\"", u2 );
		PrintWarning ( 1, routine, message );
		return STATUS_FAILURE;
	}
	if ( u1type == u2type ) {
		if ( GetConversionSame (u1, u2, mult, add) ) {
			sprintf ( message,
			"Unable to get units for \"%s\" to \"%s\"", u1, u2 );
			PrintWarning ( 1, routine, message );
			return STATUS_FAILURE;
		}
		else {	/*
			** If we are caching conversion factors, save the
			** factor...
			*/
			if ( ncache == MAX_UNITS_CACHE ) {
				/*
				** We can't add any more...
				*/
				sprintf ( message,
				"Maximum units cache size (%d) reached.  Unable to cache \"%s\" to \"%s\"",
				MAX_UNITS_CACHE, u1, u2 );
				PrintWarning ( 2, routine, message );
				PrintWarning ( 2, routine,
				"Units conversion lookup performance will be degraded" );
			}
			else {	/*
				** Add it...
				*/
				strcpy ( cache[ncache].units_from, u1 );
				strcpy ( cache[ncache].units_to, u2 );
				cache[ncache].add	= *add;
				cache[ncache].mult	= *mult;
				++ncache;
			}
			return STATUS_SUCCESS;
		}
	}
	/*
	** Else, units groups are of different types - need to do more than
	** one operation...
	*/
	else if	(((u1type == UNIT_VOLUME) && (u2type == UNIT_LENGTH))||
		((u1type==UNIT_DISCHARGE)&&(u2type == UNIT_LENGTH))) {
		/*
		** 1) Convert volume to M3, 2) convert area to M2, 3) divide
		** volume by area, 4) convert depth to correct units...
		**
		** If dealing with discharge, ignore time (for now)...
		*/
		if ( u1type == UNIT_VOLUME ) {
			if ( GetConversionSame ( u1, "M3", &mfac, &afac ) )
				return 3;
			*mult	= mfac;
		}
		else if ( u1type == UNIT_DISCHARGE ) {
			if ( GetConversionSame ( u1, "CMS", &mfac, &afac))
				return 3;
			*mult	= mfac;
		}
		if ( GetConversionSame(aunits, "M2", &mfac, &afac))
			return 5;
		area	= *aux;
		area	*= mfac;
		*mult	/= area;
		if ( GetConversionSame ( "M", u2, &mfac, &afac ) )
			return 4;
		*mult	*= mfac;	
		*add	= 0.0;
		return STATUS_SUCCESS;
	}
	return STATUS_FAILURE;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/GetConversion.c,v $";
 static char rcs_id2[] = "$Id: GetConversion.c,v 1.1 1999/02/18 15:16:41 dws Exp $";}
/*  ===================================================  */

}
