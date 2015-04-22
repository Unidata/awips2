#include "dg.h"

void dgc_qtms ( const int *mxtms, const int *both, char *tmlst, int *ntms,
	        char *trange, int *iret )
/************************************************************************
 * dgc_qtms								*
 *									*
 * This subroutine retrieves the list of times stored in the NFILE	*
 * block of DGCMN.CMN.							*
 *									*
 * If BOTH is false, only the first times are returned.  If BOTH is	*
 * .true. the first and second times are returned in the same element	*
 * of tmlst with : between them.  If the second time is blank, no : is	*
 * included.								*
 *									*
 * dgc_qtms ( mxtms, both, tmlst, ntms, trange, iret )			*
 *									*
 * Input parameters:							*
 *	*mxtms		const int	Maximum # of times to return	*
 *      *both		const int	Flag to return two times with : *
 *									*
 * Output parameters:							*
 *	*tmlst		char		List of times			*
 *	*ntms		int		Number of times			*
 *      *trange		char		Range of grid times             *
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/HPC		 3/04						*
 * R. Tian/SAIC		 4/04	Added TRANGE output			*
 * R. Tian/SAIC		 3/06	Recoded from Fortran			*
 ************************************************************************/
{
    char tstrt[37], tstop[37];
    int lstrt, i;
/*----------------------------------------------------------------------*/
    *iret = 0;

    if ( _nfile.ntmlst > *mxtms ) {
	*ntms = *mxtms;
    } else {
	*ntms = _nfile.ntmlst;
	if ( *ntms <= 0 ) return;
    }

    strcpy ( tmlst, _nfile.dtmlst1[0] );
    if ( *both == G_TRUE && _nfile.dtmlst2[0][0] != '\0' ) {
	strcat ( tmlst, ":" );
	strcat ( tmlst, _nfile.dtmlst2[0] );
    }
    for ( i = 1; i < *ntms; i++ ) {
	strcat ( tmlst, ";" );
	strcat ( tmlst, _nfile.dtmlst1[i] );
	if ( *both == G_TRUE && _nfile.dtmlst2[i][0] != '\0' ) {
	    strcat ( tmlst, ":" );
	    strcat ( tmlst, _nfile.dtmlst2[i] );
	}
    }

    /*
     * Encode the times into the "actual" grid time range (used in
     * building the default title) . . . reduce the start/stop times
     * to YYMMDD/HHthh if the other stuff is zero.  Drop A00 if both
     * have it, and drop however much of the stop YYMMDD/HH matches
     * the start values (starting at the left).
     */
    memset ( tstrt, 0, sizeof ( tstrt ) );
    memset ( tstop, 0, sizeof ( tstop ) );
    strcpy ( tstrt, _nfile.dtmlst1[0] );
    if ( *both == G_TRUE && _nfile.dtmlst2[0][0] != '\0' ) {
	strcat ( tstrt, ":" );
	strcat ( tstrt, _nfile.dtmlst2[0] );
    }
    strcpy ( tstop, _nfile.dtmlst1[(*ntms)-1] );
    if ( *both == G_TRUE && _nfile.dtmlst2[(*ntms)-1][0] != '\0' ) {
	strcat ( tstop, ":" );
	strcat ( tstop, _nfile.dtmlst2[(*ntms)-1] );
    }

    /*
     * The test on character 15 (last digit of 'hhh') ensures that
     * the work hasn't already been done on 13:17 ('hhhmm').
     */
    if ( ( tstrt[14] != '\0' ) && ( tstop[14] != '\0' ) ) {
	if ( ( strncmp ( &tstrt[15], "00", 2 ) == 0 ) &&
	     ( strncmp ( &tstop[15], "00", 2 ) == 0 ) ) {
	    memset ( &tstrt[15], 0, 2 );
	    memset ( &tstop[15], 0, 2 );
	}
	if ( ( tstrt[12] == '0' ) && ( tstop[12] == '0' ) ) {
	    memmove ( &tstrt[12], &tstrt[13], strlen(&tstrt[13])+1 );
	    memmove ( &tstop[12], &tstop[13], strlen(&tstop[13])+1 );
	}
    }

    if ( ( strncmp ( &tstrt[9], "00", 2 ) == 0 ) && 
         ( strncmp ( &tstop[9], "00", 2 ) == 0 ) ) {
	memmove ( &tstrt[9], &tstrt[11], strlen(&tstrt[11])+1 );
	memmove ( &tstop[9], &tstop[11], strlen(&tstop[11])+1 );
    }
    lstrt = strlen ( tstrt );
    if ( ( strncmp ( &tstrt[lstrt-3], "A00", 3 ) == 0 ) && 
         ( strncmp ( &tstop[lstrt-3], "A00", 3 ) == 0 ) ) {
	lstrt -= 3;
	tstop[lstrt] = '\0';
    } else if ( strncmp ( tstrt, tstop, 9 ) == 0 ) {
	strcpy ( tstop, &tstop[9] );
    }

    if ( strncmp ( tstrt, tstop, 6 ) == 0 ) {
	strcpy ( tstop, &tstop[6] );
    } else if ( strncmp ( tstrt, tstop, 4 ) == 0 ) {
	strcpy ( tstop, &tstop[4] );
    } else if ( strncmp ( tstrt, tstop, 2 ) == 0 ) {
	strcpy ( tstop, &tstop[2] );
    }
    strncpy ( trange, tstrt, lstrt );
    strcpy ( &trange[lstrt],"-" );
    strcat ( trange, tstop );

    return;
}
