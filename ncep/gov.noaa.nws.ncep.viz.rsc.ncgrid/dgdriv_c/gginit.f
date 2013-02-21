 	SUBROUTINE GG_INIT  ( mode, iret )
C************************************************************************
C* GG_INIT								*
C*									*
C* This subroutine initializes the GEMPLT plotting package.  The	*
C* current map file is set to the global map file name found in		*
C* $MAPFIL.  Thus, it is necessary to call IP_INIT before calling this	*
C* subroutine.  If IP_INIT has not been called, the map file will not	*
C* be defined.								*
C*									*
C* In the past, this subroutine set default margins for map and graph	*
C* mode.  Currently, margins will not be set or changed in GG_INIT.	*
C* Margins can be specified by the user in the input for PROJ.  The	*
C* margin definition will be extracted by GG_PROJ and set in GG_MAPS.	*
C*									*
C* GG_INIT  ( MODE, IRET )						*
C*									*
C* Input parameters:							*
C*	MODE		INTEGER		Plot mode 			*
C*				  	  0 = no change			*
C*				  	  1 = map 			*
C*				  	  2 = graph			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER 	Return code			*
C*				  	  0 = normal return		*
C*				 	 -3 = error starting GEMPLT	*
C**									*
C* Log:									*
C* M. Goodman/RDS	12/84	Original source				*
C* M. desJardins/GSFC	 2/85	Changed error processing		*
C* I. Graffman/RDS 	11/86	Added graph mode			*
C* I. Graffman/RDS 	 1/88	Added global map file name		*
C* M. desJardins/GSFC	 5/88	Eliminated fatal error for bad map file	*
C* I. Graffman/RDS	 5/88	Added docs on global map file		*
C* M. desJardins/GSFC	 6/88	Eliminated margin set and MPFIL error	*
C* G. Huffman/GSC	10/88	Change to $MAPFIL			*
C* M. desJardins/GSFC	 7/90	Set $MAPFIL for any mode		*
C* M. desJardins/GSFC 	02/91	Move map file set up to GGMAP		*
C* K. Brill/NMC         01/92   Replace GERROR with ER_WMSG             *
C* S. Jacobs/NMC	 9/94	Added common GGCMN			*
C* J. Cowie/COMET	 1/95	Changed GGCMN to IMGDEF			*
C* J. Cowie/COMET	 5/95	Moved IMGDEF common initialization to	*
C*				IM_INIT ()				*
C* M. Linda/GSC		12/95	Modified the comment block		*
C************************************************************************
C------------------------------------------------------------------------
C*	Initialize GEMPLT.  Write error message if necessary.
C
	iret = 0
	CALL GINITP  ( mode, istat, ierr )
	IF  ( ierr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GEMPLT', ierr, ' ', jerr )
	    iret = -3
	    RETURN
	END IF
C*
	RETURN
	END
