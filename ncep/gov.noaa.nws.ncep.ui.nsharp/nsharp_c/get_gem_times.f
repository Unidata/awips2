	SUBROUTINE GET_GEM_TIMES ( snfile, iflag, time_list,
     +							 ntimf, iret )
C************************************************************************
C* SNLIST								*
C*									*
C* This program lists data from a sounding dataset.			*
C*									*
C* Log:									*
C* I. Graffman/RDS	 8/87						*
C* M. desJardins/GSFC	10/88	Rewritten				*
C* M. desJardins/GSFC	 4/89	Modify to list unmerged data		*
C* S. Schotz/GSC	 8/90	Corrected bogus error message for 	*
C*				unmerged listing			*
C* J. Whistler/SSAI	 5/91	Changed output*20 to output*48		*
C* S. Jacobs/NMC	 6/94	STNDEX*48 --> *72			*
C* L. Williams/EAI	 7/94	Removed call to SNLUPD			*
C* S. Jacobs/NMC	 3/95	Changed call to SNLLEV to pass file num	*
C* T. Piper/SAIC	 1/02	Initialized snfcur & isnfln		*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*)	snfile
	INTEGER		iflag
	CHARACTER*(*)	time_list(*)
C	CHARACTER	time_list(500)*20
	CHARACTER	snfcur*72, pmdset (MMPARM)*4
	CHARACTER*20	tmptim
	LOGICAL		newfil, mrgflg
C------------------------------------------------------------------------
C*	Initialize user interface.
C
	iret = 0
	isnfln = 0
	snfcur = ' '

c	CALL IN_BDTA  ( ier ) chiz 11/19
C
C*	Open the input file.
C
        CALL SNLFIL  ( snfile, snfcur, isnfln, newfil, iflsrc, pmdset,
     +		       npmdst, ivert, mrgflg, iret )
	IF ( iret .eq. 0 ) THEN
C
C*	   Get input times and pointers.
C
	   CALL SN_GTIM  ( isnfln, LLMXTM, ntimf, time_list, ier )
C
C*	   return only 0Z and 12Z times if requested
C
	   IF ( iflag .eq. 1 ) THEN
	      j = 0
	      i = 1
	      DO i = 1, ntimf
	         IF ( ( time_list(i)(8:11) .eq. '0000' ) .or.
     +	              ( time_list(i)(8:11) .eq. '1200' ) ) THEN
		    j = j + 1
		    IF ( j .ne. i ) THEN
		       time_list (j) = time_list(i)
		    END IF
		 END IF 
	      END DO
	      ntimf = j
	   END IF
C
C*	   return reverse sorted time list
C
	   DO i=1,ntimf/2
	      tmptim = time_list(ntimf - ( i - 1) )
	      time_list(ntimf - ( i - 1) ) = time_list(i)
	      time_list(i) = tmptim
	   END DO
C
	   CALL SN_CLOS  ( isnfln, iret )
	END IF
C*
	END
