C************************************************************************
C* GET_GEM_SND (Main Routine)                                           *
C*    Gets data from a sounding dataset                                 *
C*                                                                      *
C* SWAP_SNDG (LJH ADD)                                                  *
C*    Used in sorting Sounding Data to fix NSHARP temp=dwpt bug         *
C************************************************************************
        SUBROUTINE SWAP_SNDG (rdata, i1, i2)
        REAL rdata(*)
        INTEGER i1, i2, j
        REAL temp
        DO J=1, 7
          temp=rdata((i1-1)*7+J)
          rdata((i1-1)*7+J)=rdata((i2-1)*7+J)
          rdata((i2-1)*7+J)=temp
        END DO
        RETURN
        END

	SUBROUTINE GET_GEM_SND ( snfile, dattim, area, rdata, numlev )
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
C* T. Piper/SAIC	 1/02	Initialized arecur, datcur, & isnfln	*
C* T. Piper/SIAC	11/02	Fixed calling sequence to SNLFIL&SNLLEV	*
C* L. Hinson/AWC         3/03   Defined/initialized npmdst              *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	REAL		rdata(*)
	INTEGER		numlev
	INTEGER		I, J, sindex
	CHARACTER*(*)	snfile, area, dattim
	CHARACTER	snparm*72, stndex*72, levels*72, vcoord*72,
     +			mrgdat*72
C
	CHARACTER	snfcur*72, pmdset (MMPARM)*4, arecur*48, stn*8,
     +			datcur*48, times (LLMXTM)*20, voutc*4,
     +			prmlst (MMPARM)*4, stnprm (MMPARM)*4
	REAL		vlevel (LLMXLV)
	LOGICAL		newfil, proces, mrgflg, prtflg
	INTEGER         npmdst, omegpos
C------------------------------------------------------------------------
C*	Initialize user interface.
C
        npmdst = 0
	omegpos = 0
	arecur = ' '
	datcur = ' '
	isnfln = 0
		numlev = 0
		CALL IN_BDTA  ( ier )
C
C*	    Read in variables from user interface.
C
	        proces = .true.
		prtflg = .false.
C
C*	        Open the input file.
C
C		must initialize snfcur since file is always closed (chiz)
                snfcur = ' '
	        CALL SNLFIL  ( snfile, snfcur, isnfln, newfil, iflsrc,
     +			       pmdset, npmdst, ivert, mrgflg, iret )
		IF  ( iret .ne. 0 )  proces = .false.
C
C*		Decode merge type.
C
		mrgdat = 'y'
		IF  ( proces )  THEN
		    CALL IN_MRGD  ( mrgdat, prtflg, ipttyp, ier )
		    prtflg = .not. prtflg
		    IF  ( prtflg .and. mrgflg )  THEN
			CALL ER_WMSG  ( 'SNLIST', -11, ' ', ier )
			proces = .false.
		    END IF
		END IF
C
C*	        Set the area.
C
	        IF  ( proces )  THEN
		    CALL LC_UARE  ( area, newfil, isnfln, arecur, stn,
     +				    iret )
		    IF  ( iret .ne. 0 )  proces = .false.
		END IF
C
C*	        Get the levels and vertical coordinate.
C
		levels = 'all'
		vcoord = 'pres'
	        IF  ( proces .and. ( .not. prtflg ) )  THEN
		    CALL SNLLEV  ( isnfln, levels, vcoord, ivert, nlevl,
     +				   vlevel, levtyp, voutc,  lvert, 
     +				   nparts, iret )
                    IF  ( iret .ne. 0 ) proces = .false.
		END IF
C
C*	        Get input times and pointers.
C
	        IF  ( proces )  THEN
		    CALL SNLDAT  ( isnfln, dattim, newfil,
     +				   datcur, ntime, times, iret )
		    IF  ( iret .ne. 0 )  proces = .false.
		END IF
C
C*	        Get parameter information.
C
	        IF  ( proces .and. ( .not. prtflg ) )  THEN
C		    snparm='pres;tmpc;dwpc;drct;sped;hght'
                    CALL ST_FIND('OMEG',pmdset,npmdst,omeg_pos,iret)
                    if(omeg_pos.ne.0) then
                       snparm='PRES;TMPC;DWPC;DRCT;SPED;HGHT;OMEG'
                    else
C                      Account for NO VVELs in observed soundings
                       snparm='PRES;TMPC;DWPC;DRCT;SPED;HGHT;HGHT'
                    endif
		    stndex = ' '
		    CALL SNLPRM  ( snparm, stndex, voutc, pmdset, 
     +				   npmdst, nparms, prmlst, nstnp, 
     +				   stnprm, iret )
                    IF  ( (nparms .le. 0) .and. (nstnp .eq. 0) ) THEN
			 proces = .false.
			 CALL ER_WMSG  ( 'SNLIST', -3, ' ', ier )    
                    END IF
                    IF  ( (nlevl .eq. 0) .and. (nstnp .eq. 0) )  THEN
			proces = .false.
			CALL ER_WMSG  ( 'SNLIST', -12, ' ', ier )
                    END IF
		END IF
C
C*		Get and list the data.
C
		IF  ( proces )  THEN
			CALL SNDATA  ( isnfln, times, ntime, vlevel, 
     +				       nlevl, lvert, levtyp, prmlst, 
     +				       nparms, stnprm, nstnp,
     +				       rdata, numlev, iret )
		    IF  ( iret .ne. 0 )  THEN
			CALL ER_WMSG  ( 'SNLIST', -9, ' ', ier )
			proces = .false.
		    END IF
		END IF
C
C*	Final error message and close file.
C
	CALL SN_CLOS  ( isnfln, iret )
C*      (LJH add)
C*      Sort the data with pressure descending sequence.
C*      This will fix NSHARP temp=dwpt bug
C*
        DO I = 1, numlev - 1
           sindex = I
           DO J = I+1, numlev
             IF (rdata((J-1)*7+2) .GT. rdata((sindex-1)*7+2)) THEN
               sindex=J
             END IF
           END DO
           IF (I .NE. sindex) THEN
             CALL SWAP_SNDG(rdata, i, sindex)
           END IF
        END DO
	END
