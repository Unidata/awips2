	SUBROUTINE SNDATA  ( isnfln, times, ntime, vlevel, nlev, lvert,
     +			     levtyp, prmlst, nparms, stnprm, nstnp, 
     +			     rdata, numlev, iret )
C************************************************************************
C* SNDATA								*
C*									*
C* This subroutine reads and lists merged data from the sounding	*
C* file for SNLIST.							*
C*									*
C* SNDATA  ( ISNFLN, TIMES, NTIME, VLEVEL, NLEV, LVERT, LEVTYP, 	*
C*           PRMLST, NPARMS, STNPRM, NSTNP, LUNS, NLUN, IRET )		*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	TIMES (NTIME)	CHAR*		Times				*
C*	NTIME		INTEGER		Number of times			*
C*	VLEVEL (NLEV)	REAL		Levels				*
C*	NLEV		INTEGER		Number of levels		*
C*	LVERT		INTEGER		Output vertical coordinate	*
C*	LEVTYP		INTEGER		Level type			*
C*					  1 = list			*
C*					  2 = range without increment	*
C*	PRMLST (NPARMS)	CHAR*		Level parameters		*
C*	NPARMS		INTEGER		Number of level parameters	*
C*	STNPRM (NSTNP)	CHAR*		Station parameters		*
C*	NSTNP		INTEGER		Number of station parameters	*
C*	LUNS   (NLUN)	INTEGER		Output device LUNs		*
C*	NLUN		INTEGER		Number of output devices	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -9 = no data listed		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/88	GEMPAK 4.1				*
C* M. desJardins/GSFC	11/89	Changes for STIM			*
C* K. Brill/NMC		 8/93	Change for 8-char ID			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	prmlst (*), stnprm (*), times (*)
	REAL		vlevel (*), rdata(*)
C*
	REAL		data ( LLMXDT ), sdata ( LLMXDT )
	INTEGER		loc(7)
	CHARACTER	stid*8
	LOGICAL		good, prmwrt, stnflg
	INCLUDE         'ERMISS.FNC'
C*
	DATA		loc / 1, 3, 4, 5, 6, 2, 0 /
C------------------------------------------------------------------------
	iret = 0
C
C*	Loop through times and stations listing data.
C
	prmwrt = .false.
	DO  k = 1, ntime
C
C*	    Set the time.
C
	    CALL SN_STIM  ( isnfln, times (k), ier )
C
C*	    Loop through stations.
C
	    iout = 0
	    DO WHILE  ( iout .eq. 0 )
		stnflg = .false.
		CALL SN_SNXT  ( isnfln, stid, istnm, slat, slon, selv, 
     +				iout )
C
C*		Get the data.
C
		IF  ( iout .eq. 0 )  THEN
		    CALL SN_RDAT  ( isnfln, ndlev, data, ihhmm, ier )
		    IF  ( ier .eq. 0 )  THEN
c	write(*,*) 'SNDATA: SN_RDAT was fine. Here is the data ',nparms
c	do i=1,ndlev
c	  write(6,222) data((i-1)*nparms+1), data((i-1)*nparms+2),
c    +	               data((i-1)*nparms+3), data((i-1)*nparms+4),
c    +	               data((i-1)*nparms+5), data((i-1)*nparms+6),
c    +	               data((i-1)*nparms+7)
c	enddo
c	write(*,*) 'SNDATA: Done printing data'
			good = .true.
c Save station info
			CALL PC_SSTN  ( stid, istnm, slat, slon, selv,
     +			ispri,ihhmm, ndlev, ier )
		      ELSE
			good = .false.
		    END IF
		  ELSE
		    good = .false.
		END IF
C
C*		Extract the level data.
C
c22		FORMAT ( 1x, F12.3, 1x, F12.3, 1x, I4 )
	      IF ( iout .eq. 0 .and. good ) THEN

		CALL SNLLVD  ( data, ndlev, nparms, vlevel, nlev,
     +                         lvert, levtyp, sdata, mlev, ier )
		istrt = 2
		rmax = 1090.
		DO i = 1, mlev
C		write(6,22) rmax,sdata((i-1)*nparms+1), i
		    IF ( rmax .lt. sdata((i-1)*nparms+1) ) THEN
			istrt = i+1
		    END IF
		    rmax = sdata((i-1)*nparms+1)
		END DO
C
C*		Load the data
C
c	write(*,*) 'SNDATA: nparms is ',nparms

		DO i = 1, nparms
		    IF ( ERMISS ( sdata(i) ) ) THEN
		      rdata(loc(i)+1) = -999.
		    ELSE
		      rdata(loc(i)+1) = sdata(i)
		    END IF
		END DO
		IF ( rdata(loc(5)+1) .ne. -999. ) 
     +			rdata(loc(5)+1) = PR_MSKN (rdata(loc(5)+1))
C
222		FORMAT ( 7 (1x, F10.4 ) )
		numlev = 1
		DO i = istrt, mlev
		  IF ( sdata((i-1)*nparms+1) .ge. 100. ) THEN
c	write(6,222) sdata((i-1)*nparms+1), sdata((i-1)*nparms+2),
c     +	             sdata((i-1)*nparms+3), sdata((i-1)*nparms+4),
c     +	             sdata((i-1)*nparms+5), sdata((i-1)*nparms+6),
c     +	             sdata((i-1)*nparms+7)
		    numlev = numlev + 1
		    DO j = 1, nparms
		       IF ( .not. ERMISS (sdata((i-1)*nparms+j)) ) THEN
		          rdata((numlev-1)*nparms+loc(j)+1) = 
     +					sdata((i-1)*nparms+j)
		       ELSE
		          rdata((numlev-1)*nparms+loc(j)+1) = -999.
		       END IF
		    END DO
		    IF ( rdata(((numlev-1)*nparms+6)+1) .ne. -999. )
     +	                rdata(((numlev-1)*nparms+6)+1) =
     +				PR_MSKN (rdata(((numlev-1)*nparms+6)+1))
		  END IF
		END DO
	      END IF
	    END DO
	END DO
C*
	RETURN
	END
