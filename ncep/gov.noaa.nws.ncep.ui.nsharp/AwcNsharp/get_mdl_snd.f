	SUBROUTINE  GET_MDL_SND ( gdfile, gdatim, gpoint, rdata, 
     +				  numlev, rlon, rlat )
C************************************************************************
C* GET_MDL_SND								*
C*									*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdfile, gdatim, gpoint
     	REAL		rdata (*)
C*
	CHARACTER	time (2)*20
	LOGICAL		havsfc, gottm
C*
	REAL		y (LLMXLV), data(LLMXDT)
	INTEGER		loc(7)
	INCLUDE         'ERMISS.FNC'
c	INTEGER		loc(6)
c	DATA            loc / 1, 3, 4, 5, 6, 2 /
	DATA            loc / 1, 3, 4, 5, 6, 2, 0 /
C*
C-----------------------------------------------------------------------
C*	Initialize TAE and GEMPLT.
C
c	CALL IN_BDTA  ( ier )
c	CALL GG_INIT  ( 1, iperr )
C
C*	Process the GDFILE input.
C
	CALL DG_NFIL ( gdfile, ' ', iret )
	IF ( iret .ne. 0 ) RETURN
C
C*	Process the GDATTIM input; setup the time server.
C
	CALL DG_NDTM ( gdatim, iret )
	IF ( iret .ne. 0 ) RETURN
C
C*	Get the next time to process from time server.
C
	CALL DG_NTIM ( .true., .false., time, gottm, iret )
	IF ( iret .ne. 0 ) RETURN
C
C*      Set the subset region to speed calculations.
C
        CALL DG_SUBG ( 'N', imin, imax, jmin, jmax, iret )
        IF ( iret .ne. 0 ) THEN
           CALL ER_WMSG  ( 'DG', iret, ' ', ier )
           RETURN
        END IF
C
C*	Get data.
C
        numlev = 0
	CALL GDUDTA ( 1, gdatim, 'PRES', gpoint, time, 1, data, nlev,
     +                rgx, rgy, rlat,  rlon, y, havsfc,
     +                iret )
        
	IF ( iret .ne. 0 ) RETURN
C
C*	Reformat the data.
C
c	nparms = 6
	nparms = 7
222	FORMAT ( 1x, I4, 7 ( 1x, F12.3 ) )
c222	FORMAT ( 1x, I4, 6 ( 1x, F12.3 ) )
        DO i = 1, nlev
c	  write(6,222) i,data((i-1)*nparms+1), data((i-1)*nparms+2),
c    +	               data((i-1)*nparms+3), data((i-1)*nparms+4),
c    +	               data((i-1)*nparms+5), data((i-1)*nparms+6),
c    +	               data((i-1)*nparms+7)

c Make sure we're at a pressure level betw. sfc and 100mb
          IF ( data((i-1)*nparms+1) .ge. 100. ) THEN
            numlev = numlev + 1

            DO j = 1, nparms
c            DO j = 1, nparms+1
c	        if (j .eq. (nparms+1)) goto 111
                IF ( .not. ERMISS ( data((i-1)*nparms+j) ) ) THEN
c                    rdata((numlev-1)*(nparms+1)+1+loc(j)) =
                    rdata((numlev-1)*(nparms)+1+loc(j)) =
     +                                  data((i-1)*nparms+j)
                ELSE
                    rdata(((numlev-1)*(nparms))+1+loc(j)) = -999.
c                    rdata(((numlev-1)*(nparms+1))+1+loc(j)) = -999.
                END IF
c111             continue
            END DO

c Compute wind speed
c	    rdata(((numlev-1)*(nparms+1)+1+6)) = 
	    rdata(((numlev-1)*(nparms)+1+6)) = 
     +	      PR_SPED ( PR_MSKN (data((i-1)*nparms+4)),
     +	     		PR_MSKN (data((i-1)*nparms+5)) ) 

c Compute wind direction
c	    rdata(((numlev-1)*(nparms+1)+1+5)) = 
	    rdata(((numlev-1)*(nparms)+1+5)) = 
     +		PR_DRCT ( data((i-1)*nparms+4),data((i-1)*nparms+5)) 

c If one or the other is missing set them both to missing
c	    IF ( ERMISS ( rdata(((numlev-1)*(nparms+1)+6)) ) .or.
c     +		 ERMISS ( rdata(((numlev-1)*(nparms+1)+6)) ) ) THEN
	    IF ( ERMISS ( rdata(((numlev-1)*(nparms)+6)) ) .or.
     +		 ERMISS ( rdata(((numlev-1)*(nparms)+6)) ) ) THEN
		rdata(((numlev-1)*(nparms)+1+6)) = -999.
		rdata(((numlev-1)*(nparms)+1+5)) = -999.
c		rdata(((numlev-1)*(nparms+1)+1+6)) = -999.
c		rdata(((numlev-1)*(nparms+1)+1+5)) = -999.
	    END IF
          END IF
        END DO
C
	CALL DG_NEND ( iret )
C*
	END
