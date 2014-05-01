	SUBROUTINE DM_RCLH  ( iflno, ipos, iheadr, iret )
C************************************************************************
C* DM_RCLH								*
C*									*
C* This subroutine reads a column header from a DM file.		*
C*									*
C* DM_RCLH  ( IFLNO, IPOS, IHEADR, IRET )				*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	IPOS		INTEGER		Location			*
C*									*
C* Output parameters:							*
C*	IHEADR (*)	INTEGER		Header array			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file is not open		*
C*					 -9 = invalid column		*
C*					-11 = undefined header		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	4/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
        INCLUDE         'dbcmn.cmn'  
C
	INTEGER		iheadr (*)
C
        INTEGER         istid(2), intdtf (3)
C------------------------------------------------------------------------
C
C*      For A2DB requests - set the needed information.
C
        IF (  dbread ) THEN
           IF ( dbdatasrc .eq. 'grid' ) THEN
C
C*            For grid data - set the time.
C
              CALL ST_LSTR  ( dbtime, ldbtime, ier )
              IF ( ldbtime .gt. 0 ) THEN
                 CALL TG_CTOI  ( dbtime, intdtf, ier )
                 CALL TG_ITOF  ( intdtf, iheadr, ier )
              ELSE 
                 iret = -9
                 RETURN
              END IF
           ELSE 
C
C*            For point data - set the station id.
C
              CALL ST_LSTR (dbstid, lstr, ier)
              IF ( lstr .eq. 4 .and. dbstid(1:1) .eq. 'K' ) THEN 
                 ist=2
              ELSE
                 ist = 1
              END IF
              IF ( dbdatasrc .eq. 'metar'  ) THEN
                CALL ST_STOI (dbstid(ist:lstr), 8, nv, istid, ier )
                iheadr(1) = istid(1)
                iheadr(2) = IMISSD
              ELSE IF ( dbdatasrc .eq. 'bufrua' ) THEN 
                iheadr(1) = IMISSD
                CALL ST_NUMB (dbstid(ist:lstr), istid2, ier )
                iheadr(2) = istid2
              END IF
              iheadr(3) = dbstlt
              iheadr(4) = dbstln
              iheadr(5) = dbstel
              iheadr(8) = IMISSD
           END IF 
           iret = 0
           RETURN
        END IF
C
C*	Check that the file number is valid.
C
	CALL DM_CHKF ( iflno, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Check for valid position.
C
	IF ( (ipos .le. 0) .or. (ipos .gt. kcol (iflno))) THEN
	    iret = -9
	    DO  i = 1, kckeys ( iflno )
		iheadr (i) = IMISSD
	    END DO
	  ELSE 
C
C*	    Check that this header is defined.
C
	    jloc = ipos + krow ( iflno )
	    IF  ( kheadr ( 0, jloc, iflno ) .ne. IMISSD )  THEN
C
C*		Retrieve row header.
C
		DO  i = 1, kckeys (iflno)
		    iheadr (i) = kheadr ( i, jloc, iflno )
		END DO
C
C*		Set error return.
C
	      ELSE
		iret = -11
		DO  i = 1, kckeys ( iflno )
		    iheadr (i) = IMISSD
		END DO
	    END IF
	END IF
C*
	RETURN
	END
