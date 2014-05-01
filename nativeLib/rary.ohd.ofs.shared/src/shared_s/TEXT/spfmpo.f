C MEMBER SPFMPO
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 05/10/95.14:46:07 BY $WC20SV
C
C @PROCESS LVL(77)
C
C DESC: ROUTINE TO PRINT FUTURE MAP COMPUTATIONAL ORDER
C
      SUBROUTINE SPFMPO (IPRNT,IVFMPO,FMPID,NFMPID,ISTAT)
C
      DIMENSION FMPID(2,1)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/spfmpo.f,v $
     . $',                                                             '
     .$Id: spfmpo.f,v 1.2 1997/01/29 20:55:47 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,30)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('FMPO')
C
      ISTAT=0
C
C  CHECK NUMBER OF LINES LEFT ON PAGE
      IF (ISLEFT(10).GT.0) CALL SUPAGE
C
C  PRINT HEADING
      IF (IPRNT.EQ.1) THEN
         WRITE (LP,40)
         CALL SULINE (LP,2)
         WRITE (LP,50)
         CALL SULINE (LP,2)
         WRITE (LP,*) ' '
         CALL SULINE (LP,1)
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' IVFMPO=',IVFMPO,
     *      ' NFMPID=',NFMPID,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  PRINT FUTURE MAP AREA IDENTIFIERS
      IF (NFMPID.EQ.0) THEN
         WRITE (LP,70)
         CALL SULINE (LP,2)
         GO TO 20
         ENDIF
      NPER=5
      NTIME=NFMPID/NPER
      IF (MOD(NFMPID,NPER).NE.0) NTIME=NTIME+1
      IF (NTIME.EQ.0) NTIME=1
      NUM1=1
      NUM2=NFMPID
      WRITE (LP,80)
      CALL SULINE (LP,2)
      DO 10 I=1,NTIME
         WRITE (LP,90) (J,FMPID(1,J),FMPID(2,J),J=NUM1,NUM2,NTIME)
         NUM1=NUM1+1
         CALL SULINE (LP,1)
10       CONTINUE
C
20    WRITE (LP,40)
      CALL SULINE (LP,2)
      WRITE (LP,*) ' '
      CALL SULINE (LP,1)
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,100)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' *** ENTER SPFMPO')
40    FORMAT ('0',132('-'))
50    FORMAT ('0*--> FMPO PARAMETERS ',
     *   '(FUTURE MAP COMPUTATIONAL ORDER)')
70    FORMAT ('0*** NOTE - NUMBER OF FUTURE MAP IDENTIFIERS IS ZERO.')
80    FORMAT ('0FUTURE MAP AREA IDENTIFIERS:')
90    FORMAT (T5,5(:I4,': ' ,2A4,5X))
100   FORMAT (' *** EXIT SPFMPO')
C
      END
