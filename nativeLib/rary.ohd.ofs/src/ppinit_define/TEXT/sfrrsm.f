C MODULE SFRRSM
C-----------------------------------------------------------------------
C
C  ROUTINE TO DEFINIE RRS MISSING DATA PARAMETERS.
C
      SUBROUTINE SFRRSM (INULL,NFLD,NUMFLD,ITYPE,INTEGR,REAL,
     *   LCHAR,CHAR,LCHK,CHK,
     *   ISTRT,LENGTH,LLPAR,LRPAR,
     *   NXOFLD,NOPFLD,IOPFL1,
     *   NRRSTP,RRSTYP,INWRRS,UMISS,IRSREF,URMISS,NMISS,
     *   IRRMSG,INTERP,EXTRAP,
     *   NUMERR,NUMWRN,ISTAT)
C
C
      CHARACTER*(*) CHAR(LCHAR),CHK(LCHK)
      CHARACTER*4 UMISS(*)
C
      INCLUDE 'scommon/dimrrs'      
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfrrsm.f,v $
     . $',                                                             '
     .$Id: sfrrsm.f,v 1.2 1998/04/07 15:10:51 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,250)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('RRS ')
C
      ISTAT=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  MISSING DATA OPTION
C
C  CHECK WHICH FIELD TO BE PROCESSED
      NGOTO=NXOFLD+1
      GO TO (10,140,190),NGOTO
C
C  CHECK IF OPTIONAL FIELD PRECIOUSLY PROCESSED
10    IF (IOPFL1.EQ.0) GO TO 20
         WRITE (LP,300)
         CALL SUWRNS (LP,2,NUMWRN)
C
20    IOPFL1=1
C
      IF (INULL.EQ.0) GO TO 30
         IF (INWRRS.EQ.0) GO TO 90
            WRITE (LP,280) NXOFLD,NFLD
            CALL SUERRS (LP,2,NUMERR)
            GO TO 90
30    IF (ITYPE.EQ.2) GO TO 40
         WRITE (LP,270) NXOFLD,NFLD
         CALL SUERRS (LP,2,NUMERR)
         GO TO 90
40    IF (LLPAR.GT.0) GO TO 50
         CHK(1)='NO'
         WRITE (LP,310) NFLD,CHK(1)
         CALL SUWRNS (LP,2,NUMWRN)
         GO TO 70
50    IF (LRPAR.GT.0) GO TO 60
         WRITE (LP,290) NFLD
         CALL SULINE (LP,2)
         LRPAR=LENGTH+1
60    CALL UFPACK (LCHK,CHK,ISTRT,LLPAR+1,LRPAR-1,IERR)
70    IF (CHK(1).EQ.'YES'.OR.CHK(1).EQ.'NO') GO TO 80
         WRITE (LP,320) CHK(1)
         CALL SUERRS (LP,2,NUMERR)
         GO TO 90
C
80    IF (CHK(1).EQ.'YES') GO TO 130
C
C  SET OUTPUT UNITS
      URMISS(NRRSTP)=UMISS(IRSREF)
C
C  CHECK IF MISSING OPTION ALLOWED
      IF (URMISS(NRRSTP).NE.'SAME') GO TO 90
         WRITE (LP,330) RRSTYP(NRRSTP)
         CALL SUERRS (LP,2,NUMERR)
         URMISS(NRRSTP)='EROR'
C
C  IF EXISTS, FIND SLOT WHERE INTERP AND EXTRAP PARAMETERS STORED
90    IF (INWRRS.EQ.0.AND.NMISS.GT.0) GO TO 100
         NMISS=NMISS+1
         IRRMSG=NMISS
         GO TO 120
100   NSLOT=0
      DO 110 I=1,NMISS
         IF (URMISS(I).EQ.'SAME') GO TO 110
         IF (I.GT.NRRSTP) GO TO 110
         NSLOT=NSLOT+1
110      CONTINUE
         IRRMSG=NSLOT
120   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,340) URMISS(NRRSTP),IRRMSG,NMISS
         CALL SULINE (LP,1)
         ENDIF
C
130   NOPFLD=1
      NXOFLD=1
      GO TO 240
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  INTERPOLATION OPTION
C
140   IF (INULL.EQ.0) GO TO 150
         IF (INWRRS.EQ.0) GO TO 180
            WRITE (LP,280) NXOFLD,NFLD
            CALL SUERRS (LP,2,NUMERR)
            GO TO 180
150   IF (ITYPE.EQ.2) GO TO 160
         WRITE (LP,260) NXOFLD,NFLD
         CALL SUERRS (LP,2,NUMERR)
         GO TO 180
160   IF (CHAR(1).EQ.'LIN'.OR.CHAR(1).EQ.'RET') GO TO 170
         WRITE (LP,350) CHAR(1)
         CALL SUERRS (LP,2,NUMERR)
         GO TO 180
170   IF (CHAR(1).EQ.'LIN') INTERP(IRRMSG)=0
      IF (CHAR(1).EQ.'RET') INTERP(IRRMSG)=1
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,360) INTERP(IRRMSG),CHAR(1)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
180   NXOFLD=2
      GO TO 240
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  EXTRAPOLATION RECESSION CONSTANT
C
190   IF (INULL.EQ.0) GO TO 200
         IF (INWRRS.EQ.0) GO TO 230
            WRITE (LP,280) NXOFLD,NFLD
         CALL SUERRS (LP,2,NUMERR)
            GO TO 230
200   IF (ITYPE.NE.2) GO TO 210
         WRITE (LP,260) NXOFLD,NFLD
         CALL SUERRS (LP,2,NUMERR)
         GO TO 230
210   IF (REAL.GT.0..AND.REAL.LE.1.) GO TO 220
         WRITE (LP,370) REAL
         CALL SUERRS (LP,2,NUMERR)
         GO TO 230
220   EXTRAP(IRRMSG)=REAL
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,380) EXTRAP(IRRMSG)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
230   NOPFLD=0
      NXOFLD=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
240   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,390)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
250   FORMAT (' *** ENTER SFRRSM')
260   FORMAT ('0*** ERROR - NON-CHARACTER DATA EXPECTED IN INPUT ',
     *   'FIELD ',I2,' (CARD FIELD ',I2,').')
270   FORMAT ('0*** ERROR - CHARACTER DATA EXPECTED IN INPUT ',
     *   'FIELD ',I2,' (CARD FIELD ',I2,').')
280   FORMAT ('0*** ERROR - NO VALUE FOUND FOR REQUIRED INPUT FIELD ',
     *   I2,' (CARD FIELD ',I2,').')
290   FORMAT ('0*** NOTE - RIGHT PARENTHESES ASSUMED. IN FIELD ',I2,
     *   '.')
300   FORMAT ('0*** WARNING - MISSING DATA OPTION HAS ',
     *   'ALREADY BEEN SPECIFIED.')
310   FORMAT ('0*** WARNING - NO LEFT PARENTHESIS FOUND IN CARD ',
     *   'FIELD ',I2,'. MSNG OPTION SET TO ',A4,'.')
320   FORMAT ('0*** ERROR - MISSING DATA OPTION (',A4,
     *   ') IS INVALID. VALID VALUES ARE ''YES'' OR ''NO''.')
330   FORMAT ('0*** ERROR - MISSING DATA OPTION NOT ALLOWED FOR DATA ',
     *   'TYPE ',A4,'.')
340   FORMAT (' MISSING INDICATOR SET TO : ',A4,3X,
     *   'AND IRRMSG SET TO : ',I2,3X,'AND NMISS IS : ',I2)
350   FORMAT ('0*** ERROR - INTERPOLATION OPTION (',A4,
     *   ') IS INVALID. VALID VALUES ARE ''LIN'' OR ''RET''.')
360   FORMAT (' INTERPOLATION OPTION SET TO : ',I2,3X,'(',A4,')')
370   FORMAT ('0*** ERROR - EXTRAPOLATION RECESSION CONSTANT (',F7.2,
     *   ') IS INVALID. VALUE MUST BE GREATER THAN ZERO AND LESS ',
     *   'OR EQUAL TO 1.')
380   FORMAT (' EXTRAPOLATION RECESSION CONSTANT SET TO : ',F7.2)
390   FORMAT (' *** EXIT SFRRSM')
C
      END
