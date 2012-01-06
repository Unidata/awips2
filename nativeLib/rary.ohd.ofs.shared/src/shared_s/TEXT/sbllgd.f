C MODULE SBLLGD
C-----------------------------------------------------------------------
C
      SUBROUTINE SBLLGD (FLON,FLAT,NBPTS,X,Y,ILLGD,ISTAT)
C
C  THIS ROUTINE CONVERTS FROM LONGITUDE/LATITUDE COORDINATES TO
C  HRAP GRID COORDINATES AND VISA-VERSA.
C
C  ARGUMENT LIST:
C 
C    NAME    TYPE  I/O  DIM     DESCRIPTION
C    ------  ----  ---  ------  -----------
C    FLON    R*4    I   NBPTS   LONGITUDE (DECIMAL DEGREES)
C    FLAT    R*4    I   NBPTS   LATITUDE (DECIMAL DEGREES)
C    NBPTS   I*4    I     1     NUMBER OF LAT/LON PAIRS
C    X       R*4    O   NBPTS   X COORDINATE
C    Y       R*4    O   NBPTS   Y COORDINATE
C    ILLGD   I*4    I     1     CONVERSION INDICATOR:
C                                 0=CONVERT GRID POINTS TO LAT/LON
C                                 1=CONVERT LAT/LON TO GRID POINTS
C    ISTAT   I*4    O     1     STATUS CODE
C
      DIMENSION X(NBPTS),Y(NBPTS),FLON(NBPTS),FLAT(NBPTS)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/sbllgd.f,v $
     . $',                                                             '
     .$Id: sbllgd.f,v 1.5 2001/06/13 12:41:36 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBLLGD'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      LDEBUG=ISBUG('GRID')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NBPTS=',NBPTS,' ILLGD=',ILLGD
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      NUMERR=0
C
      CALL LLGD (FLON,FLAT,NBPTS,X,Y,ILLGD,ISTAT)
      IF (ISTAT.EQ.-1) THEN
C     NUMBER OF PAIRS IS LE ZERO
         WRITE (LP,160) NBPTS
         CALL SUERRS (LP,2,NUMERR)
         ENDIF
      IF (ISTAT.EQ.-2) THEN
C     INVALID CONVERSION OPTION
         WRITE (LP,170) ILLGD
         CALL SUERRS (LP,2,NUMERR)
         ENDIF
      IF (ISTAT.GT.0) THEN
         IF (ILLGD.EQ.1) THEN
C        CHECK FOR BAD LONG/LAT INPUT
            DO 50 I=1,NBPTS
               IF (FLAT(I).LT.10.0.OR.FLAT(I).GT.80.0.OR.
     *             FLON(I).LT.40.0.OR.FLON(I).GT.180.0) THEN
                  WRITE (LP,190) FLAT(I),FLON(I)
                  CALL SUERRS (LP,2,-1)
               ENDIF
   50       CONTINUE
            ENDIF
         IF (ILLGD.EQ.0) THEN
C        CHECK FOR BAD GRID POINTS
            DO 100 I=1,NBPTS
               IF (X(I).LT.-300.0.OR.X(I).GT.1661.0.OR.
     *             Y(I).LT.1.0.OR. Y(I).GT.1601.0) THEN
                  WRITE (LP,220) X(I),Y(I)
                  CALL SUERRS (LP,2,-1)
                  ENDIF
  100          CONTINUE
            ENDIF
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         DO 130 I=1,NBPTS
            WRITE (IOSDBG,'(1X,A,I4,4(1X,A,F7.2))')
     *         'I=',I,
     *         'FLON(I)=',FLON(I),'FLAT(I)=',FLAT(I),
     *         'X(I)=',X(I),'Y(I)=',Y(I)
            CALL SULINE (IOSDBG,1)
130         CONTINUE
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBLLGD'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
160   FORMAT ('0*** ERROR - IN SBLLGD - NUMBER OF PAIRS TO BE ',
     *   'PROCESSED (',I4,') IS LESS THAN OR EQUAL TO ZERO.')
170   FORMAT ('0*** ERROR - IN SBLLGD - ',I4,' IS AN INVALID ',
     *   'CONVERSION OPTION VALUE. VALID VALUES ARE 0 AND 1.')
190   FORMAT ('0*** ERROR - IN SBLLGD - THE SPECIFIED LATITUDE/',
     *   'LONGITUDE (',F6.2,',',F6.2,') EXCEED THE VALID RANGE.')
220   FORMAT ('0*** ERROR - IN SBLLGD - THE SPECIFIED HRAP/NWSRFS ',
     *   'COORDINATES (',F8.2,',',F8.2,') EXCEED THE VALID RANGE.')
C
      END
