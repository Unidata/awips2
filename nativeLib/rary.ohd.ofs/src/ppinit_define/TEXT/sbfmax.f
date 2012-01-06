C MODULE SBFMAX
C-----------------------------------------------------------------------
C
      SUBROUTINE SBFMAX (Y,NY,YMAX,ISTAT)
C
C  THIS ROUTINE DETERMINES THE MAXIMUM VALUE IF AN ARRAY OF REAL VALUES.
C
      INCLUDE 'scommon/sudbgx'
C
      DIMENSION Y(NY)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbfmax.f,v $
     . $',                                                             '
     .$Id: sbfmax.f,v 1.2 1999/07/06 11:40:34 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBFMAX'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      LDEBUG=ISBUG('GRID')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NY=',NY
         CALL SULINE (IOSDBG,1)
         NVAL=NY
         NPER=10
         NLINE=NVAL/NPER
         IF (NVAL/NPER*NPER.NE.NVAL) NLINE=NLINE+1
         NLINE=NLINE+1
         WRITE (IOSDBG,'(1X,A,I2,A/(10(1X,F7.2)))')
     *      'Y(1...',NVAL,')=',(Y(I),I=1,NVAL)
         CALL SULINE (IOSDBG,NLINE)
         ENDIF
C
10    YMAX=Y(1)
      IF (NY.LE.1) GO TO 30
C
      DO 20 I=2,NY
         IF (Y(I).GT.YMAX) YMAX=Y(I)
20       CONTINUE
C
30    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'YMAX=',YMAX
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBFMAX'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
      END
