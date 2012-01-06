C MODULE SBIMIN
C-----------------------------------------------------------------------
C
      SUBROUTINE SBIMIN (IX,NIX,IXMIN,ISTAT)
C
C  THIS ROUTINE DETERMINES THE MINIMUM VALUE OF AN ARRAY OF INTEGER
C  VALUES.
C
      INCLUDE 'scommon/sudbgx'
C
      DIMENSION IX(NIX)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbimin.f,v $
     . $',                                                             '
     .$Id: sbimin.f,v 1.2 1999/07/06 11:42:05 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBIMIN'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      LDEBUG=ISBUG('GRID')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NIX=',NIX
         NVAL=NIX
         NPER=10
         NLINE=NVAL/NPER
         IF (NVAL/NPER*NPER.NE.NVAL) NLINE=NLINE+1
         NLINE=NLINE+1
         WRITE (IOSDBG,'(1X,A,I2,A/(10(1X,I6)))')
     *      'IX(1...',NVAL,')=',(IX(I),I=1,NVAL)
         CALL SULINE (IOSDBG,NLINE)
         ENDIF
C
      ISTAT=0
C
      IXMIN=IX(1)
      IF (NIX.LE.1) GO TO 40
C
      DO 30 I=2,NIX
         IF (IX(I).LT.IXMIN) IXMIN=IX(I)
30       CONTINUE
C
40    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'IXMIN=',IXMIN
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBIMIN'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
      END
