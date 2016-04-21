C MODULE SBIMAX
C-----------------------------------------------------------------------
C
      SUBROUTINE SBIMAX (IX,NIX,IXMAX,ISTAT)
C
C  THIS ROUTINE DETERMINES THE MAXIMUM VALUE OF AN ARRAY OF INTEGER 
C  VALUES.
C
      INCLUDE 'scommon/sudbgx'
C
      DIMENSION IX(NIX)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbimax.f,v $
     . $',                                                             '
     .$Id: sbimax.f,v 1.2 1999/07/06 11:41:52 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBIMAX'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
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
      IXMAX=IX(1)
      IF (NIX.LE.1) GO TO 30
C
      DO 20 I=2,NIX
         IF (IX(I).GT.IXMAX) IXMAX=IX(I)
20       CONTINUE
C
30    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'IXMAX=',IXMAX
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBIMAX'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
      END
