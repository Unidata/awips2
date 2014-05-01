C MODULE SBFMIN
C-----------------------------------------------------------------------
C
      SUBROUTINE SBFMIN (Y,NY,YMIN,ISTAT)
C
C  THIS ROUTINE COMPUTES THE MINIMUM VALUE OF AN ARRAY OF REAL
C  VALUES.
C
      INCLUDE 'scommon/sudbgx'
C
      DIMENSION Y(NY)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbfmin.f,v $
     . $',                                                             '
     .$Id: sbfmin.f,v 1.2 1999/07/06 11:41:15 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBFMIN'
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
      YMIN=Y(1)
      IF (NY.LE.1) GO TO 30
C
      DO 20 I=2,NY
         IF (Y(I).LT.YMIN) YMIN=Y(I)
20       CONTINUE
C
30    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'YMIN=',YMIN
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBFMIN'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
      END
