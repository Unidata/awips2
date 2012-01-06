C MODULE SBMDRW
C-----------------------------------------------------------------------
C
      SUBROUTINE SBMDRW (X,Y,MDR,MDRDIM,NMDR,ISTAT)
C
C  THIS ROUTINE COMPUTES THE MDR BOXES WHICH FALL WITHIN A SET LAT/LON
C  POINTS.
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suerrx'
C
      DIMENSION X(4),Y(4),MDR(MDRDIM)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbmdrw.f,v $
     . $',                                                             '
     .$Id: sbmdrw.f,v 1.2 1999/07/07 11:25:50 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBMDRW'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      LDEBUG=ISBUG('GRID')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,'(2(1X,A,4(1X,F7.2)))')
     *     '(X(I),I=1,4)=',(X(I),I=1,4),
     *     '(Y(I),I=1,4)=',(Y(I),I=1,4)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
C  COMPUTE MIN AND MAX POINTS IN X AND Y DIRECTIONS
      NX=4
      NY=4
      CALL SBFMIN (X,NX,XMIN,ISTAT)
      CALL SBFMAX (X,NX,XMAX,ISTAT)
      CALL SBFMIN (Y,NY,YMIN,ISTAT)
      CALL SBFMAX (Y,NY,YMAX,ISTAT)
C
C  COMPUTE BOXES SURROUNDING DESIGNATED ARE
      CALL SBMDRS (XMIN,YMIN,MXMIN,MYMIN,ISTAT)
      CALL SBMDRS (XMAX,YMAX,MXMAX,MYMAX,ISTAT)
C
      MX=MXMIN
      MY=MYMAX
      NMDR=0
C
30    NMDR=NMDR+1
      IF (NMDR.GT.MDRDIM) THEN
         WRITE (LP,150) MDRDIM
150   FORMAT ('0*** ERROR - IN SBMDRV - DIMENSION (',I4,') OF MDR ',
     *   'ARRAY HAS BEEN EXCEEDED.')
         CALL SUERRS (LP,2,-1)
         GO TO 110
         ENDIF
C
C  COMPUTE BOX NUMBER
      CALL SBMNUM (MX,MY,MDR(NMDR),ISTAT)
      MX=MX+1
      IF (MX.LE.MXMAX) GO TO 30
      MX=MXMIN
      MY=MY-1
      IF (MY.GE.MYMIN) GO TO 30
C
      IF (LDEBUG.GT.0) THEN
         NPER=10
         NLINE=NMDR/NPER
         IF (NMDR/NPER*NPER.NE.NMDR) NLINE=NLINE+1
         WRITE (IOSDBG,'(1X,A,I3,1X,A,I3,A,(10(1X,I6)))')
     *      'NMDR=',NMDR,
     *      'MDR(1...',NMDR,')=',(MDR(I),I=1,NMDR)
         CALL SULINE (LP,NLINE)
         ENDIF
C
110   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBMDRV'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
      END
