C MODULE SBGRDF
C-----------------------------------------------------------------------
C
      SUBROUTINE SBGRDF (X,Y,NBPTS,IY,IXB,IXE,NSEGS,IYP,LFACTR,ISTAT)
C
C  THIS ROUTINE DETERMINES THE SLOPE OF THE LINE CONNECTING PAIRS OF
C  BASIN BOUNDARY GRID POINTS AND CALLS THE APPROPRIATE ROUTINE TO 
C  COMPUTE THE GRID POINT DEFINITION.
C
      INCLUDE 'scommon/sudbgx'
C
      DIMENSION X(NBPTS),Y(NBPTS)
      DIMENSION IY(NSEGS),IXB(NSEGS),IXE(NSEGS)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbgrdf.f,v $
     . $',                                                             '
     .$Id: sbgrdf.f,v 1.4 2004/06/28 14:08:44 xfan Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBGRDF'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('GRID')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NBPTS=',NBPTS
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      DO 20 IP=1,NBPTS
         IJ=IP+1
         IF (IP.EQ.NBPTS) IJ=1

CFAN (begin)
CFAN HSD-bug r25-20 OHRFC  06/01/04
CFAN
CFAN In the right side boundary, Y(IP) can't be integer while
CFAN    Y(IP+1) and Y(IP-1) are less    than Y(IP)
CFAN In the left  side boundary, Y(IP) can't be integer while
CFAN    Y(IP+1) and Y(IP-1) are greater than Y(IP)
CFAN 
         IF (IP.GE.2) THEN
          IF(Y(IP)/LFACTR*LFACTR .EQ. Y(IP) 
     &      .AND. Y(IP+1) .LT. Y(IP)
     &      .AND. Y(IP-1) .LT. Y(IP)) THEN
            Y(IP)=Y(IP)-0.0001
          ENDIF
          IF(Y(IP)/LFACTR*LFACTR .EQ. Y(IP) 
     &      .AND. Y(IP+1) .GT. Y(IP)
     &      .AND. Y(IP-1) .GT. Y(IP)) THEN
            Y(IP)=Y(IP)+0.0001
          ENDIF
         ENDIF
   
CFAN (end)

         YDEL=Y(IJ)-Y(IP)
         XDEL=X(IJ)-X(IP)

C -- LWU        IF (ABS(YDEL).LT.0.001) GO TO 20
         IF (ABS(YDEL).LT.0.0001) GO TO 20
         IF (YDEL.GT.0.0)  GO TO 10
C        SLOPE OF CURRENT LINE SEGMENT IS NEGATIVE
            CALL SBYNEG (X,Y,XDEL,YDEL,IP,NBPTS,IY,IXB,IXE,NSEGS,IYP,
     *         LFACTR,ISTAT)
            GO TO 20
C     SLOPE OF CURRENT LINE SEGMENT IS POSITIVE
10       CALL SBYPOS (X,Y,XDEL,YDEL,IP,NBPTS,IY,IXB,IXE,NSEGS,IYP,
     *      LFACTR,ISTAT)
20       CONTINUE
C
      IF (LDEBUG.GT.0) THEN
         DO 30 I=1,NBPTS
           WRITE (IOSDBG,'(1X,A,I4,2(1X,A,F8.3))')
     *        'I=',I,'X(I)=',X(I),'Y(I)=',Y(I)
            CALL SULINE (IOSDBG,1)
30       CONTINUE
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBGRDF'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
      END
