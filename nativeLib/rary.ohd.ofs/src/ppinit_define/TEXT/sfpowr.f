C MODULE SFPOWR
C-----------------------------------------------------------------------
C
      SUBROUTINE SFPOWR (LX,LY,IPARM,ITYPE,POWER,ISTAT)
C
C  ROUTINE TO COMPUTE WEIGHTS FOR STATIONS BASED ON 1/D**POWER.
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sntwkx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfpowr.f,v $
     . $',                                                             '
     .$Id: sfpowr.f,v 1.3 2000/03/13 16:07:28 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SFPOWR'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('WGHT')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'LX=',LX,' LY=',LY,
     *      ' IPARM=',IPARM,' ITYPE=',ITYPE,' POWER=',POWER
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      DMIN=1.E10
C
C  COMPUTATIONAL LOOP
      DO 90 I=1,INWFIL
         GO TO (10,20,30,90,90,90,40),IPARM
10          IF (PPVRNW(I).EQ.0) GO TO 90
            IF (SFLGNW(I)/10.NE.3) GO TO 90
            GO TO 50
20       IF (PP24NW(I).EQ.0) GO TO 90
         IF (PP24NW(I).GT.99999) GO TO 90
         IF (SFLGNW(I)/10.LT.2) GO TO 90
         GO TO 50
30       IF (TA24NW(I).EQ.0) GO TO 90
         IF (SFLGNW(I)-(SFLGNW(I)/10*10).NE.2) GO TO 90
         GO TO 50
40       IF (EA24NW(I).EQ.0) GO TO 90
C     COMPUTE STATION DIFFERENCES
50       KX=CORDNW(1,I)
         KY=CORDNW(2,I)
         DELX=KX-LX
         DELY=KY-LY
         D=SQRT(DELX*DELX+DELY*DELY)
         IF (ITYPE.EQ.2) GO TO 60
         GO TO 70
60       IF (D.GE.DMIN) GO TO 90
         DMIN=D
         IMIN=I
         GO TO 90
C     COMPUTE WEIGHT FOR STATION
70       IF (D.GT.0.01) GO TO 80
         WORKNW(I)=WORKNW(I)+1/((.01)**POWER)
         GO TO 90
80       WORKNW(I)=WORKNW(I)+1./D**POWER
90       CONTINUE
C
      IF (ITYPE.NE.2) GO TO 100
         WORKNW(IMIN)=WORKNW(IMIN)+1
C
100   IF (ITYPE.NE.2) GO TO 110
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'DMIN=',DMIN,' IMIN=',IMIN,
     *      ' WORKNW(IMIN)=',WORKNW(IMIN)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
110   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SFPOWR'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
      END
