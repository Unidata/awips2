C MODULE SFRADI
C-----------------------------------------------------------------------
C
      SUBROUTINE SFRADI (C,NPCPN,COORDS,X,Y,RADINF)
C
C  ROUTINE TO COMPUTE THE RADIUS OF INFLUENCE FOR AN MAP AREA.
C
      INCLUDE 'scommon/sudbgx'
C
      DIMENSION COORDS(2,NPCPN)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfradi.f,v $
     . $',                                                             '
     .$Id: sfradi.f,v 1.2 1999/07/07 11:21:33 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SFRADI'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('WGHT')
C
      IF (LDEBUG.GT.0) THEN
         DO 10 I=1,NPCPN
            WRITE (IOSDBG,'(1X,A,I3,2(1X,A,F7.2))')
     *         'I=',I,
     *         'COORDS(1,I)=',COORDS(1,I),'COORDS(2,I)=',COORDS(2,I)
            CALL SULINE (IOSDBG,1)
10          CONTINUE
         ENDIF
C
C  COMPUTE MAXIMUM DISTANCE
      DISTMX=0.0
      DO 20 I=1,NPCPN
         DELX=X-COORDS(1,I)
         DELY=Y-COORDS(2,I)
         DIST=SQRT(DELX*DELX+DELY*DELY)
         IF (DIST.GT.DISTMX) DISTMX=DIST
20       CONTINUE
C
C  COMPUTE RADIUS OF INFLUENCE
      RADINF=DISTMX*C
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'C=',C,' RADINF=',RADINF
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SFRADI'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
      END
