C MEMBER LU3L51
C DESC LOCATE BRACKETING POINTS ON A LINE. CALLED BY TLU3.
C
C@PROCESS LVL(77)
C
      SUBROUTINE LU3L51(ISWD,E1,E2,T,NSCI,E1N,QN,KE)
C
CC  KSH START
C NOTE: THIS IS FROM SSARR ROUTINE STLU3L(ISWD,E1,E2,T,NSCI,E1N,QN,KE)
CC  KSH END
C
      DIMENSION T(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ssarresv/RCS/lu3l51.f,v $
     . $',                                                             '
     .$Id: lu3l51.f,v 1.1 1996/03/21 13:42:27 page Exp $
     . $' /
C    ===================================================================
C
C       NWP - NUMBER OF WORDS PER POINT
      KE=0
      NWP=3
      N=NSCI
C       SWITCH ISWD SHOWS DIRECTION OF LINE SEARCH.
C       IF ISWD=0, NO DIRECTION HAS BEEN ESTABLISHED.
      IF(ISWD.EQ.0) THEN
          ISWA=1
      ELSE
          ISWA=ISWD
      ENDIF
      NL=0
      NU=0
C      SCAN TABLE LOOKING FOR POINTS BRACKETING E2.
200   IF(ISWA.LT.0) GO TO 300
C      FORWARD, SO FIND A POINT .LE. E2
C
CC  KSH CHANGE START
      NSAVE=N
CC  KSH CHANGE END
C
210   IF(T(N+2).EQ.E2) GO TO 500
      IF(T(N+2).LT.E2) THEN
C      GOT ONE
              NL=N
C
CC  KSH CHANGE START
CC  COULD NOT FIND ANY VALUE LESS THAN E2
CC  USE THE LOWEST POINT AS DEFAULT
CC              IF(T(N+NWP).LT.-99999.) GO TO 250
              IF(T(N+NWP).LT.-99999.) THEN
                IF(ISWD.EQ.0) GO TO 250
                N=NSAVE
                KE=1 
                GO TO 500
              END IF
CC  KSH CHANGE END
C
              N=N+NWP
              IF(T(N).EQ.T(NL)) GO TO 210
C      OFF END OF LINE. TRY THE NEXT LINE.
              NL=0
              GO TO 210
      ELSE
C      FOUND POINT .GT. E2
              IF(NL.EQ.0) THEN
C      NO LOWER POINT ESTABLISHED FOR BRACKET
C
CC  KSH CHANGE START
CC  COULD NOT FIND ANY VALUE LESS THAN E2
CC  USE THE LOWEST POINT AS DEFAULT
CC                   IF(T(N+NWP).LT.-99999.) GO TO 250
                   IF(T(N+NWP).LT.-99999.) THEN
                     IF(ISWD.EQ.0) GO TO 250
                     N=NSAVE
                     KE=1
                     GO TO 500
                   END IF
CC  KSH CHANGE END
C
                   N=N+NWP
                   GO TO 210
              ELSE
C      INTERPOLATE FOR E1 AT THIS VALUE OF Q
                   IF(T(NL+2).EQ.T(N+2)) GO TO 490
                   IF(T(NL).NE.T(N)) THEN
                        WRITE(6,'('' STLU3L PGM ERROR. NSCI,N,NL;'',
     1                  3I5/(6F11.3))') NSCI,N,NL,(T(I),I=NL,N+2)
                        STOP 'STLU3L ERROR'
                   ELSE
                        E1N=T(NL+1) + (T(N+1)-T(NL+1))*
     1                      (E2-T(NL+2))/(T(N+2)-T(NL+2))
                        QN=T(NL)
                        NSCI=NL
                        RETURN
                   ENDIF
               ENDIF
      ENDIF
C
C      COULD NOT FIND BRACKETTING POINTS GOING UP.
250   IF(ISWD.NE.0) GO TO 400
C
C      SEARCH BACKWARD FOR BRACKETTING POINTS
300   IF(T(N+2).EQ.E2) GO TO 500
      IF(T(N+2).GT.E2) THEN
C      GOT ONE
              NU=N
              IF(N.LE.1) GO TO 400
              N=N-NWP
              IF(T(N).EQ.T(NU)) GO TO 300
C      OFF END OF LINE. TRY THE NEXT LINE.
              NU=0
              GO TO 300
      ELSE
C      FOUND POINT .LE. E2
              IF(NU.EQ.0) THEN
C      NO LOWER POINT ESTABLISHED FOR BRACKET
                   IF(N.LE.1) GO TO 400
                   N=N-NWP
                   GO TO 300
              ELSE
C      INTERPOLATE FOR E1 AT THIS VALUE OF Q
                   IF(T(N+2).EQ.T(NU+2)) GO TO 490
                   IF(T(N).NE.T(NU)) THEN
                        WRITE(6,'('' STLU3L PGM ERROR. NSCI,N,NU;'',
     1                  3I5/(6F11.3))') NSCI,N,NU,(T(I),I=N,NU+2)
                        STOP 'STLU3L ERROR'
                   ELSE
                        E1N=T(N+1) + (T(NU+1)-T(N+1))*
     1                      (E2-T(N+2))/(T(NU+2)-T(N+2))
                        QN=T(N)
                        NSCI=N
                        RETURN
                   ENDIF
               ENDIF
      ENDIF
C
C      COULD NOT FIND BRACKETTING POINTS
C
CC  KSH CHANGE START
CC  USE N=1 AS DEFAULT
CC400   NSCI=0
CC      E1N=0.
CC      Q=0.
CC      RETURN
 400  N=1
      KE=1
      GO TO 500
CC  KSH CHANGE END
C
490   WRITE(6,'('' STLU3L ERROR - FLAT SPOT IN'',
     1      '' C2 TABLE.  ARG1, ARG2, NSCI ='',2F11.3,I5/
     2      '' 1ST 2 POINTS IN TABLE:'',6F11.3)') E1,E2,N,
     3      (T(I),I=1,6)
      STOP 'C2 TABLE ERROR'
C       EXACT POINT
500   NSCI=N
      QN=T(N)
      E1N=T(N+1)
      RETURN
      END
