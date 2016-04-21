C MEMBER STLU44
C  (from old member FCEX44)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/23/95.16:29:02 BY $WC30RE
C
C @PROCESS LVL(77)
C
      SUBROUTINE STLU44 (P,Q,TS)
C        TWO-DIMENSION TABLE EVALUATION ROUTINE
C
C     INPUT:
C        P ARRAY - RFS PARAMETER ARRAY FOR REACH ROUTING
C        Q - FLOW ARGUMENT.
C      OUTPUT:
C        TS  - ANSWER INTERPOLATED FROM Q-VS-TS TABLE
C
C    WORD IN P ARRAY:
C     37        NUMBER OF POINTS IN TABLE.
C     39        TABLE IS PAIRS OF Q/TS POINTS,
C               IN ASSENDING Q ORDER, BEGINNING
C               WITH 1ST POINT IN P(39)-P(40).
      DIMENSION P(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sarroute/RCS/stlu44.f,v $
     . $',                                                             '
     .$Id: stlu44.f,v 1.2 1996/03/21 14:22:14 page Exp $
     . $' /
C    ===================================================================
C
C
C        DO INTERVAL-HALVING SEARCH.
C        FIRST POINT IS AT P(39)-P(40)
C        NUMBER OF POINTS IS AT P(37)
 60   N=NINT(P(37))
      NS=39
      NE=37 + N*2 ! P(NE) IS Q-VALUE OF LAST POINT IN TABLE
C      SEE IF Q IS IN TABLE
      NA=NS
      IF(Q.LT.P(NA)) GO TO 100 ! GO EXTRAPOLATE FROM P(NA) AND P(NA+2)
      IF(Q.EQ.P(NA)) GO TO 200 ! FOUND EQUAL
      NA=NE-2
      IF(Q.GT.P(NE)) GO TO 100
      NA=NE
      IF(Q.EQ.P(NA)) GO TO 200 ! FOUND EQUAL

C      BRACKETTED BETWEEN P(NS) AND P(NE)
 90   NI=NE-NS
      IF(NI.LE.2) THEN ! SEARCH HAS CLOSED TO TWO POINTS.
         NA=NS
         GO TO 100 ! GO INTERPOLATE.
      ENDIF
      NI=NI/2
C      NOW, INSURE THAT NI IS DIVISIBLE BY 2, SINCE IT MOVES IN
C      2-WORD STEPS THROUGH TABLE.
      NI=NI/2
      NI=NI*2

C      CODING ERROR-TEST
      IF(NI.LT.2) WRITE(6,'('' STLU2 ERROR IN SEARCH PROGRAM'')')

      NA=NS+NI
      IF(Q.EQ.P(NA)) GO TO 200 ! FOUND EQUAL.
      IF(Q.LT.P(NA)) THEN ! NA TOO HIGH IN TABLE - SET UPPER LIMIT
         NE=NA
      ELSE                ! TOO LOW IN TABLE - SET LOWER LIMIT
         NS=NA
      ENDIF
      GO TO 90 ! GO HALVE THE INTERVAL AND TRY AGAIN.

C       EXTRAPOLATE/INTERPOLATE FROM P(NA) AND P(NA+2)
 100  RATIO=(Q-P(NA) )/(P(NA+2)-P(NA) )
      TS=P(NA+1) + (RATIO*(P(NA+3)-P(NA+1) ) )
      RETURN

C       FOUND ACTUAL EQUAL POINT
 200  TS=P(NA+1)
      RETURN

      END
