C MEMBER FQDT7
C  (from old member FCEX7)
C-----------------------------------------------------------------------
C
      SUBROUTINE FQDT7(X1,X2,Y0,Y1,Y2,NP,TAB,XTA,CONK,QDT,IB)
C
      INCLUDE 'common/fdbug'
C
      DIMENSION TAB(1)
C
      LOGICAL CONK,QDT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_lagk/RCS/fqdt7.f,v $
     . $',                                                             '
     .$Id: fqdt7.f,v 1.1 1995/09/17 18:58:06 dws Exp $
     . $' /
C    ===================================================================
C
C
C.......................................................................
C
C     THIS SUBROUTINE IS CALLED IF VARIABLE K IS ENABLED AND
C     K FOUND FROM INTERPOLATION OF K,Q TABLE IS LT ROUTING
C     INTERVAL/4.0
C
C     NOTE - ROUTINE INTERVAL IS ALREADY ONE-HALF OF ORIGINAL INTERVAL
C.......................................................................
C
C     SUBROUTINE ORIGINALLY PROGRAMMED BY
C                GEORGE F. SMITH - HRL   DECEMBER 1979
C.......................................................................
C
C      VARIABLES IN ARGUMENT LIST
C
C        1. X1   - INFLOW AT BEGINNING OF INTERVAL
C        2. X2   - INFLOW AT END OF INTERVAL
C        3. Y0   - PREVIOUS OUTFLOW
C        4. Y1   - OUTFLOW AT BEGINNING OF INTERVAL
C        5. Y2   - OUTFLOW AT END OF INTERVAL
C        6. NP   - NUMBER OF PAIRS IN (K,Q) TABLE
C        7. TAB  - (K,Q) TABLE
C        8. CONK - LOGICAL VARIABLE TRUE IF K ENABLED
C        9. QDT  - LOGICAL VARIABLE TRUE IF CALLING FCMPK7 FROM FQDT7
C                                   FALSE IF CALLING FCMPK7 FROM FK7
C       10. IB   - DEBUG PRINT FLAG - PRINT IF IB = 1
C.......................................................................
C
      IF(IB.EQ.1)WRITE(IODBUG,900)X1,X2,Y0,Y1,NP,XTA,CONK,QDT
  900 FORMAT(1H0,10X,16H** FQDT7 ENTERED/
     1  10X,29HX1,X2,Y0,Y1,NP,XTA,CONK,QDT =/10X,4F10.2,I10,F10.2,2L4)
C
      XTAT=XTA/4.0
      X2T=X1
      Y1T=Y0*0.25 + Y1*0.75
      Y2T=Y1
C
C.......................................................................
C
C     LOOP THROUGH ROUTING INTERVAL ONE QUARTER PERIOD AT A TIME.
C
C     IF K STILL GT ROUTING INTERVAL DO NOT ATTENUATE.
C.......................................................................
C
      DO 10 I=1,4
      FRAC=I/4.0
      X1T=X2T
      X2T=X1*(1.-FRAC) + X2*FRAC
      Y0T=Y1T
      Y1T=Y2T
C
      CALL FCMPK7(X1T,X2T,Y0T,Y1T,Y2T,NP,TAB,XTAT,CONK,QDT,IB)
C
   10 CONTINUE
C
      Y2=Y2T
      QDT=.FALSE.
C
      IF(IB.EQ.1)WRITE(IODBUG,901)Y2
  901 FORMAT(1H0,10X,16H** LEAVING FQDT7,7H, Y2 = ,F10.2)
C
      RETURN
      END
