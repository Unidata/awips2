C MEMBER FCMPK7
C  (from old member FCEX7)
C-----------------------------------------------------------------------
C
      SUBROUTINE FCMPK7(X1,X2,Y0,Y1,Y2,NP,TAB,XTA,CONK,QDT,IB)
C.......................................................................
C
C     THIS SUBROUTINE PERFORMS THE ATTENUATION OPERATION.
C     X1,X2,Y0,AND Y1 ARE KNOWN.
C     Y2 IS CALCULATED.
C.......................................................................
C
C     SUBROUTINE ORIGINALLY PROGRAMMED BY
C                 GEORGE F. SMITH - HRL   DECEMBER 1979
C.......................................................................
C
C        VARIABLES IN ARGUMENT LIST
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
      INCLUDE 'common/fdbug'
C
      DIMENSION TAB(1)
C
      LOGICAL CONK,QDT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_lagk/RCS/fcmpk7.f,v $
     . $',                                                             '
     .$Id: fcmpk7.f,v 1.1 1995/09/17 18:57:36 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF(IB.EQ.1)WRITE(IODBUG,901)X1,X2,Y0,Y1,NP,XTA,CONK,QDT
  901 FORMAT(1H0,10X,17H** FCMPK7 ENTERED/
     1  10X,29HX1,X2,Y0,Y1,NP,XTA,CONK,QDT =/10X,4F10.2,I10,F10.2,2L4)
C
      YE=(3.0*Y1-Y0)/2.
      KNT=0
C
    5 KNT=KNT+1
      XK=FSERC7(LXXXX,YE,NP,TAB)
      IF(XK.GT.XTA/4.0.OR..NOT.CONK)GO TO 10
C
C.......................................................................
C
C     IF CONSTANT K - I.E. IF CONK = TRUE - AND K LT ROUTING
C     INTERVAL/4.0 - DO NOT ATTENUATE INFLOW.
C.......................................................................
C
      Y2=X2
      GO TO 30
   10 IF(XK.GT.XTA/4.0)GO TO 20
C.......................................................................
C
C     IF QDT TRUE - I.E. IF CALLING FCMPK7 FROM FQDT7 - HAVE ALREADY
C     QUARTERED THE ROUTING INTERVAL - SO CAN DO NO MORE.
C     DO NOT ATTENUATE INFLOW.
C
C     IF QDT FALSE - I.E. IF CALLING FCMPK7 FROM FK7 - SET QDT = TRUE
C     WHICH WILL CAUSE FQDT7 TO BE CALLED.
C.......................................................................
C
      IF(QDT)Y2=X2
      QDT=.TRUE.
      GO TO 30
C
C.......................................................................
C
C     STORE OUTFLOW IN Y2 AND CHECK CONVERGENCE WITH YE
C.......................................................................
C
   20 TR=0.5*XTA
      Y2=(TR*(X1+X2) + Y1*(XK-TR))/(XK+TR)
      IF(1.02*YE.GT.Y2.AND.0.98*YE.LT.Y2)GO TO 30
      YE=Y2
      IF(KNT.LE.20)GO TO 5
C
C.......................................................................
C
C     IF ESTIMATE OF AVG Q DOES NOT CONVERGE USE LAST GUESS AFTER
C     TWENTY TRIES.
C.......................................................................
C
      IF(IB.EQ.1)WRITE(IODBUG,900)
  900 FORMAT(1H0,10X,28HIN FCMPK7 - DID NOT CONVERGE)
C
   30 IF(IB.EQ.1)WRITE(IODBUG,902)Y2
  902 FORMAT(1H0,10X,17H** LEAVING FCMPK7,7H, Y2 = ,F10.2)
C
      RETURN
      END
