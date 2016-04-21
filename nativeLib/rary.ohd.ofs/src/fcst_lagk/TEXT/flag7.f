C MODULE FLAG7
C-----------------------------------------------------------------------
C
      SUBROUTINE FLAG7 (P,C,QA,QB,QT,NDT,COTIME,IB)
C.......................................................................
C
C     THIS SUBROUTINE CONTROLS THE LAG OPERATION.
C.......................................................................
C
C     SUBROUTINE ORIGINALLY PROGRAMMED BY
C                 GEORGE F. SMITH - HRL   DECEMBER 1979
C           MODIFIED TO ADD NEW INTERPOLATION FUNCTION TO BE USED
C           FOR THE MULTIPLE INTERCEPT PROBLEM - JML  - HRL 10/92
C.......................................................................
C
C        VARIABLES IN ARGUMENT LIST
C
C        1. P      - THE P ARRAY
C        2. C      - THE C ARRAY
C        3. QA     - THE INFLOW TIME SERIES
C        4. QB     - THE LAGGED OUTFLOW TIME SERIES
C        5. QT     - WORK SPACE
C        6. NDT    - THE NUMBER OF TIME STEPS TO BE EXECUTED
C        7. COTIME - TIME (IN HOURS RELATIVE TO START OF RUN)
C                    AT WHICH CARRYOVER WILL BE SAVED
C        8. IB     - PRINT DEBUG FLAG, PRINT IF IB = 1
C.......................................................................
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcpuck'
      COMMON/FCTIME/IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD,
     1  NOW(5),LOCAL,NOUTZ,NOUTDS,NLSTZ,IDA,IHR,LDA,LHR,IDADAT
      COMMON/RESLAG/IGAGE,TLAG(2)
C
      DIMENSION P(1),C(1),QA(1),QB(1),QT(1),CONLQ(2)
C
      LOGICAL CONLAG,ADD,DBLBK,NOTCPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_lagk/RCS/flag7.f,v $
     . $',                                                             '
     .$Id: flag7.f,v 1.3 2000/03/13 20:51:59 page Exp $
     . $' /
C    ===================================================================
C
      DATA ICPU/4HCPU /
C
C
      NOTCPU=.TRUE.
C
      IF(IB.EQ.1.AND.IFBUG(ICPU).EQ.1)NOTCPU=.FALSE.
C.......................................................................
C
C     PRINT DEBUG INFORMATION.
C.......................................................................
C
      IF(IB.EQ.1)WRITE(IODBUG,*) 'ENTER FLAG7'
C
      IF(NOTCPU)GO TO 700
      CALL URTIMR(LAPSE,ICPUT)
      ELAPSE=LAPSE/100.
      XICPUT=ICPUT/100.
      WRITE(IODBUG,701)ELAPSE,XICPUT
  701 FORMAT(1H0,10X,'** ELAPSED CPU TIME = ',F13.2,
     1  ', TOTAL CPU TIME = ',F13.2)
C
  700 ADD=.TRUE.
      NP=P(16)
      NC=C(1)
      IF(IB.NE.1)GO TO 5
      CALL FPRPC7(NP,P,NC,C)
      WRITE(IODBUG,900)(QA(I),I=1,NDT)
  900 FORMAT(1H0,10X,26HIN SUBROUTINE FLAG7 - QA =/(1X,10F12.3))
C
    5 ITA=P(5)
      NPLQ=P(19)
C
      XOUT0=C(3)
      MXLCO=C(5)
C
      LQT=1
      LC=6
      I2=2
C.......................................................................
C
C     MOVE OLD CARRYOVER TO QT OR TO NEW CARRYOVER LOCATIONS.
C.......................................................................
C
      IF(MXLCO.EQ.0)GO TO 100
C
      DO 20 I=1,MXLCO
      IF(C(5+I*2).EQ.0.0)GO TO 100
      IF(C(5+I*2).GT.COTIME)GO TO 10
      CALL UMEMOV(C(4+I*2),QT(LQT),I2)
      LQT=LQT+2
      GO TO 20
C
   10 C(LC)=C(4+I*2)
      C(LC+1)=C(5+I*2) - COTIME
      LC=LC+2
   20 CONTINUE
C.......................................................................
C
C     ZERO REMAINDER OF C ARRAY.
C.......................................................................
C
  100 Z=0.0
      IFIL=NC-LC
      IF(IFIL.GT.0)CALL UMEMST(Z,C(LC+1),IFIL)
C
C.......................................................................
C
C     IF NO TIME STEPS ARE DESIRED - RETURN
C
C     ELSE - SET CONSTANT LAG SWITCH.
C.......................................................................
C
      IF(NDT.LE.0)RETURN
C
      CONLAG=.FALSE.
      IF(IFIX(P(19)).NE.0)GO TO 110
      CONLAG=.TRUE.
      CONLQ(1)=P(20)
      CONLQ(2)=1.E20
      NPLQ=1
C
C.......................................................................
C
C     INTERPOLATE TO FIND LAGS IN FUNCTION FSERC7.
C     STORE THE LAGS IN EITHER QT OR C AS IS APPROPRIATE.
C.......................................................................
C
  110 IF(NOTCPU)GO TO 702
      CALL URTIMR(LAPSE,ICPUT)
      ELAPSE=LAPSE/100.
      XICPUT=ICPUT/100.
      WRITE(IODBUG,703)ELAPSE,XICPUT
  703 FORMAT(1H0,10X,'**ABOUT TO LAG INFLOW - ELAPSED CPU TIME = ',
     1  F13.2,', TOTAL CPU TIME = ',F13.2)
C
  702 DO 130 I=1,NDT
      CURENT=I*ITA
      IF(CONLAG)TIME=FSERC7(LXXXX,QA(I),NPLQ,CONLQ)
      IF(.NOT.CONLAG)TIME=FSERC7(LXXXX,QA(I),NPLQ,P(20))
      IF(IGAGE.GE.1) TLAG(IGAGE)=TIME
C
      IF(TIME+CURENT.GT.COTIME)GO TO 120
      IF(LQT.LT.(MXLCO+NDT)*2)GO TO 115
      WRITE(IPR,901)
  901 FORMAT(1H0,10X,8H** ERROR/
     1  11X,47HATTEMPT TO WRITE BEYOND THE END OF THE QT ARRAY)
      CALL ERROR
      GO TO 200
C
  115 QT(LQT)=QA(I)
      QT(LQT+1)=TIME+CURENT
      LQT=LQT+2
      GO TO 130
C
  120 IF(LC.LT.NC)GO TO 125
      WRITE(IPR,902)
  902 FORMAT(1H0,10X,8H** ERROR/
     1  11X,46HATTEMPT TO WRITE BEYOND THE END OF THE C ARRAY)
      CALL ERROR
      GO TO 200
C
  125 C(LC)=QA(I)
      C(LC+1)=TIME + CURENT - COTIME
      LC=LC+2
  130 CONTINUE
C
C.......................................................................
C
C     SHIFT VALUES IN QT ARRAY AHEAD AND INSERT CURRENT OUTFLOW
C     VALUE FROM CARRYOVER.
C.......................................................................
C
  200 LQT=LQT-1
      IF(LQT.EQ.0)GO TO 215
      DO 210 I=1,LQT
      J=LQT-I+1
      QT(J+2)=QT(J)
  210 CONTINUE
C
  215 QT(1)=XOUT0
      QT(2)=0.
      LQT=LQT+2
C.......................................................................
C
C     IF THE FIRST CARRYOVER TIME IS NONZERO INSERT IT AND Q VALUE
C     AT THE END OF THE QT ARRAY.
C.......................................................................
C
      IF(C(7).EQ.0.0)GO TO 218
      QT(LQT+1)=C(6)
      QT(LQT+2)=C(7) + COTIME
      LQT=LQT+2
C
  218 IF(IB.GE.1)WRITE(IODBUG,905)(QT(I),I=1,LQT)
C.......................................................................
C
C     PROCESS QT ARRAY USING ATLANTA METHOD OF REMOVING
C     DOUBLE BACK VALUES BY SUMMING ALL PARTS OF THE
C     ARRAY THAT FALL 'INSIDE' THE CURVE AT ANY GIVEN TIME.
C
C.......................................................................
C
C     FIRST REMOVE ANY SEQUENTIAL FLOWS WHICH HAVE EXACTLY THE
C     SAME TIMES ASSOCIATED WITH THEM.  AVERAGE ALL SEQUENTIAL
C     FLOWS AT SAME TIME AND STORE AS ONE FLOW AT THAT TIME.
C.......................................................................
C
      IF(NOTCPU)GO TO 704
      CALL URTIMR(LAPSE,ICPUT)
      ELAPSE=LAPSE/100.
      XICPUT=ICPUT/100.
      WRITE(IODBUG,705)ELAPSE,XICPUT
  705 FORMAT(1H0,10X,'**ABOUT TO REMOVE IDENTICAL LAG TIMES -  ',
     1  'ELAPSED CPU TIME = ',F13.2,', TOTAL CPU TIME = ',F13.2)
C
  704 LQTM2=LQT-2
      IF(LQTM2.GT.0)GO TO 219
      DO 230 I=1,NDT
  230 QB(I)=QT(1)
      GO TO 500
C
  219 L=0
      DO 222 I=2,LQTM2,2
      IF(I.LE.L)GO TO 222
      K=I+2
      IF(QT(K).NE.QT(I))GO TO 222
  220 L=K
      IF(K+2.GT.LQT)GO TO 235
      IF(QT(K+2).NE.QT(K))GO TO 235
      K=K+2
      GO TO 220
C
  235 SUM=0.
      DO 236 KK=I,K,2
  236 SUM=SUM+QT(KK-1)
      QT(I-1)=SUM/((L-I)/2+1)
      IBEGN=I+2
      DO 237 KK=IBEGN,L,2
  237 QT(KK)=-999.
C
  222 CONTINUE
C
      LQTN=LQT
  245 LQT=LQTN
      DO 240 KK=2,LQT,2
      IF(QT(KK).GT.-998.5)GO TO 240
      LQTN=LQT-2
      LQTEND=LQT-1
      DO 238 KKK=KK,LQTEND
  238 QT(KKK-1)=QT(KKK+1)
      GO TO 245
  240 CONTINUE
C
C     CHECK FOR DOUBLE BACK OF QT ARRAY -
C     IF NO DOUBLE BACK CAN PROCESS MORE EFFICIENTLY IN
C      NEXT SECTION OF CODING.
C
      DBLBK=.FALSE.
      LQTM2=LQT-2
      IF(LQTM2.EQ.0)GO TO 242
      DO 241 I=2,LQTM2,2
      IF(QT(I).LT.QT(I+2))GO TO 241
      DBLBK=.TRUE.
      GO TO 242
  241 CONTINUE
C
  242 IF(NOTCPU)GO TO 708
      CALL URTIMR(LAPSE,ICPUT)
      ELAPSE=LAPSE/100.
      XICPUT=ICPUT/100.
      WRITE(IODBUG,709)DBLBK,ELAPSE,XICPUT
  709 FORMAT(1H0,10X,'**ABOUT TO COMPUTE QB - DBLBK = ',L4/11X,
     1  'ELAPSED CPU TIME = ',F13.2,', TOTAL CPU TIME = ',F13.2)
C
  708 IF(DBLBK)GO TO 243
C
C     NO DOUBLEBACKS IF AT THIS CODING
C.......................................................................
C
      LOCQT=1
      NUMQT=LQT
C
      DO 400 I=1,NDT
      TSTAR=I*ITA
      QB(I)=FSERC7(LX,TSTAR,NUMQT,QT(LOCQT))
      LOCQT=LX*2-1
      NUMQT=LQT-LOCQT+1
  400 CONTINUE
      GO TO 500
C     *** HAVE DOUBLEBACKS TO GET HERE ***
C.......................................................................
C
C     NOW CHECK QT VALUE AGAINST TIMES WANTED FOR OUTFLOW TIME SERIES
C
  243 DO 300 I=1,NDT
C
C     TSTAR IS THE TIME VALUES WILL BE SUMMED FOR
C
      TSTAR=I*ITA
C
      Q=0.
C
      DO 290 J=2,LQT,2
C
C     IF TIME IN QT ARRAY (EVEN LOCATIONS - PAIRS ARE Q,T) IS EQ TO
C     TSTAR CHECK WHETHER SURROUNDING VALUES ARE GT OR LT
C
      IF(QT(J).NE.TSTAR)GO TO 250
C
C     IF AT 1ST PAIR IN QT ONLY CHECK NEXT PAIR NOT PREVIOUS PAIR
C
      IF(J.GT.2)GO TO 247
C
C     IF ALSO AT LAST PAIR SET QB(I) TO QT(1)
C
      IF(J.LT.LQT)GO TO 246
      Q=Q+QT(1)
      GO TO 295
C
  246 IF(QT(J+2).LE.TSTAR)GO TO 249
      Q=Q+QT(J-1)
      ADD=.TRUE.
      GO TO 290
C
  249 IF(QT(J+2).GE.TSTAR)GO TO 290
      Q=Q-QT(J-1)
      ADD=.FALSE.
      LASTJ=J-1
      TLAST=TSTAR
      GO TO 290
C
C     IF AT LAST PAIR IN QT ONLY CHECK PREVIOUS PAIR, NOT NEXT PAIR
C
  247 IF(J.LT.LQT)GO TO 251
C
      IF(QT(J-2).LE.TSTAR)GO TO 248
      Q=Q-QT(J-1)
      ADD=.FALSE.
      LASTJ=J-1
      TLAST=TSTAR
      GO TO 290
C
  248 IF(QT(J-2).GE.TSTAR)GO TO 290
      Q=Q+QT(J-1)
      ADD=.TRUE.
      GO TO 290
C
C     GET HERE IF QT(J) EQ TSTAR AND NOT AT BEGINNING OR END OF
C        QT ARRAY
C
C     IF TIME OF PREVIOUS QT PAIR IS LT TSTAR AND TIME OF NEXT
C     QT PAIR IS GT TSTAR -- ADD CURRENT FLOW
C
  251 IF(QT(J-2).GE.TSTAR.OR.QT(J+2).LE.TSTAR)GO TO 252
      Q=Q+QT(J-1)
      ADD=.TRUE.
      GO TO 290
C
C     IF TIME OF PREVIOUS QT PAIR IS GT TSTAR AND TIME OF NEXT
C     QT PAIR IS LT TSTAR -- SUBT CURRENT FLOW
C
  252 IF(QT(J-2).LE.TSTAR.OR.QT(J+2).GE.TSTAR)GO TO 290
      Q=Q-QT(J-1)
      ADD=.FALSE.
      LASTJ=J-1
      TLAST=TSTAR
C
C     IF NEITHER OF THE ABOVE CASES THEN AT EDGE OF QT CURVE --
C     SO DON'T ADD OR SUBT CURRENT FLOW
C
      GO TO 290
C
C     HERE QT(J) NE TSTAR
C     IF QT(J) LT TSTAR AND QT(J+2) GT TSTAR -- ADD CURRENT FLOW
C
C     IF AT END OF QT ARRAY CONTINUE
C
  250 IF(J.EQ.LQT)GO TO 290
C
      IF(QT(J).GE.TSTAR.OR.QT(J+2).LE.TSTAR)GO TO 260
C
CC      Q=Q+FSERC7(LXXXX,TSTAR,2,QT(J-1))
      QADD=FINTP7(TSTAR,QT(J-1))
      Q=Q+QADD
C     WRITE(6,9999) I,J,QT(J),QT(J+2),TSTAR,QADD,Q
C9999 FORMAT('  I  J     QT(J)    QT(J+2)     TSTAR      QADD         Q'
C    ./I3,I3,5F10.3)
      ADD=.TRUE.
      GO TO 290
C
C     IF CURRENT TIME IS GT TSTAR AND NEXT TIME IS LT TSTAR --
C       SUBT CURRENT FLOW
C
  260 IF(QT(J).LE.TSTAR.OR.QT(J+2).GE.TSTAR)GO TO 290
C
CC      Q=Q-FSERC7(LXXXX,TSTAR,2,QT(J-1))
      QSUB=FINTP7(TSTAR,QT(J-1))
      Q=Q-QSUB
C     WRITE(6,9998) I,J,QT(J),QT(J+2),TSTAR,QSUB,Q
C9998 FORMAT('  I  J     QT(J)    QT(J+2)     TSTAR      QSUB         Q'
C    ./I2,I3,5F10.3)
C
      ADD=.FALSE.
      LASTJ=J-1
      TLAST=TSTAR
C
  290 CONTINUE
C
  295 IF(.NOT.ADD)Q=Q+FSERC7(LXXXX,TLAST,2,QT(LASTJ))
C     WRITE(6,9997) I,TSTAR,Q
C9997 FORMAT(5X,'===== I=',I3,3X,'TSTAR=',F10.2,3X,'Q=',F10.3,'===='/)
C
      QB(I)=Q
C
  300 CONTINUE
C
  500 IF(NOTCPU)GO TO 706
      CALL URTIMR(LAPSE,ICPUT)
      ELAPSE=LAPSE/100.
      XICPUT=ICPUT/100.
      WRITE(IODBUG,707)ELAPSE,XICPUT
  707 FORMAT(1H0,10X,'**LEAVING FLAG7 - ELAPSED CPU TIME = ',F13.2,
     1  ', TOTAL CPU TIME = ',F13.2)
C
  706 IF(IB.NE.1)RETURN
C
C.......................................................................
C
C     PRINT DEBUG INFORMATION IF DESIRED.
C.......................................................................
C
      WRITE(IODBUG,903)(QB(I),I=1,NDT)
  903 FORMAT(1H0,10X,4HQB =/(1X,10F12.3))
      WRITE(IODBUG,904)(C(I),I=1,NC)
  904 FORMAT(1H0,10X,3HC =/(1X,10F12.3))
      NQT=(NDT+C(5))*2
      WRITE(IODBUG,905)(QT(I),I=1,NQT)
  905 FORMAT(1H0,10X,4HQT =/(1X,10F12.3))
      WRITE(IODBUG,906)NDT,COTIME,LQT
  906 FORMAT(' EXIT FLAG7: NDT=',I10,' COTIME=',F10.2,' LQT=',I10)
C
      RETURN
      END
