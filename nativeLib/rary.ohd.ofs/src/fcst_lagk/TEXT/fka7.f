C MEMBER FKA7
C  (from old member FCEX7)
C
      SUBROUTINE FKA7(P,C,QB,NDT,COTIME,IB)
C
C.......................................................................
C
C     THIS SUBROUTINE DOES THE ATTENUATION (K) COMPUTATIONS
C     FOR THE ATLANTA METHOD
C.......................................................................
C
C      SUBROUTINE ORIGINALLY PROGRAMMED BY
C            GEORGE F. SMITH - HRL   DECEMBER 1979
C.......................................................................
C
C      VARIABLES IN ARGUMENT LIST
C        1. P      - THE P ARRAY
C        2. C      - THE C ARRAY
C        3. QB     - INPUT  - LAGGED INFLOW
C                    OUTPUT - ROUTED (ATTENUATED) OUTFLOW
C        4. NDT    - THE NUMBER OF TIME STEPS IN THIS EXECUTION
C        5. COTIME - TIME (IN HOURS RELATIVE TO START OF RUN)
C                    AT WHICH CARRYOVER WILL BE SAVED
C        6. IB     - PRINT DEBUG FLAG, PRINT IF IB = 1
C.......................................................................
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fcpuck'
C
      DIMENSION P(1),C(1),QB(1)
C
      LOGICAL MEANQ,CONK,NOTCPU,LBUG
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_lagk/RCS/fka7.f,v $
     . $',                                                             '
     .$Id: fka7.f,v 1.1 1995/09/17 18:57:59 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA L3/4HL3  /
      DATA ICPU/4HCPU /
C
      LBUG=.FALSE.
      IF(IB.EQ.1)LBUG=.TRUE.
C
      NOTCPU=.TRUE.
      IF(LBUG.AND.IFBUG(ICPU).EQ.1)NOTCPU=.FALSE.
C
      IF(LBUG)WRITE(IODBUG,600)
  600 FORMAT(1H0,10X,15H** FKA7 ENTERED)
C
      IF(NOTCPU)GO TO 700
      CALL URTIMR(LAPSE,ICPUT)
      ELAPSE=LAPSE/100.
      XICPUT=ICPUT/100.
      WRITE(IODBUG,701)ELAPSE,XICPUT
  701 FORMAT(1H0,10X,'** ELAPSED CPU TIME = ',F13.2,
     1  ', TOTAL CPU TIME = ',F13.2)
C
700   IBOS=P(16)+1
      IBOS1=IBOS+1
      NPOS=P(IBOS)
      IBOS4=IBOS+NPOS*2+1
      IBOS14=IBOS4+1
      IBK=P(18)
      NPKQ=P(IBK)
      IBK1=IBK+1
C
      CONK=.FALSE.
      IF(NPKQ.EQ.0)CONK=.TRUE.
C
      IF(.NOT.CONK)GO TO 2
      XK1=P(IBK1)
      XK2=XK1
      XK14=XK1
      XK24=XK1
C
    2 ITA=P(5)
      XITA=ITA/4.
C
      X2=C(2)
      Y1=C(3)
      S2ODT=C(4)*2./ITA
      FACT=1.
      IPWARN=0
C
      DTYPIN=P(4)
      CALL FDCODE(DTYPIN,STDUNT,INDIMS,MSNG,NUPDT,TSCALE,NEXTRA,IER)
      MEANQ=.FALSE.
      IF(INDIMS.EQ.L3)MEANQ=.TRUE.
C
      IF(.NOT.LBUG)GO TO 20
      WRITE(IODBUG,602)IBOS,IBOS1,NPOS,IBOS4,IBOS14,
     1  IBK,NPKQ,IBK1,ITA,XITA,X2,Y1,S2ODT,MEANQ
  602 FORMAT(1H0,10X,47HINITIAL VALUES FOR IBOS,IBOS1,NPOS,IBOS4,IBOS14
     1   ,45H,IBK,NPKQ,IBK1,ITA,XITA,X2,Y1,S2ODT,MEANQ ARE/11X,9I10,
     2    G10.4/11X,3G10.4,2X,L4)
      WRITE(IODBUG,611)(QB(I),I=1,NDT)
  611 FORMAT(1H0,10X,'INFLOW VALUES TO K OPERATION (QB ARRAY)'/
     1  (1X,10G10.4))
      IEOS1=IBOS4-1
      WRITE(IODBUG,612)NPOS,(P(I),I=IBOS1,IEOS1)
  612 FORMAT(1H0,10X,'THE O VS 2*S/DT+O TABLE FOLLOWS - THE NUMBER OF ',
     1  'PAIRS OF VALUES IS ',I3/(11X,8G12.4))
C
      DO 12 I=1,NPKQ
      IF(XITA*2.0.GT.P(IBK+I*2-1))GO TO 15
   12 CONTINUE
C
      WRITE(IODBUG,615)
  615 FORMAT(1H0,10X,'THERE IS NO O VS 2*S/(DT/4)+O TABLE')
      GO TO 20
C
   15 NPOS4=P(IBOS4)
      IEOS14=IBOS4+2*NPOS4
      WRITE(IODBUG,613)NPOS4,(P(I),I=IBOS14,IEOS14)
  613 FORMAT(1H0,10X,'THE O VS 2*S/(DT/4)+O TABLE FOLLOWS - THE ',
     1  'NUMBER OF PAIRS OF VALUES IS ',I3/(11X,8G12.3))
C.......................................................................
C
C     BEGIN OVERALL LOOP FOR THIS PASS THROUGH K OPERATION
C.......................................................................
C
   20 IF(NOTCPU)GO TO 702
      CALL URTIMR(LAPSE,ICPUT)
      ELAPSE=LAPSE/100.
      XICPUT=ICPUT/100.
      WRITE(IODBUG,703)ELAPSE,XICPUT
  703 FORMAT(1H0,10X,'**ABOUT TO START DO 10 LOOP -  ELAPSED CPU TIME ',
     1  '= ',F13.2,', TOTAL CPU TIME = ',F13.2)
C
  702 DO 10 I=1,NDT
C
      X1=X2
      X2=QB(I)
      QPREV=Y1
C
      IF(.NOT.MEANQ)VALUE=X1 + X2 + S2ODT - Y1
      IF(MEANQ)VALUE=2.*X2*24./ITA + S2ODT - Y1
C
      IF(VALUE.LT.1.0E-7)VALUE=0.0
C
      Y2=FSERC7(LXXXX,VALUE,NPOS,P(IBOS1))
C
      IF(LBUG.AND.MEANQ)WRITE(IODBUG,614)VALUE
  614 FORMAT(11X,'IN DO 10 LOOP, VALUE = ',G10.4)
C
      IF(MEANQ)GO TO 9
C
      IF(CONK)GO TO 56
C
      XK1=FSERC7(LXXXX,Y1,NPKQ,P(IBK1))
C
      XK2=FSERC7(LXXXX,Y2,NPKQ,P(IBK1))
C
   56 IF(LBUG)WRITE(IODBUG,603)X1,X2,VALUE,Y2,XK1,XK2
  603 FORMAT(11X,40HIN DO 10 LOOP, X1,X2,VALUE,Y2,XK1,XK2 = ,6G10.4)
C
      IF(XK1.GE.XITA*2.0.AND.XK2.GE.XITA*2.0)GO TO 9
C
      IF(XK1.GT.XITA/2.0.OR.XK2.GT.XITA/2.0)GO TO 4
C
C.......................................................................
C
C     GET HERE IF K FOR Y1 AND K FOR Y2 ARE BOTH LT DT/2
C     SET OUTFLOW = MINIMUM OF (INFLOW,VALUE)
C.......................................................................
C
      Y2=X2
      IF(VALUE.LT.Y2)Y2=VALUE
      IF(LBUG)WRITE(IODBUG,604)Y2
  604 FORMAT(11X,17HK LT DT/2 - Y2 = ,G10.4)
C
      GO TO 9
C
C.......................................................................
C
C     GET HERE IF K FOR Y1 GT DT/2 AND K FOR Y2 LE DT/2 OR VICE VERSA
C     SOLVE EQUATIONS IN THIS LOOP WITH DT=ORIGINAL DT/4
C.......................................................................
C
    4 NPOS4=P(IBOS4)
      S2ODT=S2ODT*4.
      IF(S2ODT.LT.-0.5)IPWARN=IPWARN+1
      DX=X2-X1
C
      IF(LBUG)WRITE(IODBUG,605)NPOS4,S2ODT,DX
  605 FORMAT(11X,41HABOUT TO ENTER DO 5 LOOP - TIME INTERVAL ,
     1   06H= DT/4/11X,17HNPOS4,S2ODT,DX = ,I10,2G10.4)
C
      DXOVR4=DX/4.
      XITAO2=XITA/2.
C
      DO 5 J=1,4
      X14=X1+(J-1)*DXOVR4
      X2=X14+DXOVR4
C
      VALUE=X14 + X2 + S2ODT - Y1
C
      Y2=FSERC7(LXXXX,VALUE,NPOS4,P(IBOS14))
C
      IF(CONK)GO TO 50
      XK14=FSERC7(LXXXX,Y1,NPKQ,P(IBK1))
      XK24=FSERC7(LXXXX,Y2,NPKQ,P(IBK1))
   50 IF(XK14.LT.XITAO2.OR.XK24.LT.XITAO2)GO TO 55
      GO TO 6
C
C.......................................................................
C
C     IF EITHER K FOR Y1 OR K FOR Y2 IS STILL LT NEW DT/2
C     (I.E. ORIGINAL DT/8) SET OUTFLOW=MIN(INFLOW,VALUE) AND CONTINUE IN
C     QUARTER PERIOD LOOP
C.......................................................................
C
   55 IF(LBUG)WRITE(IODBUG,609)Y1,Y2,XK14,XK24,X2
  609 FORMAT(11X,45HK LT DT/8 IN DO 5 LOOP - SET Y2=MIN(X2,VALUE)/
     1   11X,21HY1,Y2,XK14,XK24,X2 = ,5G10.4)
      Y2=X2
      IF(VALUE.LT.Y2)Y2=VALUE
C
    6 S2ODT=VALUE - Y2
      IF(S2ODT.LT.-0.5)IPWARN=IPWARN+1
C
      IF(LBUG)WRITE(IODBUG,606)J,X14,X2,VALUE,Y2,S2ODT
  606 FORMAT(11X,41HIN DO 5 LOOP - J,X14,X2,VALUE,Y2,S2ODT = ,
     1   I10,5G10.4)
C
      Y1=Y2
C
    5 CONTINUE
C
      FACT=4.
C
 9    QB(I)=Y2
C
      S2ODT=(VALUE - Y2)/FACT
C
      IF(S2ODT.LT.-0.5)IPWARN=IPWARN+1
      FACT=1.
C
      IF(LBUG)WRITE(IODBUG,607)Y2,S2ODT
  607 FORMAT(11X,27HIN DO 10 LOOP - Y2,S2ODT = ,2G10.4)
C
      Y1=Y2
C
   10 CONTINUE
C.......................................................................
C
C     STORE CARRYOVER - IF C(4), CURRENT STORAGE, IS LT ZERO
C     BECAUSE OF ROUNDOFF ERROR SET TO ZERO
C.......................................................................
C
      C(2)=X2
      C(3)=Y2
      C(4)=S2ODT*ITA/2.
C
      IF(C(4).LT.0.0)C(4)=0.0
C
      IF(LBUG)WRITE(IODBUG,608)C(2),C(3),C(4)
  608 FORMAT(11X,36HAFTER DO 10 LOOP - C(2),C(3),C(4) = ,
     1   3G10.4)
C
      IF(LBUG)WRITE(IODBUG,601)(QB(I),I=1,NDT)
  601 FORMAT(11X,14HIN FKA7 - QB =/(10X,14F8.2))
C
      IF(NOTCPU)GO TO 704
      CALL URTIMR(LAPSE,ICPUT)
      ELAPSE=LAPSE/100.
      XICPUT=ICPUT/100.
      WRITE(IODBUG,705)ELAPSE,XICPUT
  705 FORMAT(1H0,10X,'**LEAVING FKA7 - ELAPSED CPU TIME = ',F13.2,
     1  ', TOTAL CPU TIME = ',F13.2)
C
  704 IF(IPWARN.LE.0)RETURN
C
      WRITE(IPR,610)IPWARN
  610 FORMAT(1H0,10X,46H**WARNING**  IN SUBROUTINE FKA7, THE VALUE OF
     1   ,26HS20DT HAS GONE BELOW -0.5 ,I10,07H TIMES./
     2  24X,56HTHIS INDICATES A POSSIBLE ERROR IN THE LAG/K PARAMETERS.)
      CALL WARN
C
      RETURN
      END
