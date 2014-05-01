C MEMBER XQT51
C DESC TIME-INTERVAL-LOOP EXECUTION CONTROLLER FOR 'SSARRSV'
C----------------------------------------------------------------------
C
C@PROCESS LVL(77)
C
      SUBROUTINE XQT51(PO,CO,D,WK,NPA,IDPT,LOCWS)
C-------------------------------------------------------------------
C           LOOP THROUGH THE ENTIRE RUN PERIOD ONE PERIOD AT A TIME,
C           ESTABLISHING STATE VARIABLE VALUE AT PERIOD START,
C           STORING COMPUTED RESULTS AT PERIOD END,
C           UPDATING AND SAVING CARRYOVER.
C
C  ISTYP, =0, NON-BACK; =1, UPPER-BACK; =2, TRIBU-BACK; =3, 3 VAR
C----------------------------------------------------------------------
C  WRITTEN BY - KUANG HSU - HRL - OCTOBER 1994
C  BUG R22-9 FIXED BY - XiaoBiao FAN - 12/03/2002
C----------------------------------------------------------------------
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fcary'
      INCLUDE 'common/sarr51'
      INCLUDE 'common/xco51'
      INCLUDE 'common/lts51'
      INCLUDE 'common/unit51'
      INCLUDE 'common/mod151'
C
      EQUIVALENCE (IS1,S1),(IS2,S2)
      EQUIVALENCE (IFA(1),FA(1)),(IFB(1),FB(1))
C
      DIMENSION PO(*),CO(*),D(*),NPA(*),WK(*),IDPT(*),LOCWS(*)
      DIMENSION IFA(7),FA(7),IFB(7),FB(7),LOBS(2)
      REAL*8 RESTYP(4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ssarresv/RCS/xqt51.f,v $
     . $',                                                             '
     .$Id: xqt51.f,v 1.7 2003/03/14 16:49:36 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA RESTYP/8H'NOT A' ,8H'UPPER' ,8H'TRIBU' ,8H'3-VAR' /
C
C-------------------------------------------
C  DEBUG OUTPUT IF REQUESTED
C
      IF (IBUG.GE.1) WRITE(IODBUG,1650)
 1650 FORMAT('   *** ENTER XQT51 ***')
C
C  SET INITIAL VALUES
C
      NUMSIM = 0
      LOBSTO = 0
      IFCST = 0
      NQTPWRN = 1
      JDAORG = JDAWEK
      NDAYRN = NODAYR
      JULDAY = JULDAT
      JDAWEK = JDAORG
      NUMSIM = NUMSIM + 1
      IENTER = -999
      LTW = LOCWS(NRES+1)
      LCW = LOCWS(NRES+2)
      LASTCO = 0
      XH2 = MINODT/2
      CF = DCF*(XH2/12.)
      NP = NRES
C
C  IF ONLY ONE REGULATION OPTION IS USED IN A TWO RESERVOIR SYSTEM
C  OR IN A UPSTREAM SATTION BACKWATER AFFECTED BY A DOWNSTREAM RESERVOIR
C  CHANGE REGULATION OPTION ARRAY RRC TEMPORARILY
      IF(NRES.LE.1) GO TO 230
      NRRC = RRC(1,NRES)
      IF(NRRC.GT.7) GO TO 230
      NRRC = RRC(1,NRES-1)
      IF(NRRC.LE.7) GO TO 230
      DO 240 I=1,NRRC
      RRC(I,NRES) = RRC(I,NRES-1)
 240  RRC(I,NRES-1) = 0.0
      RRC(1,NRES-1) = 7.0
      RRC(7,NRES-1) = 100.0
 230  CONTINUE
C
C  COPY INITIAL CARRYOVER INTO WORK SPACE
C
      CALL UMEMOV(CO,WK(LCW),NUMCOV)
C
C  DETERMINE IF OBSERVED DATA ARE AVAILABLE AND
C  FILL THE TRACES WITH OBSERVED OR COMPUTED FROM OBSERVED VALUES.
C
      DO 300 II=1,NRES
 300  LOBS(II) = 0
      IF(NRUN.GT.0) CALL XOBS51(PO,CO,WK,D,LOCWS,IDPT,LOBS)
      CALL MDYH1(IDA,IHR,IMON,IDAY,IYEAR,IHRM,NLSTZ,NOUTDS,TZCS)
C
C  NOW LOOP THROUGH ALL PERIODS IN RUN
C
      DO 1000 NS2=1,NUM
C
      IF (IBUG.GE.2) THEN
        WRITE(IODBUG,1610) NS2
        WRITE(IODBUG,1613) NS2,(WK(LCW+L-1),L=1,NUMCOV)
      ENDIF
 1610 FORMAT(/2X,24(1H*)/2X,1H*,22X,1H*/2X,1H*,' TIME INTERVAL NO. ',I2,
     .2H */2X,1H*,22X,1H*/2X,24(1H*)/)
 1613 FORMAT(/'   ** CARRYOVER AT START OF INTERVAL ',I3,1H:,
     & /1X,F10.1,2(F10.1,F10.2,F10.0,F12.2))
C
C
C  LOAD FA AND FB ARRAY
      IDUM = (IDA-1)*24+IHR-MINODT+(NS2-1)*MINODT-IFIX(RRC(3,NRES))
      IF(RRC(1,NRES).LE.7.0) IDUM=(NS2-NRUN-1)*MINODT
      IFA(1) = IDUM*10
      IFB(1) = IFA(1)+MINODT*10
C  REST OF FB ARRAY NEEDED ONLY IN THREE VARIABLE RELATION WITHOUT
C  BACKWATER ROUTING
      DO 400 IP=1,NP
      LPA = LOCWS(IP)-1
      IP2 = IP*2
C
C  SET INITIAL VALUES FROM CARRYOVER
C
      QO1 = WK(LPA+71)
      ELEV1 = WK(LPA+69)
      FA(IP2) = QO1
      FA(IP2+1) = ELEV1
 400  CONTINUE
C
      ELDS = 0.0
      IF(NRES.EQ.2) ELDS = WK(LCW+2)
      DO 900 II = 1,NRES
      IRES = II
      IP = II
      LPA = LOCWS(IP)-1
      ISTYP = NPA(LPA+14)
      CALL TSPT51(PO,CO,D,LOCWS,IDPT)
C
C  LOAD INFLOWS QI1, QI2 AND QIM
C
      QI0 = QI1
      IF(IRES.GE.2) GO TO 520
C
      IF(LDQI1.GT.0) GO TO 500
      LDQI1=LDQI2-1
      IF(NS2.EQ.1) THEN
        QI0=CO(1)
        QI1=QI0
        FA(1+2*NRES+IP)=QI0
        GO TO 510
      END IF
C
 500  QI1 = D(LDQI1+NS2-1)
 510  QI2 = D(LDQI2+NS2-1)
      QIM = 0.5*(QI1+QI2)
 520  CONTINUE
C
C  SET INITIAL VALUES FROM CARRYOVER
C
C     !cxf 12/03/2002   for BUG r22-9     cxf 12/03/2002!
C
C  There's some strange behavoir when running a particular segment 
C  in ESP which has a SSARRESV operation involving an upstream and 
C  downstream reservoir.  The segment is QBYQ2 and the problem occurs 
C  on New Year's day when the reservoir elevation takes a dive and 
C  the outflow goes to minimum. This all happens around a period were
C  the reservoir outfloe is constant.
C
C  This is a problem with carryover. When the SSARRESV operation starts 
C  at the beginning of the month, it should read the carryover written 
C  at the end of the previous month (last tiem step) instead of the 
C  initial carryover. 
C
C     !cxf 12/03/2002   for BUG r22-9     cxf 12/03/2002!
C
cxf     IF(NS2.EQ.1) THEN
        LCW1 = LCW + (NRES-IRES)*4 + 1
        QO1  = WK(LCW1)
        IF(ISTYP.EQ.3) QO1=QI1
        ELEV1 = WK(LCW1+1)
        IS1    = IFIX(WK(LCW1+2))
cxf     END IF
C
C  SET END OF PERIOD VALUES FROM THE TRACE HOLDING AREA
C  VALUES HAVE BEEN INITIALIZED TO MISSING (-999.0) 
C  IF OBSERVED DATA EXISTS, VALUES ARE REPLACED WITH OBSERVED DATA
C  FOR THE PERIOD BEFORE THE LAST OBSERVED DATA (FROM ROUTINE XU1751)
C
      QO1N  = WK(LWQO1+NS2-1)
      QO2   = WK(LWQO2+NS2-1)
      QOM   = WK(LWQM+NS2-1)
      ELEV2 = WK(LWEL+NS2-1)
      IS2   = IFIX(WK(LWST+NS2-1))
C
      IF(ISTYP.NE.3) GO TO 700
      IP2 = IP*2
C
C  CONVERT TO INPUT UNITS FOR TABLE LOOKUP
      QO2 = D(LDQI2+NS2-1)
      ELEV2 = D(LDQI2+NS2-1)
      FB(IP2) = QO2
      FB(IP2+1) = ELEV2
      IF (IBUG.LT.2) GO TO 640
      IRTYP = ISTYP+1
      WRITE(IODBUG,1620) RESTYP(IRTYP)
      WRITE(IODBUG,1630) 
     & QI1,QI2,QIM,QO1,QO1N,QO2,QOM,ELEV1,ELEV2,IS1,IS2
 640  CONTINUE
      IS51 = NPA(LPA+51)
      CALL TRPR51(IS51,QI0,QI1,QI2,WK,NPA,NPA(LPA+1),FA,FB)
      I55=NPA(LPA+55)
      IF(I55.EQ.1) QO2 =  FB(2*IP)
      IF(I55.EQ.2) ELEV2 =  FB(2*IP+1)
      DQ = QO2-QI2
      QO1N = QI1+DQ
      IF(ISTYP.LE.0 .AND. QO1N.LE.0.) QO1N=0.0
      FA(1+2*NP+IP) = QO1N
      IS2 = 0
      WK(LPA+71) = QO2
      WK(LPA+69) = ELEV2
      NPA(LPA+38) = IS2
      GO TO 880
 700  CONTINUE
      IF (IBUG.LT.2) GO TO 840
      IRTYP = ISTYP+1
      WRITE(IODBUG,1620) RESTYP(IRTYP)
 1620 FORMAT(/5X,A8,'BACKWATER AFFECTED RESERVOIR:')
      WRITE(IODBUG,1630) 
     & QI1,QI2,QIM,QO1,QO1N,QO2,QOM,ELEV1,ELEV2,IS1,IS2
 1630 FORMAT(5X,'BEGINNING OF PERIOD VALUES -',
     & /5X,'       QI1       QI2       QIM       QO1      QO1N',
     & '       QO2       QOM     ELEV1     ELEV2        S1        S2',
     & /5X,7F10.1,2F10.2,2I12)
 840  CONTINUE
C
C  ACTUAL COMPUTATION BEGINS RIGHT AFTER THE LAST OBSERVED DATA
C
      NPA(LPA+52) = 2*IP
      IF(NS2.LE.LOBS(IRES)) THEN
        WK(LPA+71) = QO2
        WK(LPA+69) = ELEV2
        NPA(LPA+38) = IS2
        GO TO 881
      END IF
      IS50 = NPA(LPA+50)
cc      IF(IS50.EQ.3) THEN
      IF(ISTYP.EQ.2) THEN
        FB(2*IS50) = D(LDQL2+NS2-1)
        FB(2*IS50+1) = D(LDQL2+NS2-1)
      END IF
      IS51 = NPA(LPA+51)
      CALL LKRT51(IS51,XH2,QI1,QI2,WK,NPA,NPA(LPA+1),FA,FB,CF,ELDS,
     & NQTPWRN)
C
      IS52 = NPA(LPA+52)
      QO1N = FA(IS52)
      IF(ISTYP.LE.0 .AND. QO1N.LE.0.) QO1N=0.0
      QO2 = WK(LPA+71)
      IF(ISTYP.LE.0 .AND. QO2.LE.0.) QO2=0.0
      ELEV2 = WK(LPA+69)
      IS2 = NPA(LPA+38)
 880  CONTINUE
      QOM = 0.5*(QO1N+QO2)
 881  WK(LWQO1+NS2-1) = QO1N
      WK(LWQO2+NS2-1) = QO2
      WK(LWQM+NS2-1) = QOM
      WK(LWEL+NS2-1) = ELEV2
      WK(LWST+NS2-1) = FLOAT(IS2)
C
C  UPDATE CARRYOVER
      IF(IRES.EQ.1) WK(LCW) = QI2
      WK(LCW1) = QO2
      WK(LCW1+1) = ELEV2
      WK(LCW1+2) = FLOAT(IS2)
      IF (IBUG.GE.2) WRITE(IODBUG,1690) 
     & QO1N,QO2,QOM,ELEV2,IS2                
 1690 FORMAT(/10X,'END OF PERIOD OUTPUTS -',
     & '      QO1N       QO2       QOM     ELEV2          S2',
     & /33X,3F10.1,F10.2,I12)
C
      IF(NRES.LE.1) GO TO 900
      QL1=0.0
      QL2=0.0
      IF(LDQL2.LE.0) GO TO 896
      QL2=D(LDQL2+NS2-1)
      IF(LDQL1.GT.0) GO TO 895
C
C  SET INITIAL VALUES FROM CARRYOVER
C
      LDQL1=LDQL2-1
      IF(NS2.EQ.1) THEN
        LCW1 = LCW + (NRES-IRES)*4 + 1
        QL1  = WK(LCW1+3)
        GO TO 896
      END IF
 895  QL1 = D(LDQL1+NS2-1)
 896  QI1 = QO1N+QL1
      QI2 = QO2+QL2
      QIM = 0.5*(QI1+QI2)
      WK(LCW1+3) = QL2
 900  CONTINUE
C
C  SAVE CARRYOVER IF NEEDED
C
      IF (ISAVCO.EQ.0) GO TO 910
      IF (LASTCO.EQ.NCSTOR) GO TO 910
      IPOS = LASTCO + 1
      IF (ICOPOS(IPOS).NE.NS2) GO TO 910
C
C  SAVE CARRYOVER, WE'VE PASSED ALL THE TESTS
C
C  ALTERNATIVE WAY TO GET DATES WITHOUT /FCARY/ IS:
C    NPD = NS2 - 1
C    MHR = (MOD(NPD,NTIM24)+1)*MINODT
C    CALL FCWTCO(JULDAY,MHR,WK(LOCW),NUMCOV)
C
      IF (IBUG.GE.2) WRITE(IODBUG,1691) NS2,(WK(LCW+L-1),L=1,NUMCOV)
 1691 FORMAT('  >>> SAVING CARRYOVER FOR PERIOD NO. ',I3 /
     &,(6F12.3))
C
      CALL FCWTCO(ICDAY(IPOS),ICHOUR(IPOS),WK(LCW),NUMCOV)
      LASTCO = LASTCO + 1
C
 910  CONTINUE
C
C  UPDATE VARIABLES FOR NEXT PERIOD
C
      QO1 = QO2
      ELEV1 = ELEV2
      S1 = S2
C
 1000 CONTINUE
C
C  OUTPUT SIMULATED VALUES TO TIME-SERIES IF REQUESTED.
C
      CALL XSMO51(PO,WK,D,IDPT,LOCWS)
C
C----------------------------------------------------------
C  LAST STEP IS TO UPDATE CO ARRAY FOR USE OF SSARRESV IN ESP AND MCP
C
      CALL UMEMOV(WK(LCW),CO,NUMCOV)
C
C-----------------------------------------------------------------
C  WE'RE DONE WITH THE EXECUTION OF THE OPERATION.
C
 9000 CONTINUE
C
C  IF ONLY ONE REGULATION OPTION IS USED IN A TWO RESERVOIR SYSTEM
C  OR IN A UPSTREAM SATTION BACKWATER AFFECTED BY A DOWNSTREAM RESERVOIR
C  RESTORE REGULATION OPTION ARRAY RRC.
      IF(NRES.LE.1) GO TO 1730
      NRRC = RRC(1,NRES-1)
      IF(NRRC.GT.7) GO TO 1730
      NRRC = RRC(1,NRES)
      DO 1740 I=1,NRRC
      RRC(I,NRES-1) = RRC(I,NRES)
1740  RRC(I,NRES) = 0.0
C
1730  IF (IBUG.GE.1) WRITE(IODBUG,1699)
 1699 FORMAT('    *** EXIT XQT51 ***')
      RETURN
      END
