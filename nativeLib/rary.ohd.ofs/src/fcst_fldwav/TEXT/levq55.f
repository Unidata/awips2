      SUBROUTINE LEVQ55(PO,NB,YU,DDX,HWLV,WCLV,IFLV,TFLV0,TFLV,BLVMX,
     . HFLV,HLVMN,HLV,BLV,HPLV,DPLV,NJFM,NIFM,NJTO,NITO,QLV,QPOND,HPOND,
     . WTLV,K1,K2,K22)
C
C      THIS SUBROUTINE COMPUTES LEVEE RELATED LATERAL FLOWS QLV
C      QL1 --- LEVEE OVERTOPPING FLOW
C      QL2 --- LEVEE BREACHING FLOW
C      QL3 --- FLOW THROUGH PIPE(S)

      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
      COMMON/M3255/IOBS,KTERM,KPL,JNK,TEH
      COMMON/M3455/KXP,ICD,ITMAX,KWARM
      COMMON/LEV55/NLEV,DHLV,NPOND,DTHLV,IDTHLV
      COMMON/METR55/METRIC
      COMMON/IONUM/IN,IPR,IPU
      COMMON/NETWK55/NET


      INCLUDE 'common/fdbug'
      INCLUDE 'common/ofs55'
CC      INCLUDE 'common/opfil55'
C
      DIMENSION PO(*),NB(1),YU(K2,K1),DDX(K2,K1)
      DIMENSION HWLV(K22),WCLV(K22),IFLV(K22),TFLV0(K22),TFLV(K22)
      DIMENSION BLVMX(K22),HFLV(K22),HLVMN(K22),WTLV(K22)
      DIMENSION HLV(K22),BLV(K22),HPLV(K22),DPLV(K22)
      DIMENSION NJFM(K22),NIFM(K22),NJTO(K22),NITO(K22)
      DIMENSION QLV(K2,K1),QPOND(K22),HPOND(K22)
      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/levq55.f,v $
     . $',                                                             '
     .$Id: levq55.f,v 1.3 2004/02/02 21:52:03 jgofus Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/ 'LEVQ55  '  /
C
      CALL FPRBUG(SNAME,1,55,IBUG)

      LVJNK=3
      LVC=0
        TT1=0.0
        SUB2M=1.0
      DO 100 J=1,JN+NET
      N=NB(J)
      DO 100 I=1,N
      QLV(I,J)=0.0
  100 CONTINUE
  110 QPOND(L)=0.0
      IDTHLV=0
      IQLL=0
      DO 2000 L=1,NLEV
      IQL=0
      JFM=NJFM(L)
      IFM1=NIFM(L)
      IFM2=IFM1+1
      JTO=NJTO(L)
      ITO1=NITO(L)
      ITO2=ITO1+1
      WM1=YU(IFM1,JFM)
      WM2=YU(IFM2,JFM)
      WM=0.5*(WM1+WM2)
      IF(ITO1.EQ.0) THEN
        WT=HPOND(JTO)
        WT1=WT
        WT2=WT
      END IF
      IF(ITO1.GT.0) THEN
        WT1=YU(ITO1,JTO)
        WT2=YU(ITO2,JTO)
        WT=0.5*(WT1+WT2)
      END IF
      WCC=WCLV(L)
      DX=DDX(IFM1,JFM)
CCC   DXD=DX
      SUB1=1.00
      SUB2=1.00
      QL1=0.0
      QL2=0.0
      QLP=0.0
cc      HTOP=HMLV(L)+0.05
cc      IF (WM.GE.HTOP .AND. WT.GT.HTOP) THEN
cc      IDTHLV=1
cc      GOTO 2000
cc	ENDIF
c--------------------------
c******* When pond elevation is too close to levee or breach bottom
c        elevation, stop computing levee flow

      HLTOP=HWLV(L)+0.05
      HBTOP=HLTOP
      IF(IFLV(L).EQ.1) HBTOP=HLV(L)+0.05
      HTOP=MIN(HLTOP,HBTOP)
      IF(WM.LT.HTOP.AND.WT.LT.HTOP) GO TO 2000
c-------------------------
      IF(ABS(WM-WT).LE.DHLV) GO TO 1000
C  LEVEE CREVASSE ASSUMED TO OCCUR AT MIDDLE OF REACH
      IF(IFLV(L).EQ.0) THEN
        IF(WM.GE.HFLV(L)) THEN
          TFLV0(L)=TT-DT/TIMF
          IF(TFLV0(L).LE.0.0) TFLV0(L)=0.0
          IFLV(L)=1
        END IF
      END IF
      IF(IFLV(L).EQ.0) GO TO 500
      TR=(TT-TFLV0(L))/TFLV(L)
      IF(TR.GE.1.0) TR=1.0
      BLV(L)=TR*BLVMX(L)
      HLV(L)=HWLV(L)-TR*(HWLV(L)-HLVMN(L))
      HFM=WM-HLV(L)
      HTO=WT-HLV(L)
C  LEVEE CREVASSE FLOW OCCURED
      IDTHLV=1
      IF(WM.LT.WT)GO TO 400
C
C  FLOW FROM MAIN RIVER TO TRIBUTARY
      IF (HFM.LT.0.05) GOTO 500
      SNFM2=-1.0
      SNTO2=1.0
      IF(HTO.GT.0.0) THEN
        IF(HTO.GT.0.67*HFM) SUB2=1.-27.8*(HTO/HFM-0.67)**3
      END IF
      QL2=SUB2*WCC*BLV(L)*HFM**1.5
      IQL=1
      GO TO 500
  400 CONTINUE
C  FLOW FROM TRIBUTARY TO MAIN RIVER
      IF (HTO.LT.0.05) GOTO 500
      SNFM2=1.0
      SNTO2=-1.0
      IF(HFM.GT.0.00) THEN
        IF(HFM.GT.0.67*HTO)SUB2=1.0-27.8*(HFM/HTO-0.67)**3
      END IF
      QL2=SUB2*WCC*BLV(L)*HTO**1.5
      IQL=1
  500 CONTINUE
C  SHORT PIPE OR CULVERT AT MIDDLE OF REACH ASSUMED
      DPA=ABS(DPLV(L))
      IF(DPA.LE.0.0001) GO TO 1000
      BP=DX-DPA
      IF(HLV(L).LE.HPLV(L) .AND. BLV(L).GE.BP) GO TO 1000
      HWP=HPLV(L)-0.5*DPA
      AA=0.7854*DPA*DPA
      IF(WT.GE.WM) GO TO 510
C  DPLV <0.0 --FLOP GATE USED; >0.0 --NO FLOP GATE
      IF(DPLV(L).LT.0.0 .OR. WM.LE.HWP) GO TO 1000
      SNFM3=-1.0
      SNTO3=1.0
      WTT=WT
      IF(WT.LT.HWP) WTT=HWP
      DR=(WM-HWP)/DPA
      IF(DR.GE.1.0) DR=1.0
      CA=6.4*DR*AA
      QLP=CA*SQRT(WM-WTT)
      IQL=1
      GO TO 1000
  510 IF(WT.LE.HWP) GO TO 1000
      SNFM3=1.0
      SNTO3=-1.0
      WMM=WM
      IF(WM.LE.HWP) WMM=HWP
      DR=(WT-HWP)/DPA
      IF(DR.GE.1.0) DR=1.0
      CA=6.4*DR*AA
      QLP=CA*SQRT(WT-WMM)
      IQL=1
 1000 CONTINUE
C OVERTOPPING FLOW COMPUTATION
C BASED ON TWO-SUBREACHES:QL1=QL1U+QL1D  (UP PART + DOWN PART)
      QL1=0.0
      QL1U=0.0
      QL1D=0.0
      HWE=HWLV(L)
      BB=BLV(L)
      WMU=0.5*(WM+WM1)
      WMD=0.5*(WM+WM2)
      WTU=0.5*(WT+WT1)
      WTD=0.5*(WT+WT2)
      LTOP=(DX-BB)/2.0
C COMPUTE OVERTOPPING IN UP-PART (QL1U)
      IF (WMU.LE.HWE .AND. WTU.LE.HWE) GOTO 1800
        IF (ABS(WMU-WTU).LE.DHLV) GOTO 1800
           IQL=1
         IF (WMU.GT.WTU) THEN
         HM1=WMU-HWE
         HT1=WTU-HWE
           SUB1=1.0
         IF (HT1.GT.0.67*HM1) SUB1=1.0-27.8*(HT1/HM1-0.67)**3
         QL1U=-1.0*SUB1*WCC*LTOP*HM1**1.5
         ELSE
           HM1=WTU-HWE
           HT1=WMU-HWE
           SUB1=1.0
           IF (HT1.GT.0.67*HM1) SUB1=1.0-27.8*(HT1/HM1-0.67)**3
           QL1U=SUB1*WCC*LTOP*HM1**1.5
         ENDIF
C COMPUTE OVERTOPPING IN DOWN-PART (QL1D)
1800  IF (WMD.LE.HWE .AND. WTD.LE.HWE) GOTO 1820
      IF (ABS(WMD-WTD).LE.DHLV) GOTO 1820
         IQL=1
           IF (WMD.GT.WTD) THEN
         HM1=WMD-HWE
         HT1=WTD-HWE
           SUB1=1.0
           IF (HT1.GT.0.67*HM1) SUB1=1.0-27.8*(HT1/HM1-0.67)**3
         QL1D=-1.0*SUB1*WCC*LTOP*HM1**1.5
         ELSE
           HM1=WTD-HWE
           HT1=WMD-HWE
           SUB1=1.0
           IF (HT1.GT.0.67*HM1) SUB1=1.0-27.8*(HT1/HM1-0.67)**3
         QL1D=SUB1*WCC*LTOP*HM1**1.5
       ENDIF
1820  QL1=QL1U+QL1D
        SNFM=1.0
        SNTO=-1.0
        IF (IQL.EQ.1) IDTHLV=1
        IF (QL1.LT.0.0) THEN
        QL1=-1.0*QL1
        SNFM=-1.0
        SNTO=1.0
        ENDIF
      IF(IQL.EQ.0) GO TO 2000
      IQLL=1
      QLV(IFM1,JFM)=QLV(IFM1,JFM)+QL1*SNFM+QL2*SNFM2+QLP*SNFM3
      IF(ITO1.GT.0) QLV(ITO1,JTO)=QLV(ITO1,JTO)+
     $ QL1*SNTO+QL2*SNTO2+QLP*SNTO3
      IF(ITO1.EQ.0) QPOND(JTO)=QPOND(JTO)+
     $ QL1*SNTO+QL2*SNTO2+QLP*SNTO3
      PQL1=QL1*SNFM
      PQL2=QL2*SNFM2
      PQLP=QLP*SNFM3
      PBB=BB
        IF (METRIC.EQ.1) THEN
        PQL1=PQL1/35.32
        PQL2=PQL2/35.32
        PQLP=PQLP/35.32
        PBB=PBB/3.281
        WM=WM/3.281
        WT=WT/3.281
        ENDIF
      LVC=LVC+1
      IF(JNK.GE.LVJNK.AND.LVC.EQ.1) WRITE(IPR,3004)
 3004 FORMAT(/10X,'TT   LV   JM   IM   JT   IT',6X,'QLOVTP',6X,'QLPOND',
     .6X,'QLBRCH',3X,'BR-WDTH',4X,'WSEL-M',4X,'WSEL-T','  SUB-M  SUB-T')
      IF(JNK.GE.LVJNK) WRITE(IPR,3005) TT,L,JFM,IFM1,JTO,ITO1,PQL1,PQLP,
     . PQL2,PBB,WM,WT,SUB1,SUB2
 3005 FORMAT(F12.3,5I5,3F12.3,3F10.3,2F7.2)
      WTLV(L)=WT
C FOR INCREASE OR DECREASE TIME STEP AFTER BREACH FAILURE
        IF (SUB2.LT.SUB2M) SUB2M=SUB2
        TT1A=0.0
      IF (IFLV(L).EQ.1) TT1A=TFLV0(L)+5*TFLV(L)
      IF (TT1A.GT.TT1) TT1=TT1A
C FOR DEBUG USE BY DEVELOPER, PRINT INFO FOR LEVEE IPTEMP
      IPTEMP=210000
        IF (L.NE.IPTEMP) GOTO 2000
      DTUSED=TT-TTEMP
        DHPOOL=WM-WT
      WRITE(IPR,'(F12.3,3F14.1,4F14.3,I4)')
     $ TT,PQL1+PQL2+PQLP,PQL1,PQL2,WM,DHPOOL,DTUSED,SUB2,IDTHLV
      TTEMP=TT
 2000 CONTINUE

      IF(NPOND.EQ.0.OR.IQLL.EQ.0) GOTO 3500
      CALL POND55(PO(LOSAP),PO(LOHSAP),QPOND,HPOND)
      IF(JNK.LT.5) GOTO 3500
        IF(METRIC.EQ.1) THEN
          DO 1622 L=1,NPOND
            QPOND(L)=QPOND(L)/35.32
            HPOND(L)=HPOND(L)/3.281
 1622     CONTINUE
        ENDIF
        WRITE(IPR,*)
        WRITE(IPR,2111) (QPOND(L),L=1,NPOND)
        WRITE(IPR,2112) (HPOND(L),L=1,NPOND)
        IF(METRIC.EQ.1) THEN
          DO 1624 L=1,NPOND
            QPOND(L)=QPOND(L)*35.32
            HPOND(L)=HPOND(L)*3.281
 1624     CONTINUE
        END IF
 2111 FORMAT(2X,'QPOND(L)=',8F10.0,10(/11X,8F10.0))
 2112 FORMAT(2X,'HPOND(L)=',8F10.2,10(/11X,8F10.2))
 3500 IF (IDTHLV.EQ.0) GOTO 999
      IF (TT1.LE.0.01) GOTO 999
      IF (TT.LE.TT1) GOTO 999
        IF (SUB2M.LT.0.3) GOTO 999
      IDTHLV=0
  999 RETURN
      END
