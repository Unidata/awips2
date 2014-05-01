      SUBROUTINE SUMARY55(PO,QTC,LTQTC,STC,LTSTC,TII,NB,X,NGS,HS,NBT,NN,
     . NGAGE,TYPK,YPK,TQPK,QPK,VPK,IFLAG,K1,K2,K3,K4,K7,K8,K9,K23)
C
      CHARACTER NOTE1,NOTE2

      COMMON/FLP55/KFLP
      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
      COMMON/M655/KTIME,DTHYD,J1
      COMMON/M3255/IOBS,KTERM,KPL,JNK,TEH
      COMMON/METR55/METRIC
      COMMON/SS55/NCS,A,B,DB,R,DR,AT,BT,P,DP,ZH
      COMMON/IONUM/IN,IPR,IPU
        COMMON/NETWK55/NET
 
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ofs55'
C
      DIMENSION PO(*),QTC(*),LTQTC(*),STC(*),LTSTC(*),TII(*)
      DIMENSION NB(K1),X(K2,K1),NGS(K4,K1),HS(K9,K2,K1),NBT(K1)
      DIMENSION NGAGE(K1),NN(K23,K1)
      DIMENSION TYPK(K2,K1),YPK(K2,K1),TQPK(K2,K1),QPK(K2,K1),VPK(K2,K1)
      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/sumary55.f,v $
     . $',                                                             '
     .$Id: sumary55.f,v 1.3 2004/02/02 21:50:29 jgofus Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/ 'SUMARY55' /

      CALL FPRBUG(SNAME,1,55,IBUG)
C ..................
C print the appropriate heading for the table
      WRITE(IPR,'(///)')
      IF(KFLP.EQ.0) THEN
C ..................
C composite channel
      WRITE(IPR,1001)
 1001 FORMAT(1X,92('-'))
      IF(IFLAG.EQ.1) WRITE(IPR,500)
  500 FORMAT(//22X,'PROFILE OF CRESTS AND TIMES FOR ENTIRE RUN PERIOD')
      IF(IFLAG.EQ.2) WRITE(IPR,510)
  510 FORMAT(//24X,'PROFILE OF CRESTS AND TIMES FOR FORECAST PERIOD')
      WRITE(IPR,1000)
 1000 FORMAT(10x,'                        *  ORIGINAL CROSS-SECTION'/
     .       10X,'                        #  PEAK STAGE EXCEEDED MAX HS'
     .     //'   RVR  SEC    LOCATION    BOTTOM     TIME MAX',
     .           '    MAX WSEL   TIME MAX     MAX FLOW   MAX VEL')
      IF(METRIC.EQ.0) THEN
        WRITE(IPR,1492)
 1492   FORMAT('   NO.  NO.      MILE       FEET      WSEL(HR)      FEET
     .     FLOW(CFS)       CFS       (FPS)   ')
      ELSE
        WRITE(IPR,1493)
 1493   FORMAT('   NO.  NO.       KM       METERS     WSEL(HR)     METER
     .S    FLOW(CMS)       CMS        (M/S)') 
      ENDIF
      WRITE(IPR,1001)
      ELSE
C ..................
C floodplains
      WRITE(IPR,1100)
 1100 FORMAT(1X,111('-'))
      IF(IFLAG.EQ.1) WRITE(IPR,500)
      IF(IFLAG.EQ.2) WRITE(IPR,510)
      WRITE(IPR,1105)
 1105 FORMAT(39X,'*  ORIGINAL CRESS-SECTION'/
     .       39X,'#  PEAK STAGE EXCEEDED MAX HS'//
     . '   RVR  SEC    LOCATION    BOTTOM     TIME MAX    MAX WSEL   TIM
     .E MAX     MAX FLOW    MAX VL    MAX VC    MAX VR ')
      IF(METRIC.EQ.0) THEN
        WRITE(IPR,1110)
 1110   FORMAT('   NO.  NO.      MILE       FEET      WSEL(HR)      FEET
     .     FLOW(CFS)       CFS       (FPS)     (FPS)     (FPS)   ')
      ELSE
        WRITE(IPR,1115)
 1115   FORMAT('   NO.  NO.       KM       METERS     WSEL(HR)     METER
     .S    FLOW(CMS)       CMS       (M/S)     (M/S)     (M/S)')
      ENDIF
      WRITE(IPR,1100)
      ENDIF


C ..................
C get proper units
      IF(METRIC.EQ.0) THEN
        FT=1.
        CFS=1.
      ELSE
        FT=3.281
        CFS=35.32
      ENDIF
C ..................
C get peak parameters
      DO 1500 J=1,JN+NET
      IF(J.GT.1) WRITE(IPR,1315)
      N=NB(J)
      DO 1450 I=1,N
      NOTE1=' '
      NOTE2=' '
      PTY=TYPK(I,J)
      PY=YPK(I,J)
      PTQ=TQPK(I,J)
      PQ=QPK(I,J)
      BOT=HS(1,I,J)
      PX=X(I,J)
      PVEL=VPK(I,J)
      IF(PY.GT.HS(NCS,I,J)) NOTE2='#'
      DO 1430 IK=1,NBT(J)
        IF (I.EQ.NN(IK,J)) NOTE1='*'
 1430 CONTINUE

C ..................
C compute the peak velocity in the channel & floodplains
      IF(KFLP.EQ.1) THEN
        CALL CONV55(PO(LOQKC),PO(LOHKC),PO(LCBEV),PO(LCNKC),J,I,PY,QK,
     1   DQK,BET,DBE,0,K1,K2)
        SFI=PQ/QK
        CALL SECTF55(NCS,J,I,PY,BL,AL,BR,AR,HS,PO(LOBSL),PO(LOBSR),
     *   PO(LOASL),PO(LOASR),K1,K2,K9)
        CALL FRICTL55(NCS,PO(LOCML),PO(LOYQCM),J,I,PY,CMNL,DCML,
     .   K1,K7,K8)
        CALL FRICTR55(NCS,PO(LOCMR),PO(LOYQCM),J,I,PY,CMNR,DCMR,
     .   K1,K7,K8)
        CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),HS,PO(LOASS),
     .    PO(LOBSS),J,I,PY,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
        QFL=1.49*SFI*AL*(AL/BL)**0.66667/CMNL
        QFR=1.49*SFI*AR*(AR/BR)**0.66667/CMNR
        PVL=0.0
        PVC=0.0
        PVR=0.0
        IF (AL.GT.0.0) PVL=QFL/AL
        IF (AR.GT.0.0) PVR=QFR/AR
        QFC=PQ-QFL-QFR
        AC=A-AL-AR
        PVC=QFC/AC
      ENDIF

C ..................
C convert parameters to metric
      IF(METRIC.EQ.1) THEN
        PY=PY/3.281
        PQ=PQ/35.32
        BOT=BOT/3.281
        PX=PX*1.6093
        PVEL=PVEL/3.281
        PVL=PVL/3.281
        PVR=PVR/3.281
        PVC=PVC/3.281
      ENDIF

C ..................
C print the profile crest table
      IF(KFLP.EQ.0) THEN
        WRITE(IPR,2123) J,I,NOTE1,NOTE2,PX,BOT,PTY,PY,PTQ,PQ,PVEL
      ELSE
        WRITE(IPR,2123) J,I,NOTE1,NOTE2,PX,BOT,PTY,PY,PTQ,PQ,PVL,PVC,PVR
      ENDIF
 1450 CONTINUE
 1500 CONTINUE

C ..................
C print the contents of the compute wsel & flow arrays at
C each plotting station
      WRITE(IPR,1001)
      IF(JNK.LT.9) GO TO 1700
      DO 1660 J=1,JN
      NGAG=NGAGE(J)
      IF(NGAG.EQ.0) GO TO 1660

      KJ=LCAT21(1,J,NGAGE)-1
      DO 1650 K=1,NGAG
      KST=1
      KND=8
 1645 IF(KND.GE.NGAG) KND=NGAG
      WRITE(IPR,1315)
      WRITE(IPR,1693) J,(NGS(KK,J),KK=KST,KND)
 1693 FORMAT(/6H KTIME,2X,10HTII(KTIME),5X,
     & 'COMPUTED STAGES FOR RIVER=',I2,2X,'SECTION=',8I5/)

CC      LSTC=LTSTC(KJ)-1
      DO 1640 KT=1,KTIME
      WRITE(IPR,2124) KT,TII(KT),((STC(KT+LTSTC(KJ+KK)-1)/FT),
     . KK=KST,KND)
 1640 CONTINUE
      WRITE(IPR,1315)
      WRITE(IPR,1694) J,(NGS(KK,J),KK=KST,KND)
 1694 FORMAT(/6H KTIME,2X,10HTII(KTIME),5X,
     & 'COMPUTED DISCHARGE FOR RIVER=',I2,2X,'SECTION=',8I5/)

CC      LQTC=LTQTC(KJ)-1
      DO 1641 KT=1,KTIME
      WRITE(IPR,2125) KT,TII(KT),((QTC(KT+LTQTC(KJ+KK)-1)/CFS),
     . KK=KST,KND)
 1641 CONTINUE
      IF(KND.GE.NGAG) GO TO 1660
      KST=KND+1
      KND=KST+8
      GO TO 1645
 1650 CONTINUE
 1660 CONTINUE
 1700 CONTINUE
 1315 FORMAT(1X)
 2123 FORMAT(I3,I5,2A1,F12.3,F12.2,F12.5,F12.2,F12.5,F12.0,3F10.2)
 2124 FORMAT(I6,F12.3,8F12.2)
 2125 FORMAT(I6,F12.3,8F12.0)
      RETURN
      END
