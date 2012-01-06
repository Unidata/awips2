C MEMBER PUC54
C
      SUBROUTINE PUC54(PO,CO)
C.......................................
C     THIS SUBROUTINE PUNCHES PARAMETERS AND CARRYOVER VALUES FOR
C       THE SIMPLE WATER BALANCE (SWB-NILE) OPERATION IN THE
C       FORMAT REQUIRED BY THE INPUT SUBROUTINE PIN54.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C       QINGYUN DUAN - GCIP CLIMATE PROJECT  SEPTEMBER 1995 VERSION 1
C.......................................
C
      DIMENSION PO(1),CO(1)
      DIMENSION TSID1(2),TSID2(2),TSID3(2)
      DIMENSION IM(3),IY(3),LM(3),LY(3)
C     COMMON BLOCKS
C                                           *--> FROM COMMON.FDBUG
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C                                           *--> FROM COMMON.IONUM
      COMMON/IONUM/IN,IPR,IPU
C
C                                           *--> FROM COMMON.FPROG
      COMMON/FPROG/MAINUM,VERS,VDATE(2),PNAME(5),NDD
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc54.f,v $
     . $',                                                             '
     .$Id: puc54.f,v 1.1 1997/09/22 17:36:10 page Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA FRZE/4HFRZE/,BLANK/4H    /
      DATA PROT/4HPROT/
C
C***********************************************************
C     TRACE LEVEL=1, NO DEBUG OUTPUT
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,'** PUC54 ENTERED')
C
C***********************************************************
C     CONTROL VARIABLES
      IVER=PO(1)
      ITP=PO(7)
      LPM=PO(14)
      NPM=PO(15)
      LET=PO(16)
      NCO=PO(17)
      NXCO=PO(18)
      LRS=PO(19)
      LRG=PO(20)
      LSM=PO(21)
      LFRZE=PO(22)
      LTA=PO(23)
      LPP=PO(24)
      LWE=PO(25)
      LFE=PO(26)
      LSN=PO(27)
      LPROT=PO(28)
      LSUMS=PO(31)
C
C***********************************************************
C     PUNCH CARD NUMBER 1
C***********************************************************
C
      WRITE(IPU,901) (PO(I),I=2,6),ITP,(PO(I),I=8,13)
  901 FORMAT(5A4,3X,I2,7X,2A4,1X,A4,7X,2A4,1X,A4)
C
C***********************************************************
C     PUNCH CARD NUMBER 2
C***********************************************************
C
      RSOP=BLANK
      RGOP=BLANK
      SMOP=BLANK
      FGOP=BLANK
      TAOP=BLANK
      PPOP=BLANK
      WEOP=BLANK
      FEOP=BLANK
      SNOP=BLANK
      PROP=BLANK
      ISUM=0
      IF(LRS.GT.0) RSOP=PO(LRS+2)
      IF(LRG.GT.0) RGOP=PO(LRG+2)
      IF(LSM.GT.0) SMOP=PO(LSM+2)
      IF(LFRZE.GT.0) FGOP=FRZE
      IF(LTA.GT.0) TAOP=PO(LTA+2)
      IF(LPP.GT.0) PPOP=PO(LPP+2)
      IF(LWE.GT.0) WEOP=PO(LWE+2)
      IF(LFE.GT.0) FEOP=PO(LFE+2)
      IF(LSN.GT.0) SNOP=PO(LSN+2)
      IF(LPROT.GT.0) PROP=PROT
      IF(LSUMS.GT.0) ISUM=1
      WRITE(IPU,902) RSOP,RGOP,SMOP,FGOP,TAOP,PPOP,WEOP,FEOP,SNOP,PROP,
     +               ISUM
  902 FORMAT(10(1X,A4),1X,I1)
C
C***********************************************************
C     PUNCH CARD NUMBER 3
C***********************************************************
C
      TSID1(1)=PO(LET)
      TSID1(2)=PO(LET+1)
      PETYPE=PO(LET+2)
      IPE1=LET+3
      IPE2=LET+14
      WRITE(IPU,903)TSID1,PETYPE,(PO(I),I=IPE1,IPE2)
  903 FORMAT(2X,2A4,1X,A4,5X,12F5.2)
C
C***********************************************************
C     PUNCH CARD NUMBER 4
C***********************************************************
C
      IOPTET=PO(LPM+7)
      WRITE(IPU,904) (PO(LPM+I-1),I=1,7),IOPTET
  904 FORMAT(2F5.2,F5.0,F5.2,2F5.3,F5.2,1X,I1)
C
C***********************************************************
C     PUNCH CARD NUMBER 5
C***********************************************************
C
      IF(LFRZE.EQ.0) GO TO 190
      IF((LRS.EQ.0).AND.(LRG.EQ.0).AND.(LSM.EQ.0)) GO TO 120
      CALL TSV54(LRS,PO,TSID1,DT1,1)
      CALL TSV54(LRG,PO,TSID2,DT2,1)
      CALL TSV54(LSM,PO,TSID3,DT3,0)
      WRITE(IPU,905)TSID1,TSID2,TSID3,DT3
  905 FORMAT(3(2X,2A4),3X,A2)
C
C***********************************************************
C     PUNCH CARD NUMBER 6
C***********************************************************
C
  120 IF((LTA.EQ.0).AND.(LPP.EQ.0).AND.(LWE.EQ.0)) GO TO 140
      CALL TSV54(LTA,PO,TSID1,DT1,0)
      CALL TSV54(LPP,PO,TSID2,DT2,0)
      CALL TSV54(LWE,PO,TSID3,DT3,0)
      WRITE(IPU,906)TSID1,DT1,TSID2,DT2,TSID3,DT3
  906 FORMAT(3(2X,2A4,3X,A2))
C
C***********************************************************
C     PUNCH CARD NUMBER 7
C***********************************************************
C
  140 IF((LFE.EQ.0).AND.(LSN.EQ.0)) GO TO 150
      CALL TSV54(LFE,PO,TSID1,DT1,0)
      CALL TSV54(LSN,PO,TSID2,DT2,0)
      WRITE(IPU,908)TSID1,DT1,TSID2,DT2
  908 FORMAT(2(2X,2A4,3X,A2))
C
C***********************************************************
C     PUNCH CARD NUMBER 8
C***********************************************************
C
  150 IF(LFRZE.EQ.0) GO TO 190
      WRITE(IPU,909)(PO(LFRZE+I-1),I=1,5)
  909 FORMAT(F5.1,F5.2,2F5.3,F5.1)
C
C***********************************************************
C     PUNCH CARD NUMBER 9
C***********************************************************
C
  190 WRITE(IPU,910) (CO(I),I=1,NCO)
  910 FORMAT(2F5.1)
C
C***********************************************************
C     PUNCH CARD NUMBER 10
C***********************************************************
C
  200 IF(LFRZE.EQ.0) GO TO 205
      WRITE(IPU,911)(CO(I),I=NCO+1,NCO+NXCO)
  911 FORMAT(5F5.1,F5.2,2F5.1)
C
C***********************************************************
C     PUNCH CARD NUMBER 11
C***********************************************************
C
  205 IF(LPROT.EQ.0) GO TO 990
      IF(MAINUM.LT.3) GO TO 990
      NP=0
      DO 208 I=1,3
      L=LPROT+(I-1)*2+1
      M=PO(L)
      IF(M.LE.0) GO TO 210
      IY(I)=(M-1)/12
      IM(I)=M-IY(I)*12
      M=PO(L+1)
      IF(M.LE.0) GO TO 210
      LY(I)=(M-1)/12
      LM(I)=M-LY(I)*12
      NP=NP+1
  208 CONTINUE
  210 IF(NP.EQ.0) GO TO 990
      WRITE(IPU,912) (IM(I),IY(I),LM(I),LY(I),I=1,NP)
  912 FORMAT(6(3X,I2,1X,I4))
C
C***********************************************************
C     END OF PUNCHED CARD SUBROUTINE
C***********************************************************
C
  990 IF(ITRACE.LT.1) GO TO 999
      WRITE(IODBUG,913)
  913 FORMAT(1H0,'**EXIT PUC54')
  999 RETURN
      END
