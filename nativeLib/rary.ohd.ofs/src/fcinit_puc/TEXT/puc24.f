C MEMBER PUC24
C  (from old member FCPUC24)
C
      SUBROUTINE PUC24(PO,CO)
C                             LAST UPDATE: 02/22/94.13:47:36 BY $WC30EA
C
C***********************************************************
C     THIS IS THE PUNCH SUBROUTINE FOR THE API-CONT OPERATION
C***********************************************************
C     INITIALLY WRITTEN BY ERIC ANDERSON, HRL - SEPT 1992
C***********************************************************
      DIMENSION PO(1),CO(1)
      DIMENSION VOP(3),TSID1(2),TSID2(2),TSID3(2),TSID4(2),TSID5(2)
      DIMENSION IM(3),IY(3),LM(3),LY(3)
C     COMMON BLOCKS
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fprog'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc24.f,v $
     . $',                                                             '
     .$Id: puc24.f,v 1.2 1996/03/21 15:35:59 page Exp $
     . $' /
C    ===================================================================
C
C     DATA STATEMENTS
      DATA VOP/3HWKN,3HAEI,3HATI/,FRZE/4HFRZE/,BLANK/4H    /
      DATA PROT/4HPROT/
C***********************************************************
C     TRACE LEVEL=1, NO DEBUG OUTPUT
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,'** PUC24 ENTERED')
C***********************************************************
C     CONTROL VARIABLES
      IVER=PO(1)
      ITP=PO(7)
      IVOPT=PO(14)
      LPE=PO(15)
      LSC=PO(16)
      LWE=PO(17)
      LTA=PO(18)
      LRS=PO(19)
      LRG=PO(20)
      LAI=PO(21)
      LAPI=PO(22)
      LPROT=PO(23)
      LFRZE=PO(24)
      LRSPM=PO(26)
      LRGPM=PO(27)
      LAPIC=PO(29)
      LAETI=PO(30)
      LFRS=PO(31)
      LSUMS=PO(32)
      NRSPM=17
      IF (IVER.EQ.1) NRSPM=16
      LFI=0
      LFEI=0
      IF(LFRZE.EQ.0) GO TO 100
      IF(PO(LFRZE).NE.BLANK) LFI=LFRZE
      IF(PO(LFRZE+4).NE.BLANK) LFEI=LFRZE+4
C***********************************************************
C     PUNCH CARD NUMBER 1
  100 IF (LRG.GT.0) GO TO 106
  105 WRITE(IPU,901) (PO(I),I=2,6),ITP,(PO(I),I=8,13)
  901 FORMAT(5A4,3X,I2,7X,2A4,1X,A4,7X,2A4,1X,A4,F7.0)
      GO TO 110
  106 IF (PO(LRG+3).EQ.0.0) GO TO 105
      WRITE(IPU,901) (PO(I),I=2,6),ITP,(PO(I),I=8,13),PO(LRG+3)
C***********************************************************
C     PUNCH CARD NUMBER 2
  110 FGOP=BLANK
      SCOP=BLANK
      WEOP=BLANK
      RSOP=BLANK
      RGOP=BLANK
      AIOP=BLANK
      APOP=BLANK
      FIOP=BLANK
      ACOP=BLANK
      ETOP=BLANK
      FSOP=BLANK
      FEOP=BLANK
      PROP=BLANK
      ISUM=0
      IF(LFRZE.GT.0) FGOP=FRZE
      IF(LSC.GT.0) SCOP=PO(LSC+2)
      IF(LWE.GT.0) WEOP=PO(LWE+2)
      IF(LRS.GT.0) RSOP=PO(LRS+2)
      IF(LRG.GT.0) RGOP=PO(LRG+2)
      IF(LAI.GT.0) AIOP=PO(LAI+2)
      IF(LAPI.GT.0) APOP=PO(LAPI+2)
      IF(LFI.GT.0) FIOP=PO(LFI+2)
      IF(LAPIC.GT.0) ACOP=PO(LAPIC+2)
      IF(LAETI.GT.0) ETOP=PO(LAETI+2)
      IF(LFRS.GT.0) FSOP=PO(LFRS+2)
      IF(LFEI.GT.0) FEOP=PO(LFEI+2)
      IF(LPROT.GT.0) PROP=PROT
      IF(LSUMS.GT.0) ISUM=1
      WRITE(IPU,902) VOP(IVOPT+1),FGOP,SCOP,WEOP,RSOP,RGOP,AIOP,APOP,
     1FIOP,ACOP,ETOP,FSOP,FEOP,PROP,ISUM
  902 FORMAT(2X,A3,13(1X,A4),1X,I1)
C***********************************************************
C     PUNCH CARD NUMBER 3
      IF((LPE.EQ.0).AND.(LTA.EQ.0)) GO TO 130
      IF(LPE.EQ.0) GO TO 121
      TSID1(1)=PO(LPE)
      TSID1(2)=PO(LPE+1)
      PETYPE=PO(LPE+2)
      PEADJ=PO(LPE+3)
  121 IF(LTA.EQ.0) GO TO 122
      TSID2(1)=PO(LTA)
      TSID2(2)=PO(LTA+1)
      TATYPE=PO(LTA+2)
      ITTA=PO(LTA+3)
      EDIFF=PO(LTA+4)
      IF(EDIFF.EQ.0.0) GO TO 122
      TALX=PO(LTA+5)
      TALN=PO(LTA+6)
  122 IF(LPE.EQ.0) GO TO 125
      IF(LTA.GT.0) GO TO 123
C     EVAPORATION ONLY
      WRITE(IPU,903)TSID1,PETYPE,PEADJ
  903 FORMAT(2X,2A4,1X,A4,F5.2,2X,2A4,1X,A4,3X,I2,5X,F5.0,2F5.1)
      GO TO 130
C     EVAPORATION AND TEMPERATURE
  123 IF(EDIFF.NE.0.0) GO TO 124
      WRITE(IPU,903)TSID1,PETYPE,PEADJ,TSID2,TATYPE,ITTA,EDIFF
      GO TO 130
  124 WRITE(IPU,903)TSID1,PETYPE,PEADJ,TSID2,TATYPE,ITTA,EDIFF,TALX,
     1TALN
      GO TO 130
C     TEMPERATURE ONLY
  125 IF(EDIFF.NE.0.0) GO TO 126
      WRITE(IPU,9031)TSID2,TATYPE,ITTA,EDIFF
 9031 FORMAT(22X,2A4,1X,A4,3X,I2,5X,F5.0,2F5.1)
      GO TO 130
  126 WRITE(IPU,9031)TSID2,TATYPE,ITTA,EDIFF,TALX,TALN
C***********************************************************
C     PUNCH CARD NUMBER 4
  130 IF((LSC.EQ.0).AND.(LWE.EQ.0)) GO TO 140
      CALL TSV24(LSC,PO,TSID1,DT1,0)
      CALL TSV24(LWE,PO,TSID2,DT2,0)
      WRITE(IPU,904)TSID1,DT1,TSID2,DT2
  904 FORMAT(2X,2A4,3X,A2,2X,2A4,3X,A2)
C***********************************************************
C     PUNCH CARD NUMBER 5
  140 IF((LRS.EQ.0).AND.(LRG.EQ.0).AND.(LAI.EQ.0).AND.(LAPI.EQ.0).AND.
     1(LFI.EQ.0)) GO TO 150
      CALL TSV24(LRS,PO,TSID1,DT1,1)
      CALL TSV24(LRG,PO,TSID2,DT2,1)
      CALL TSV24(LAI,PO,TSID3,DT3,0)
      CALL TSV24(LAPI,PO,TSID4,DT4,0)
      CALL TSV24(LFI,PO,TSID5,DT5,0)
      WRITE(IPU,905)TSID1,TSID2,TSID3,DT3,TSID4,DT4,TSID5,DT5
  905 FORMAT(2(2X,2A4),3(2X,2A4,3X,A2))
C***********************************************************
C     PUNCH CARD NUMBER 5A
  150 IF((LAPIC.EQ.0).AND.(LAETI.EQ.0).AND.(LFRS.EQ.0).AND.(LFEI.EQ.0))
     1GO TO 160
      CALL TSV24(LAPIC,PO,TSID1,DT1,0)
      CALL TSV24(LAETI,PO,TSID2,DT2,0)
      CALL TSV24(LFRS,PO,TSID3,DT3,1)
      CALL TSV24(LFEI,PO,TSID4,DT4,0)
      WRITE(IPU,9051)TSID1,DT1,TSID2,DT2,TSID3,TSID4,DT4
 9051 FORMAT(2(2X,2A4,3X,A2),2X,2A4,7X,2A4,3X,A2)
C***********************************************************
C     PUNCH CARD NUMBER 6
  160 WRITE(IPU,906)PO(LRSPM),(PO(LRSPM+I),I=2,8)
  906 FORMAT(3F5.2,2F5.3,3F5.2)
C***********************************************************
C     PUNCH CARD NUMBER 6A
      IF (IVER.GT.1) GO TO 165
  161 WRITE(IPU,9061)PO(LRSPM+13),PO(LRSPM+1),(PO(LRSPM+I),I=10,12),
     1PO(LRSPM+14),PO(LRSPM+15),PO(LRSPM+9)
 9061 FORMAT(F5.3,F5.2,2F5.3,3F5.2,F5.1,F5.2)
      GO TO 169
  165 IF ((LSC.EQ.0).AND.(LWE.EQ.0)) GO TO 161
      WRITE(IPU,9061)PO(LRSPM+13),PO(LRSPM+1),(PO(LRSPM+I),I=10,12),
     1PO(LRSPM+14),PO(LRSPM+15),PO(LRSPM+9),PO(LRSPM+16)
C***********************************************************
C     PUNCH CARD NUMBER 7
  169 IF(IVOPT.NE.0) GO TO 170
      WRITE(IPU,907)PO(LRSPM+NRSPM),PO(LRSPM+NRSPM+1)
  907 FORMAT(2F5.0)
      GO TO 180
  170 IF(IVOPT.NE.1) GO TO 171
      WRITE(IPU,9071)(PO(LRSPM+NRSPM+I),I=1,2),PO(LRSPM+NRSPM)
 9071 FORMAT(2F5.2,F5.3)
      GO TO 180
  171 IF(IVOPT.NE.2) GO TO 180
      WRITE(IPU,9072)(PO(LRSPM+NRSPM+I),I=1,2),PO(LRSPM+NRSPM)
 9072 FORMAT(2F5.1,F5.2)
C***********************************************************
C     PUNCH CARD NUMBER 8
  180 WRITE(IPU,908)PO(LRGPM),(PO(LRGPM+I),I=1,4)
  908 FORMAT(F5.4,F5.3,3F5.2)
C***********************************************************
C     PUNCH CARD NUMBER 9
      IF(LFRZE.EQ.0) GO TO 190
      WRITE(IPU,909)(PO(LFRZE+I),I=11,13),(PO(LFRZE+I),I=8,10),
     1 (PO(LFRZE+I),I=14,15)
  909 FORMAT(F5.3,2F5.2,F5.1,F5.3,F5.2,F5.4,F5.2)
C***********************************************************
C     PUNCH CARD NUMBER 10
  190 IF((IVOPT.GT.0).OR.(LFRZE.GT.0)) GO TO 191
      WRITE(IPU,910)(CO(I),I=1,4)
  910 FORMAT(4F5.2,5X,F5.1,F5.2)
      GO TO 200
  191 IF(IVOPT.GT.0) GO TO 192
      WRITE(IPU,910)(CO(I),I=1,4),CO(6),CO(7)
      GO TO 200
  192 IF(IVOPT.NE.1) GO TO 195
      IF(LFRZE.GT.0) GO TO 193
      WRITE(IPU,9101)(CO(I),I=1,5)
 9101 FORMAT(5F5.2,F5.1,F5.2)
      GO TO 200
  193 WRITE(IPU,9101)(CO(I),I=1,7)
      GO TO 200
  195 IF(IVOPT.NE.2) GO TO 200
      IF(LFRZE.NE.0) GO TO 196
      WRITE(IPU,9102) (CO(I),I=1,5)
 9102 FORMAT(4F5.2,2F5.1,F5.2)
      GO TO 200
  196 WRITE(IPU,9102)(CO(I),I=1,7)
C***********************************************************
C     PUNCH CARD NUMBER 11
  200 IF(LPROT.EQ.0) GO TO 990
      IF(MAINUM.LT.3) GO TO 990
      NP=0
      DO 205 I=1,3
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
  205 CONTINUE
  210 IF(NP.EQ.0) GO TO 990
      WRITE(IPU,911) (IM(I),IY(I),LM(I),LY(I),I=1,NP)
  911 FORMAT(6(3X,I2,1X,I4))
C***********************************************************
C     END OF PUNCHED CARD SUBROUTINE
  990 IF(ITRACE.LT.1) GO TO 999
      WRITE(IODBUG,912)
  912 FORMAT(1H0,'**EXIT PUC24')
  999 RETURN
      END
