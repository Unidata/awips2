C MEMBER PRP54
C
      SUBROUTINE PRP54(PO)
C
C.......................................
C  THIS IS THE PRINT PARAMETER ROUTINE FOR THE SWB-NILE OPERATION
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C        QINGYUN DUAN - GCIP CLIMATE PROJECT SEPTEMBER 1995 VERSION 1
C.......................................
C
      DIMENSION PO(1),IYR(3),IMO(3),LYR(3),LMO(3)
      DIMENSION DESCRP(5),PID(2),RID(2),DET(2,2)
C
C******************************************
C   COMMON BLOCKS
C******************************************
C
C                                           *--> FROM COMMON.FDBUG
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C                                           *--> FROM COMMON.IONUM
      COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp54.f,v $
     . $',                                                             '
     .$Id: prp54.f,v 1.1 1997/09/22 15:35:05 page Exp $
     . $' /
C    ===================================================================
C
C
C
C******************************************
C   DATA STATEMENTS
C******************************************
C
      DATA DET/4HUNIF,4HORM ,4HDIUR,4HNAL /
C
C******************************************
C  CHECK TRACE LEVEL
C******************************************
C
      IF (ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT (1H0,16H** PRP54 ENTERED)
C
C******************************************
C  GET CONTROL VARIABLES
C*******************************************
C
      DO 100 I=1,5
  100 DESCRP(I)=PO(I+1)
      ITP=PO(7)
      PID(1)=PO(8)
      PID(2)=PO(9)
      PTYPE=PO(10)
      RID(1)=PO(11)
      RID(2)=PO(12)
      RTYPE=PO(13)
      LPM=PO(14)
      NPM=PO(15)
      LET=PO(16)
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
      LPO=PO(29)
      LSUMS=PO(31)
C
C***************************************************************
C  START THE PARAMETER DISPLAY -- DESCRIPTION AND TIME SERIES
C***************************************************************
C
      WRITE(IPR,901) (PO(I),I=2,6)
  901 FORMAT(1H0,10X,'SIMPLE WATER BALANCE (SWB-NILE) OPERATION FOR',
     +       1X,5A4)
      WRITE(IPR,902)
  902 FORMAT(1H0,20X,'TIME SERIES USED BY THIS OPERATION',/16X,
     -       'CONTENTS',14X,'I.D.',7X,'TYPE',5X,'TIME INTERVAL')
      WRITE(IPR,903) (PO(I),I=8,10),ITP,PO(LPM)
  903 FORMAT(1H ,10X,'RAIN+SNOWMELT',12X,2A4,5X,A4,7X,I2,1X,
     -       'HOURS',6X,'PXADJ=',F5.2)
      WRITE(IPR,904) (PO(I),I=11,13),ITP
  904 FORMAT(1H ,10X,'TOTAL RUNOFF',13X,2A4,5X,A4,7X,I2,1X,'HOURS')
C
      JDT=24
      WRITE (IPR,905) PO(LET),PO(LET+1),PO(LET+2),JDT,PO(LPM+1)
  905 FORMAT (1H ,10X,'POTENTIAL ET',13X,2A4,5X,A4,7X,I2,1X,'HOURS',
     -        6X,'PEADJ=',F5.2)
C
C*******************************************
C     PRINT OPTIONAL TIME SERIES USED.
C*******************************************
C
      IF (LRS.EQ.0) GO TO 105
      LM=LRS-1
      WRITE(IPR,906) (PO(LM+I),I=1,3),ITP
  906 FORMAT(1H ,10X,'SURFACE RUNOFF',11X,2A4,5X,A4,7X,I2,1X,'HOURS')
C
  105 IF (LRG.EQ.0) GO TO 110
      LM=LRG-1
      WRITE(IPR,907) (PO(LM+I),I=1,3),ITP
  907 FORMAT(1H ,10X,'GROUNDWATER RUNOFF',7X,2A4,5X,A4,7X,I2,1X,'HOURS')
C
  110 IF (LSM.EQ.0) GO TO 115
      JDT=PO(LSM+3)
      LM=LSM-1
      WRITE(IPR,908) (PO(LM+I),I=1,3),JDT
  908 FORMAT(1H ,10X,'SOIL MOISTURE CONTENT',4X,2A4,5X,A4,7X,I2,
     +       1X,'HOURS')
C
  115 IF (LTA.EQ.0) GO TO 120
      JDT=PO(LTA+3)
      LM=LTA-1
      WRITE(IPR,909) (PO(LM+I),I=1,3),JDT
  909 FORMAT(1H ,10X,'AIR TEMPERATURE',10X,2A4,5X,A4,7X,I2,1X,'HOURS')
C
  120 IF (LPP.EQ.0) GO TO 125
      JDT=PO(LPP+3)
      LM=LPP-1
      WRITE(IPR,910) (PO(LM+I),I=1,3),JDT
  910 FORMAT(1H ,10X,'RAIN+SNOWFALL',12X,2A4,5X,A4,7X,I2,1X,'HOURS')
C
  125 IF (LWE.EQ.0) GO TO 130
      JDT=PO(LWE+3)
      LM=LWE-1
      WRITE(IPR,911) (PO(LM+I),I=1,3),JDT
  911 FORMAT(1H ,10X,'SNOW WATER-EQUIVALENT',4X,2A4,5X,A4,7X,I2,1X,
     -       'HOURS')
C
  130 IF (LFE.EQ.0) GO TO 135
      JDT=PO(LFE+3)
      LM=LFE-1
      WRITE(IPR,912) (PO(LM+I),I=1,3),JDT
  912 FORMAT(1H ,10X,'FROST EFFICIENCY INDEX',3X,2A4,5X,A4,7X,I2,1X,
     -       'HOURS')
C
  135 IF (LSN.EQ.0) GO TO 140
      JDT=PO(LSN+3)
      LM=LSN-1
      WRITE(IPR,913) (PO(LM+I),I=1,3),JDT
  913 FORMAT(1H ,10X,'SNOWFALL',17X,2A4,5X,A4,7X,I2,1X,'HOURS')
C
C*******************************************
C     PRINT PARAMETER VALUES.
C*******************************************
C
  140 WRITE (IPR,915)
  915 FORMAT (1H0,/,11X,40HPARAMETER VALUES - CAPACITIES ARE IN MM.)
      IOPTET=PO(LPM+7)
      I=1
      IF(IOPTET.EQ.1) I=2
      WRITE (IPR,916)
  916 FORMAT (1H0,21X,'DMAX',6X,'KG',3X,'ALPSM',3X,'ALPRT',5X,'KDT',
     -        3X,'DAILY ET DIST')
      WRITE (IPR,917) (PO(LPM+J+1),J=1,5),(DET(J,I),J=1,2)
  917 FORMAT (1H ,17X,5F8.3,4X,2A4)
C
      WRITE (IPR,918) (I,I=1,12)
  918 FORMAT (1H0,10X,'16TH OF MONTH VALUES',12I6)
      I=LET+3
      L=I+11
      IF (IOPTET.EQ.1) GO TO 150
      WRITE (IPR,919) (PO(J),J=I,L)
  919 FORMAT (1H ,10X,'ET-DEMAND-MM/DAY',4X,12F6.1)
      GO TO 155
  150 WRITE (IPR,920) (PO(L+I),I=1,12)
  920 FORMAT (1H ,10X,'PE-ADJUSTMENT',7X,12F6.2)
C
  155 IF (LFRZE.EQ.0) GO TO 170
      WRITE(IPR,921)
  921 FORMAT(1H0,10X,'FROZEN GROUND',10X,'KIMP',2X,'DSOIL',2X,'POROS',
     -       4X,'WWP',2X,'CVICE')
      WRITE(IPR,922) (PO(LFRZE+I-1),I=1,5)
  922 FORMAT(1H ,30X,5F7.2)
C
C*******************************************
C  DETAILED OUTPUT DISPLAY
C*******************************************
C
  170 IF (LPROT.EQ.0) GO TO 180
      WRITE(IPR,923)
  923 FORMAT(1H0,10X,'PRINT DETAILED OUTPUT')
      IF (LPROT.EQ.1) GO TO 180
      N=0
      DO 175 I=1,3
      L=LPROT + (I-1)*2 +1
      JM=PO(L)
      IF (JM.EQ.0) GO TO 175
      IYR(I)=(JM-1)/12
      IMO(I)=JM-IYR(I)*12
      JM=PO(L+1)
      IF (JM.EQ.0) GO TO 175
      LYR(I)=(JM-1)/12
      LMO(I)=JM-LYR(I)*12
      N=N+1
  175 CONTINUE
C
      IF (N.EQ.0) GO TO 180
      WRITE(IPR,924) (IMO(I),IYR(I),LMO(I),LYR(I),I=1,N)
  924 FORMAT(1H ,20X,3(5X,I2,'/',I4,'-',I2,'/',I4))
C
C***************************************
C    STORAGE OF SUMS
C***************************************
C
  180 IF (LSUMS.EQ.0) GO TO 190
      WRITE(IPR,925)
  925 FORMAT(1H0,10X,'WATER BALANCE SUMS ARE STORED')
C
C*******************************************
C    CHECK FOR TRACE LEVEL
C*******************************************
C
  190 IF (ITRACE.LT.1) GO TO 199
      WRITE(IODBUG,926)
  926 FORMAT(1H0,'**EXIT PRP54')
C
  199 RETURN
      END
