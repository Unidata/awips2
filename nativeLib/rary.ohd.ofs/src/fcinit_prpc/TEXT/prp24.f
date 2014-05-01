C MEMBER PRP24
C  (from old member FCPRP24)
C
      SUBROUTINE PRP24(PO)
C                             LAST UPDATE: 05/05/95.08:39:35 BY $WC30EA
C
C**********************************************************
C  THIS IS THE PRINT PARAMETER ROUTINE FOR THE API-CONT
C  OPERATION
C***********************************************************
C  INITIALLY WRITTEN BY  ERIC ANDERSON -- , HRL,  MARCH 1990
C************************************************************
C
      DIMENSION PO(1)
      DIMENSION SNAME(2),IYR(3),IMO(3),LYR(3),LMO(3)
C
C******************************************
C   COMMON BLOCKS
C******************************************
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fprog'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp24.f,v $
     . $',                                                             '
     .$Id: prp24.f,v 1.1 1995/09/17 18:50:07 dws Exp $
     . $' /
C    ===================================================================
C
C
C******************************************
C   DATA STATEMENTS
C******************************************
C
      DATA SNAME/4HPRP2,4H4   /
      DATA BLANK/4H    /
C
C******************************************
C  CHECK TRACE LEVEL
C******************************************
C
      IF (ITRACE.LT.1) GO TO 100
      WRITE(IODBUG,900) SNAME
  900 FORMAT(1H0,'**ENTER',1X,2A4)
C
C******************************************
C  GET CONTROL VARIABLES
C*******************************************
C
  100 IVER=PO(1)
      ITP=PO(7)
      LPE=PO(15)
      LSC=PO(16)
      LWE=PO(17)
      LTA=PO(18)
      LRS=PO(19)
      LRG=PO(20)
      LAI=PO(21)
      LAPI=PO(22)
      LFRZE=PO(24)
      LAPIC=PO(29)
      LAETI=PO(30)
      LFRS=PO(31)
      LRSPM=PO(26)
      LRGPM=PO(27)
      LPROT=PO(23)
      LSUMS=PO(32)
      IVOPT=PO(14)
      NRSPM=17
      IF (IVER.EQ.1) NRSPM=16
C
C*******************************************
C  START THE PARAMETER DISPLAY -- DESCRIPTION AND TIME SERIES
C********************************************
C
      WRITE(IPR,901) (PO(I),I=2,6)
  901 FORMAT(1H0,10X,'CONTINUOUS API OPERATION FOR',1X,5A4)
      WRITE(IPR,902)
  902 FORMAT(1H0,20X,'TIME SERIES USED BY THIS OPERATION',/16X,
     -'CONTENTS',14X,'I.D.',7X,'TYPE',5X,'TIME INTERVAL')
      WRITE(IPR,903) (PO(I),I=8,10),ITP,PO(LRSPM)
  903 FORMAT(1H ,10X,'RAIN+MELT/PRECIP',9X,2A4,5X,A4,7X,I2,1X,
     -'HOURS',6X,'PXADJ=',F5.2)
      IF (PO(13).EQ.BLANK) GO TO 101
      WRITE(IPR,904) (PO(I),I=11,13),ITP
  904 FORMAT(1H ,10X,'TOTAL RUNOFF',13X,2A4,5X,A4,7X,I2,1X,'HOURS')
C
  101 IF (LPE.EQ.0) GO TO 105
      JDT=24
      LM=LPE-1
      WRITE(IPR,905) (PO(LM+I),I=1,3),JDT,PO(LM+4)
  905 FORMAT(1H ,10X,'POTENTIAL EVAP',11X,2A4,5X,A4,7X,I2,1X,'HOURS',
     1  6X,'PEADJ=',F5.2)
C
  105 IF (LSC.EQ.0) GO TO 110
      JDT=PO(LSC+3)
      LM=LSC-1
      WRITE(IPR,906) (PO(LM+I),I=1,3),JDT
  906 FORMAT(1H ,10X,'AREAL EXTENT OF SNOW',5X,2A4,5X,A4,7X,I2,1X,
     -'HOURS')
C
  110 IF (LWE.EQ.0) GO TO 115
      JDT=PO(LWE+3)
      LM=LWE-1
      WRITE(IPR,907) (PO(LM+I),I=1,3),JDT
  907 FORMAT(1H ,10X,'SNOW WATER-EQUIVALENT',4X,2A4,5X,A4,7X,I2,1X,
     -'HOURS')
C
  115 IF (LTA.EQ.0) GO TO 120
      JDT=PO(LTA+3)
      LM=LTA-1
      WRITE(IPR,908) (PO(LM+I),I=1,3),JDT
  908 FORMAT(1H ,10X,'AIR TEMPERATURE',10X,2A4,5X,A4,7X,I2,1X,'HOURS')
      DELEV=PO(LTA+4)
      IF (DELEV.EQ.0.0) GO TO 120
      WRITE (IPR,933) DELEV,PO(LTA+5),PO(LTA+6)
  933 FORMAT(1H+,73X,'DELEV=',F5.0,3X,'LAPSE RATES--MAX=',F5.1,2X,
     -  'MIN=',F5.1)
C
  120 IF (LRS.EQ.0) GO TO 125
      LM=LRS-1
      WRITE(IPR,909) (PO(LM+I),I=1,3),ITP
  909 FORMAT(1H ,10X,'STORM RUNOFF',13X,2A4,5X,A4,7X,I2,1X,'HOURS')
C
  125 IF (LRG.EQ.0) GO TO 130
      LM=LRG-1
      WRITE(IPR,910) (PO(LM+I),I=1,3),ITP
  910 FORMAT(1H ,10X,'GROUNDWATER RUNOFF',7X,2A4,5X,A4,7X,I2,1X,'HOURS')
      IF (PO(LM+4).EQ.0.0) GO TO 130
      WRITE(IPR,936) PO(LM+4)
  936 FORMAT(1H+,73X,'AREA=',F8.2,1X,'MI2')
C
  130 IF (LAI.EQ.0) GO TO 135
      JDT=PO(LAI+3)
      LM=LAI-1
      WRITE(IPR,911) (PO(LM+I),I=1,3),JDT
  911 FORMAT(1H ,10X,'TENSION INDEX',13X,2A4,5X,A4,7X,I2,1X,'HOURS')
C
  135 IF (LAPI.EQ.0) GO TO 140
      JDT=PO(LAPI+3)
      LM=LAPI-1
      WRITE(IPR,912) (PO(LM+I),I=1,3),JDT
  912 FORMAT(1H ,10X,'API',22X,2A4,5X,A4,7X,I2,1X,'HOURS')
C
  140 IF (LFRZE.EQ.0) GO TO 142
      JDT=PO(LFRZE+3)
      IF (JDT.EQ.0) GO TO 141
      LM=LFRZE-1
      WRITE(IPR,913) (PO(LM+I),I=1,3),JDT
  913 FORMAT(1H ,10X,'FROST INDEX',14X,2A4,5X,A4,7X,I2,1X,'HOURS')
  141 JDT=PO(LFRZE+7)
      IF (JDT.EQ.0) GO TO 142
      LM=LFRZE+3
      WRITE(IPR,935) (PO(LM+I),I=1,3),JDT
  935 FORMAT(1H ,10X,'FROST EFFICIENCY INDEX',3X,2A4,5X,A4,7X,I2,1X,
     -'HOURS')
C
  142 IF (LAPIC.EQ.0) GO TO 144
      JDT=PO(LAPIC+3)
      LM=LAPIC-1
      WRITE(IPR,914) (PO(LM+I),I=1,3),JDT
  914 FORMAT(1H ,10X,'API CONTENTS',13X,2A4,5X,A4,7X,I2,1X,'HOURS')
C
  144 IF (LAETI.EQ.0) GO TO 148
      JDT=PO(LAETI+3)
      LM=LAETI-1
      IF (IVOPT.EQ.2) GO TO 146
      WRITE(IPR,9141) (PO(LM+I),I=1,3),JDT
 9141 FORMAT(1H ,10X,'AEI',22X,2A4,5X,A4,7X,I2,1X,'HOURS')
      GO TO 148
  146 WRITE(IPR,9142) (PO(LM+I),I=1,3),JDT
 9142 FORMAT(1H ,10X,'ATI',22X,2A4,5X,A4,7X,I2,1X,'HOURS')
C
  148 IF (LFRS.EQ.0) GO TO 150
      LM=LFRS-1
      WRITE(IPR,937) (PO(LM+I),I=1,3),ITP
  937 FORMAT(1H ,10X,'FRACTION SURFACE RUNOFF',2X,2A4,5X,A4,7X,I2,
     1  1X,'HOURS')
C
C*******************************************
C  PARAMETER VALUES
C*******************************************
C
  150 WRITE(IPR,915)
  915 FORMAT(1H0,10X,'PARAMETER VALUES - ENGLISH UNITS')
      WRITE(IPR,916)
  916 FORMAT(1H0,10X,'SURFACE-MAJOR',10X,'AIXW',3X,'AIXD',5X,
     -'CW',5X,'CD',5X,'CS',3X,'SMIX',3X,'FRSX')
      IF (MAINUM.EQ.1) WRITE(IPR,938)
  938 FORMAT(1H+,85X,'AIADJ')
C
      AIXW=PO(LRSPM+2)
      AIXD=PO(LRSPM+3)
      CW=PO(LRSPM+4)
      CD=PO(LRSPM+5)
      SMIX=PO(LRSPM+6)
      CS=PO(LRSPM+7)
      FRSX=PO(LRSPM+8)
      WRITE(IPR,917) AIXW,AIXD,CW,CD,CS,SMIX,FRSX
  917 FORMAT(1H ,30X,2F7.2,2F7.3,3F7.2)
      IF (MAINUM.EQ.1) WRITE(IPR,939) PO(25)
  939 FORMAT(1H+,80X,F10.2)
      WRITE(IPR,918)
  918 FORMAT(1H0,10X,'SURFACE-MINOR',9X,'PIMPV',3X,'APIK',4X,'PEX',
     -  4X,'PEN',4X,'EFC',3X,'RIVA',3X,'RVAI',3X,'APIX',2X,'APIKS')
C
      APIK=PO(LRSPM+1)
      PEX=PO(LRSPM+10)
      PEN=PO(LRSPM+11)
      EFC=PO(LRSPM+12)
      PIMPV=PO(LRSPM+13)
      RIVA=PO(LRSPM+14)
      RVAI=PO(LRSPM+15)
      APIX=PO(LRSPM+9)
      IF (IVER.GT.1) GO TO 152
  151 WRITE(IPR,919) PIMPV,APIK,PEX,PEN,EFC,RIVA,RVAI,APIX
  919 FORMAT(1H ,30X,F7.3,8F7.2)
      GO TO 153
  152 IF ((LSC.EQ.0).AND.(LWE.EQ.0)) GO TO 151
      APIKS=PO(LRSPM+16)
      WRITE(IPR,919) PIMPV,APIK,PEX,PEN,EFC,RIVA,RVAI,APIX,APIKS
C
  153 IF (IVOPT.NE.0) GO TO 155
      WRITE(IPR,920)
  920 FORMAT(1H0,10X,'VARIATION-1ST QUAD',6X,'WKW',4X,'WKD')
C
      WKW=PO(LRSPM+NRSPM)
      WKD=PO(LRSPM+NRSPM+1)
      WRITE(IPR,921) WKW,WKD
  921 FORMAT(1H ,30X,2F7.1)
      GO TO 160
C
  155 IF (IVOPT.NE.1) GO TO 156
      WRITE(IPR,922)
  922 FORMAT(1H0,10X,'VARIATION-1ST QUAD',5X,'AEIX',3X,'AEIN',3X,
     -'AEIK')
C
      AEIK=PO(LRSPM+NRSPM)
      AEIX=PO(LRSPM+NRSPM+1)
      AEIN=PO(LRSPM+NRSPM+2)
      WRITE(IPR,923) AEIX,AEIN,AEIK
  923 FORMAT(1H ,30X,2F7.2,F7.3)
      GO TO 160
  156 WRITE(IPR,924)
  924 FORMAT(1H0,10X,'VARIATION-1ST QUAD',5X,'ATIX',3X,'ATIN',3X,
     -'ATIR')
C
      ATIR=PO(LRSPM+NRSPM)
      ATIX=PO(LRSPM+NRSPM+1)
      ATIN=PO(LRSPM+NRSPM+2)
      WRITE(IPR,925) ATIX,ATIN,ATIR
  925 FORMAT(1H ,30X,2F7.0,F7.2)
C
  160 WRITE(IPR,926)
  926 FORMAT(1H0,10X,'BASEFLOW',15X,'BFPK',3X,'BFIK',3X,'BFIM',
     -  3X,'AICR',5X,'CG')
C
      BFPK=PO(LRGPM)
      BFIK=PO(LRGPM+1)
      BFIM=PO(LRGPM+2)
      AICR=PO(LRGPM+3)
      CG=PO(LRGPM+4)
      WRITE(IPR,927) BFPK,BFIK,BFIM,AICR,CG
  927 FORMAT(1H ,30X,F7.4,F7.3,3F7.2)
C
      IF (LFRZE.EQ.0) GO TO 170
      WRITE(IPR,928)
  928 FORMAT(1H0,10X,'FROZEN GROUND',9X,'CSOIL',2X,'CSNOW',4X,'GHC',
     -  3X,'FICR',5X,'CF',5X,'CP',5X,'CT',4X,'EFA')
C
      FICR=PO(LFRZE+8)
      CF=PO(LFRZE+9)
      CP=PO(LFRZE+10)
      CSOIL=PO(LFRZE+11)
      CSNOW=PO(LFRZE+12)
      GHC=PO(LFRZE+13)
      CT=PO(LFRZE+14)
      EFA=PO(LFRZE+15)
      WRITE(IPR,929) CSOIL,CSNOW,GHC,FICR,CF,CP,CT,EFA
  929 FORMAT(1H ,30X,3F7.2,F7.1,F7.3,F7.2,F7.4,F7.2)
C
C*******************************************
C  DETAILED OUTPUT DISPLAY
C*******************************************
C
  170 IF (LPROT.EQ.0) GO TO 180
      WRITE(IPR,930)
  930 FORMAT(1H0,10X,'PRINT DETAILED OUTPUT')
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
      WRITE(IPR,931) (IMO(I),IYR(I),LMO(I),LYR(I),I=1,N)
  931 FORMAT(1H ,20X,3(5X,I2,'/',I4,'-',I2,'/',I4))
C
C***************************************
C    STORAGE OF SUMS
C***************************************
C
  180 IF (LSUMS.EQ.0) GO TO 190
      WRITE(IPR,934)
  934 FORMAT(1H0,10X,'WATER BALANCE SUMS ARE STORED')
C
C*******************************************
C    CHECK FOR TRACE LEVEL
C*******************************************
C
  190 IF (ITRACE.LT.1) GO TO 199
      WRITE(IODBUG,932) SNAME
  932 FORMAT(1H0,'**EXIT',1X,2A4)
C
  199 RETURN
      END
