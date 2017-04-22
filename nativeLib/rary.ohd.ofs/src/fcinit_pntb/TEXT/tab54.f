C MEMBER TAB54
C
      SUBROUTINE TAB54(TO,LEFT,IUSET,NXT,LPO,PO,LCO,TS,MTS,LWORK,IDT)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE SIMPLE
C        WATER BALANCE (SWB-NILE) OPERATION
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C        QINGYUN DUAN - GCIP CLIMATE PROJECT     AUGUST 1995
C.......................................
C
      INTEGER TO(1)
      DIMENSION PO(1),TS(MTS)
      DIMENSION SNAME(2)
C
C*********************************************************
C   COMMON BLOCKS
C*********************************************************
C
C                                           *--> FROM COMMON.FDBUG
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab54.f,v $
     . $',                                                             '
     .$Id: tab54.f,v 1.2 1998/04/07 11:41:58 page Exp $
     . $' /
C    ===================================================================
C
C
C
C*********************************************************
C  DATA STATEMENTS
C*********************************************************
C
      DATA SNAME/4HTAB5,4H4   /
      DATA BLANK/4H    /
C
C*********************************************************
C  CHECK IF DEBUG ON -- TRACE LEVEL=1
C*********************************************************
C
      CALL FPRBUG(SNAME,1,54,IBUG)
C
C*********************************************************
C   GET CONTROL VARIABLES
C*********************************************************
C
      IDT=24
      ITP=PO(7)
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
      LENGTH=15
      LWORK=0
C
C*********************************************************
C  CHECK IF SPACE AVAILABLE IN THE T ARRAY
C*********************************************************
C
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF (IERR.EQ.0) GO TO 100
      IUSET=0
      IDT=0
      GO TO 205
C
C*********************************************************
C  SPACE IS AVAILABLE-- MAKE ENTRIES INTO THE ARRAY
C*********************************************************
C
  100 TO(1)=54
      TO(2)=NXT+LENGTH
      TO(3)=LPO
      TO(4)=LCO
C
C*********************************************************
C   PRECIPITATION (RAIN+SNOWMELT)
C*********************************************************
C
      CALL CKINPT(PO(8),PO(10),ITP,LD,TS,MTS,IERR)
      TO (5)=LD
C
C*********************************************************
C  TOTAL RUNOFF
C*********************************************************
C
      IF (PO(13).EQ.BLANK) GO TO 101
      CALL FINDTS(PO(11),PO(13),ITP,LD,LTS,DIM)
      TO(6)=LD
      IF (LTS.GT.0) TS(LTS+8)=1.01
      GO TO 102
  101 TO(6)=0
C
C*********************************************************
C  POTENTIAL EVAPORATION
C*********************************************************
C
  102 IF (PO(LET+2).EQ.BLANK) GO TO 105
      JDT=24
      CALL CKINPT(PO(LET),PO(LET+2),JDT,LD,TS,MTS,IERR)
      TO (7)=LD
      GO TO 110
  105 TO(7)=0
C
C*********************************************************
C    STORM RUNOFF
C*********************************************************
C
  110 IF (LRS.EQ.0) GO TO 115
      CALL FINDTS(PO(LRS),PO(LRS+2),ITP,LD,LTS,DIM)
      TO(8)=LD
      IF (LTS.GT.0) TS(LTS+8)=1.01
      GO TO 120
  115 TO(8)=0
C
C*********************************************************
C     GROUNDWATER RUNOFF OR DISCHARGE
C*********************************************************
C
  120 IF (LRG.EQ.0) GO TO 125
      CALL FINDTS(PO(LRG),PO(LRG+2),ITP,LD,LTS,DIM)
      TO(9)=LD
      IF (LTS.GT.0) TS(LTS+8)=1.01
      GO TO 130
  125 TO(9)=0
C
C*********************************************************
C     SOIL MOISTURE CONTENTS
C*********************************************************
C
  130 IF (LSM.EQ.0) GO TO 135
      JDT=PO(LSM+3)
      CALL FINDTS(PO(LSM),PO(LSM+2),JDT,LD,LTS,DIM)
      TO(10)=LD
      IF (LTS.GT.0) TS(LTS+8)=1.01
      GO TO 140
  135 TO(10)=0
C
C*********************************************************
C   AIR TEMPERATURE
C*********************************************************
C
  140 IF (LTA.EQ.0) GO TO 145
      JDT=PO(LTA+3)
      CALL CKINPT(PO(LTA),PO(LTA+2),JDT,LD,TS,MTS,IERR)
      TO(11)=LD
      GO TO 150
  145 TO(11)=0
C
C*********************************************************
C   PRECIPITATION (RAIN+SNOWFALL)
C*********************************************************
C
  150 IF (LPP.EQ.0) GO TO 155
      JDT=PO(LPP+3)
      CALL CKINPT(PO(LPP),PO(LPP+2),JDT,LD,TS,MTS,IERR)
      TO(12)=LD
      GO TO 160
  155 TO(12)=0
C
C*********************************************************
C  WATER-EQUIVALENT OF SNOW
C*********************************************************
C
  160 IF (LWE.EQ.0) GO TO 165
      JDT=PO(LWE+3)
      CALL CKINPT(PO(LWE),PO(LWE+2),JDT,LD,TS,MTS,IERR)
      TO(13)=LD
      GO TO 170
  165 TO(13)=0
C
C*********************************************************
C   FROST EFFICIENCY INDEX
C*********************************************************
C
  170 IF (LFE.EQ.0) GO TO 175
      JDT=PO(LFE+3)
      CALL FINDTS(PO(LFE),PO(LFE+2),JDT,LD,LTS,DIM)
      TO(14)=LD
      IF(LTS.GT.0) TS(LTS+8)=1.01
      GO TO 180
  175 TO(14)=0
C
C*********************************************************
C   SNOWFALL
C*********************************************************
C
  180 IF (LSN.EQ.0) GO TO 185
      JDT=PO(LSN+3)
      CALL FINDTS(PO(LSN),PO(LSN+2),JDT,LD,LTS,DIM)
      TO(15)=LD
      IF(LTS.GT.0) TS(LTS+8)=1.01
      GO TO 200
  185 TO(15)=0
C
C********************************************************8
C        ALL ENTRIES HAVE BEEN MADE
C*********************************************************
C        CHECK FOR DEBUG OUTPUT
C*********************************************************
C
  200 IUSET=LENGTH
      IF (IBUG.EQ.0) GO TO 205
      WRITE(IODBUG,900) IDT
  900 FORMAT(1H0,'SWB-NILE TAB DEBUG--COMPUTIONAL DT=',I2,
     -/6X,'CONTENTS OF THE TO ARRAY')
      WRITE(IODBUG,901) (TO(I),I=1,IUSET)
  901 FORMAT(1H0,20I6)
C
  205 IF (ITRACE.LT.1) GO TO 199
      WRITE(IODBUG,902)
  902 FORMAT(1H0,'**EXIT TAB54')
C
C*********************************************************
C    CONTENTS OF THE TO ARRAY ARE AS FOLLOWS
C
C   POSITION               CONTENTS
C      1        OPERATION NUMBER=54
C      2        LOCATION OF NEXT OPERATION IN T ARRAY
C      3        LOCATION OF PO ARRAY IN P ARRAY
C      4        LOCATION OF CO ARRAY IN C ARRAY
C      5        LOCATION OF PRECIP IN D ARRAY
C      6        LOCATION TO PUT TOTAL RUNOFF IN D ARRAY
C                                =0 IF NOT USED
C      7        LOCATION OF PE IN D, =0 IF NOT USED
C      8        LOCATION TO PUT STORM RUNOFF IN D,
C                                =0 IF NOT USED
C      9        LOCATION TO PUT GROUNDWATER RUNOFF IN D,
C                                =0 IF NOT USED
C     10        LOCATION TO PUT SOIL MOISTURE CONTENT IN D,
C                                =0 IF NOT USED
C     11        LOCATION OF TA IN D, =0 IF NOT USED
C     12        LOCATION OF PP IN D,  =0 IF NOT USED
C     13        LOCATION OF WE IN D,  =0 IF NOT USED
C     14        LOCATION TO PUT FROST EFFICIENCY INDEX IN D,
C                                =0 IF NOT USED
C     15        LOCATION TO PUT SNOWFALL IN D, =0 IF NOT USED
C****************************************************************
C
  199 RETURN
      END
