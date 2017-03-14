C MEMBER TAB24
C  (from old member FCTAB24)
C
      SUBROUTINE TAB24(TO,LEFT,IUSET,NXT,LPO,PO,LCO,TS,MTS,
     -LWORK,IDT)
C*********************************************************
C  THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE
C  API-CONT OPERATION
C**********************************************************
C  INITIALLY WRITTEN BY -- ERIC ANDERSON,  HRL,  MARCH 1990
C**********************************************************
C
      INTEGER TO(1)
      DIMENSION PO(1),TS(MTS)
      DIMENSION SNAME(2)
C
C*********************************************************
C   COMMON BLOCKS
C*********************************************************
C
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab24.f,v $
     . $',                                                             '
     .$Id: tab24.f,v 1.1 1995/09/17 18:49:11 dws Exp $
     . $' /
C    ===================================================================
C
C
C*********************************************************
C  DATA STATEMENTS
C*********************************************************
C
      DATA SNAME/4HTAB2,4H4   /
      DATA BLANK/4H    /
C
C*********************************************************
C  CHECK IF DEBUG ON -- TRACE LEVEL=1
C*********************************************************
C
      CALL FPRBUG(SNAME,1,24,IBUG)
C
C*********************************************************
C   GET CONTROL VARIABLES
C*********************************************************
C
      IDT=24
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
      LFRZE=PO(24)
      LAPIC=PO(29)
      LAETI=PO(30)
      LFRS=PO(31)
      LENGTH=20
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
      GO TO 195
C
C*********************************************************
C  SPACE IS AVAILABLE-- MAKE ENTRIES INTO THE ARRAY
C*********************************************************
C
  100 TO(1)=24
      TO(2)=NXT+LENGTH
      TO(3)=LPO
      TO(4)=LCO
C
C*********************************************************
C   PRECIPITATION
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
  102 IF (LPE.EQ.0) GO TO 105
      JDT=24
      CALL CKINPT(PO(LPE),PO(LPE+2),JDT,LD,TS,MTS,IERR)
      TO (7)=LD
      GO TO 110
  105 TO(7)=0
C*********************************************************
C   AREAL EXTENT OF SNOW COVER
C*********************************************************
  110 IF (LSC.EQ.0) GO TO 115
      JDT=PO(LSC+3)
      CALL CKINPT(PO(LSC),PO(LSC+2),JDT,LD,TS,MTS,IERR)
      TO(8)=LD
      GO TO 120
  115 TO(8)=0
C*********************************************************
C  WATER-EQUIVALENT OF SNOW
C*********************************************************
C
  120 IF (LWE.EQ.0) GO TO 125
      JDT=PO(LWE+3)
      CALL CKINPT(PO(LWE),PO(LWE+2),JDT,LD,TS,MTS,IERR)
      TO(9)=LD
      GO TO 130
  125 TO(9)=0
C*********************************************************
C   AIR TEMPERATURE
C*********************************************************
C
  130 IF (LTA.EQ.0) GO TO 135
      JDT=PO(LTA+3)
      CALL CKINPT(PO(LTA),PO(LTA+2),JDT,LD,TS,MTS,IERR)
      TO(10)=LD
      GO TO 140
  135 TO(10)=0
C*********************************************************
C    STORM RUNOFF
C*********************************************************
C
  140 IF (LRS.EQ.0) GO TO 145
      CALL FINDTS(PO(LRS),PO(LRS+2),ITP,LD,LTS,DIM)
      TO(11)=LD
      IF (LTS.GT.0) TS(LTS+8)=1.01
      GO TO 150
  145 TO(11)=0
C*********************************************************
C     GROUNDWATER RUNOFF OR DISCHARGE
C*********************************************************
C
  150 IF (LRG.EQ.0) GO TO 155
C
      CALL FINDTS(PO(LRG),PO(LRG+2),ITP,LD,LTS,DIM)
      TO(12)=LD
      IF (LTS.GT.0) TS(LTS+8)=1.01
      GO TO 160
  155 TO(12)=0
C*********************************************************
C   ANTECEDENT INDEX
C*********************************************************
C
  160 IF (LAI.EQ.0) GO TO 165
      JDT=PO(LAI+3)
      CALL FINDTS(PO(LAI),PO(LAI+2),JDT,LD,LTS,DIM)
      TO(13)=LD
      IF (LTS.GT.0) TS(LTS+8)=1.01
      GO TO 170
  165 TO(13)=0
C*********************************************************
C  ANTECEDENT PRECIPITATION INDEX
C*********************************************************
C
  170 IF (LAPI.EQ.0) GO TO 175
      JDT=PO(LAPI+3)
      CALL FINDTS(PO(LAPI),PO(LAPI+2),JDT,LD,LTS,DIM)
      TO(14)=LD
      IF(LTS.GT.0) TS(LTS+8)=1.01
      GO TO 180
  175 TO(14)=0
C*********************************************************
C     FROST INDEX
C*********************************************************
C
  180 IF (LFRZE.EQ.0) GO TO 185
      JDT=PO(LFRZE+3)
      IF (JDT.EQ.0) GO TO 185
      CALL FINDTS(PO(LFRZE),PO(LFRZE+2),JDT,LD,LTS,DIM)
      TO(15)=LD
      IF (LTS.GT.0) TS(LTS+8)=1.01
      GO TO 200
  185 TO(15)=0
C*********************************************************
C     API CONTENTS
C*********************************************************
C
  200 IF (LAPIC.EQ.0) GO TO 205
      JDT=PO(LAPIC+3)
      CALL FINDTS(PO(LAPIC),PO(LAPIC+2),JDT,LD,LTS,DIM)
      TO(16)=LD
      IF (LTS.GT.0) TS(LTS+8)=1.01
      GO TO 210
  205 TO(16)=0
C
C********************************************************
C     ANTECEDENT EVAPORATION INDEX
C********************************************************
C
  210 IF (LAETI.EQ.0) GO TO 215
      IF (IVOPT.EQ.2) GO TO 215
      JDT=PO(LAETI+3)
      CALL FINDTS(PO(LAETI),PO(LAETI+2),JDT,LD,LTS,DIM)
      TO(17)=LD
      IF (LTS.GT.0) TS(LTS+8)=1.01
      GO TO 220
  215 TO(17)=0
C
C*******************************************************
C     ANTECEDENT TEMPERATURE INDEX
C*******************************************************
C
  220 IF (LFRS.EQ.0) GO TO 225
      IF (IVOPT.EQ.1) GO TO 225
      JDT=PO(LFRS+3)
      CALL FINDTS(PO(LFRS),PO(LFRS+2),JDT,LD,LTS,DIM)
      TO(18)=LD
      IF (LTS.GT.0) TS(LTS+8)=1.01
      GO TO 230
  225 TO(18)=0
C
C*********************************************************
C     FROST EFFICIENCY INDEX
C*********************************************************
C
  230 IF (LFRZE.EQ.0) GO TO 235
      JDT=PO(LFRZE+7)
      IF (JDT.EQ.0) GO TO 235
      CALL FINDTS(PO(LFRZE+4),PO(LFRZE+6),JDT,LD,LTS,DIM)
      TO(19)=LD
      IF (LTS.GT.0) TS(LTS+8)=1.01
      GO TO 240
  235 TO(19)=0
C
C***************************************
C   FRACTION SURFACE RUNOFF
C***************************************
C
  240 IF (LFRS.EQ.0) GO TO 245
      CALL FINDTS(PO(LFRS),PO(LFRS+2),ITP,LD,LTS,DIM)
      TO(20)=LD
      IF (LTS.GT.0) TS(LTS+8)=1.01
      GO TO 190
  245 TO(20)=0
C
  190 IUSET=LENGTH
C********************************************************8
C        ALL ENTRIES HAVE BEEN MADE
C*********************************************************
C        CHECK FOR DEBUG OUTPUT
C*********************************************************
C
      IF (IBUG.EQ.0) GO TO 195
      WRITE(IODBUG,900) IDT
  900 FORMAT(1H0,'API-CONT TAB DEBUG--COMPUTIONAL DT=',I2,
     -/6X,'CONTENTS OF THE TO ARRAY')
      WRITE(IODBUG,901) (TO(I),I=1,IUSET)
  901 FORMAT(1H0,20I6)
C
  195 IF (ITRACE.LT.1) GO TO 199
      WRITE(IODBUG,902) SNAME
C
  902 FORMAT(1H0,'**EXIT',1X,2A4)
C
C*********************************************************
C    CONTENTS OF THE TO ARRAY ARE AS FOLLOWS
C
C   POSITION               CONTENTS
C      1        OPERATION NUMBER=24
C      2        LOCATION OF NEXT OPERATION IN T ARRAY
C      3        LOCATION OF PO ARRAY IN P ARRAY
C      4        LOCATION OF CO ARRAY IN C ARRAY
C      5        LOCATION OF PRECIP IN D ARRAY
C      6        LOCATION TO PUT TOTAL RUNOFF IN D ARRAY
C                                =0 IF NOT USED
C      7        LOCATION OF PE IN D, =0 IF NOT USED
C      8        LOCATION OF AESC IN D,  =0 IF NOT USED
C      9        LOCATION OF WE IN D,  =0 IF NOT USED
C     10        LOCATION OF TA IN D, =0 IF NOT USED
C     11        LOCATION TO PUT STORM RUNOFF IN D,
C                                =0 IF NOT USED
C     12        LOCATION TO PUT GROUNDWATER RUNOFF IN D,
C                                =0 IF NOT USED
C     13        LOCATION TO PUT ANTECEDENT INDEX IN D,
C                                =0 IF NOT USED
C     14        LOCATION TO PUT API IN D, = 0 IF NOT USED
C     15        LOCATION TO PUT FROST INDEX IN D, =0 IF NOT USED
C     16        LOCATION TO PUT APIC IN D, =0 IF NOT USED
C     17        LOCATION TO PUT AEI IN D, =0 IF NOT USED
C     18        LOCATION TO PUT ATI IN D, =0 IF NOT USED
C     19        LOCATION TO PUT FEI IN D, =0 IF NOT USED
C     20        LOCATION TO PUT FRACTION SURFACE RUNOFF IN D,
C                                =0 IF NOT USED
C****************************************************************
C
  199 RETURN
      END
