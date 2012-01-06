C MEMBER EX45
C  (from old member FCEX45)
C @PROCESS LVL(77)
C
C                             LAST UPDATE: 08/24/95.08:23:50 BY $WC30KH
C
      SUBROUTINE EX45(PO,CO,QIN,QOUT)
C.......................................................................
C     THIS IS THE EXECUTION SUBROUTINE FOR THE
C        DELTA-TS OPERATION.
C.......................................................................
C     SUBROUTINE INITIALLY WRITTEN BY
C        KUANG HSU -- HRL   DEC. 1993
C.......................................................................
      DIMENSION PO(*),CO(*),QIN(*),QOUT(*)
      DIMENSION SNAME(2)
C
C     COMMON BLOCKS.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/FCTIME/IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD,
     1NOW(5),LOCAL,NOUTZ,NOUTDS,NLSTZ,IDA,IHR,LDA,LHR,IDADAT
      COMMON/FCARY/IFILLC,NCSTOR,ICDAY(20),ICHOUR(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ex/RCS/ex45.f,v $
     . $',                                                             '
     .$Id: ex45.f,v 1.2 1996/01/14 18:02:07 page Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA SNAME/4HEX45,4H    /
C.......................................
C     TRACE LEVEL FOR SUBROUTINE=1, DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,45,IBUG)
C.......................................................................
C     VALUES OF CONTROL VARIABLES.
  100 IDT=PO(5)
C
C.......................................................................
C     DEBUG OUTPUT - PRINT PO() AND CO()
      ITIQ=(IDA-IDADAT)*24/IDT+IHR/IDT
      LTIQ=(LDA-IDADAT)*24/IDT+LHR/IDT
      IF(IBUG.EQ.0) GO TO 108
      IUSEP=10
      WRITE(IODBUG,903)
 903  FORMAT(1H0,'CONTENTS OF PO AND CO ARRAYS.')
      WRITE(IODBUG,904) (PO(I),I=1,IUSEP)
  904 FORMAT(1X,'PO=',F10.2,2X,2A4,1X,A4,F10.2,2X,2A4,1X,A4,2F10.2)
      WRITE(IODBUG,905) CO(1)
 905  FORMAT(1X,'CO= ',F10.2)
C     DEBUG OUTPUT - PRINT QIN()
      WRITE(IODBUG,940)
  940 FORMAT(/1X,'INPUT TIME SERIES')
      WRITE(IODBUG,942) (QIN(I),I=ITIQ,LTIQ)
  942 FORMAT(5X,10F10.2)
  108 CONTINUE
C.......................................................................
C     COMPUTE THE TOTAL NUMBER OF PERIODS
      NPD=(LDA-IDA)*24/IDT+(LHR-IHR)/IDT+1
C.......................................
C     CHECK IF CARRYOVER NEEDS TO BE SAVED.
      IF(IFILLC.EQ.0) GO TO 110
C
C     CHECK IF INTERMEDIATE CARRYOVER VALUES NEED TO BE SAVED.
      IF(NCSTOR.EQ.0) GO TO 110
      DO 105 J=1,NCSTOR
      KDA=ICDAY(J)
      KHR=ICHOUR(J)
      I=(KDA-IDADAT)*24/IDT+KHR/IDT
      CALL FCWTCO(KDA,KHR,QIN(I),1)
  105 CONTINUE
C.......................................
C     INITIALIZE DAY AND HOUR
  110 KDA=IDA
      KHR=IHR
C.......................................................................
C     BEGIN COMPUTATIONAL TIME INTERVAL LOOP
      DO 200 IPD=1,NPD
C
C     GET LOCATION OF DATA VALUES IN THE TWO ARRAYS.
      I=(KDA-IDADAT)*24/IDT+KHR/IDT
C
C     CHECK IF EITHER VALUE IS MISSING.
      IF(IFMSNG(QIN(I)).EQ.1) GO TO 119
      IF(IPD.GE.2) GO TO 115
      IF(IFMSNG(CO(1)).EQ.1) GO TO 119
      QOUT(I)=QIN(I)-CO(1)
      IF ((CO(1).LT.-988.99).AND.(CO(1).GT.-989.01)) QOUT(I)=0.0
      GO TO 125
 115  IF(IFMSNG(QIN(I-1)).EQ.1) GO TO 119
C
C     BOTH VALUES ARE OKAY
      QOUT(I)=QIN(I)-QIN(I-1)
      GO TO 125
C
C     AT LEAST ONE VALUE IS MISSING - RESULT IS THUS MISSING.
  119 QOUT(I)=-999.0
C     INCREMENT TO NEXT TIME STEP.
  125 KHR=KHR+IDT
      IF(KHR.LE.24) GO TO 200
      KHR=IDT
      KDA=KDA+1
  200 CONTINUE
C
C     SAVE FINAL PERIOD CARRYOVER
      IF(IFILLC.EQ.1) CO(1)=QIN(LTIQ)
C.......................................................................
C     DEBUG OUTPUT - PRINT QOUT(), AND CO()
      ITIQ=(IDA-IDADAT)*24/IDT+IHR/IDT
      LTIQ=(LDA-IDADAT)*24/IDT+LHR/IDT
      IF(IBUG.EQ.0) GO TO 800
      WRITE(IODBUG,944)
  944 FORMAT(/1X,'RATE OF CHANGE TIME SERIES')
      WRITE(IODBUG,942) (QOUT(I),I=ITIQ,LTIQ)
      WRITE(IODBUG,946)
  946 FORMAT(/1X,'CARRYOVER VALUE')
      WRITE(IODBUG,942) CO(1)
  800 CONTINUE
      IF (ITRACE.GE.1) WRITE(IODBUG,901)
 901  FORMAT(1H0,'** EXIT EX45')
      RETURN
      END
