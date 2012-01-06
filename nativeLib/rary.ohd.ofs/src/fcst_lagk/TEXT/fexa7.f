C MODULE FEXA7
C-----------------------------------------------------------------------
C
      SUBROUTINE FEXA7 (P,C,QA,QB,CT,QT,IB)
C.......................................................................
C
C     THIS SUBROUTINE IS CALLED BY EX7 AFTER WORKING SPACE IS
C     PARTITIONED AMONG SEVERAL TIME SERIES.
C     THIS IS THE SUBROUTINE WHICH CONTROLS THE LAG/K LOOPING.
C.......................................................................
C
C     SUBROUTINE ORIGINALLY PROGRAMMED BY
C          GEORGE F. SMITH - HRL   DECEMBER 1979
C.......................................................................
C
C     VARIABLES IN ARGUMENT LIST
C
C        1. P  - THE P ARRAY
C        2. C  - THE C ARRAY
C        3. QA - THE INFLOW TIME SERIES
C        4. QB - THE OUTFLOW TIME SERIES
C        5. CT - WORK SPACE FOR COPY OF THE C ARRAY
C        6. QT - WORK SPACE
C        7. IB - PRINT DEBUG FLAG, PRINT IF IB = 1
C.......................................................................
C
C     THIS SUBROUTINE CAN HANDLE TWO DIFFERENT METHODS OF COMPUTING
C     VARIABLE K.  THESE TWO METHODS ARE:
C      1.  THE METHOD USED IN THE MCP2 PROGRAM AND DESCRIBED IN SECTION
C      II.4.2.1 OF THE NWSRFS USERS MANUAL.
C      2.  THE METHOD USED BY THE ATLANTA RFC.  THIS METHOD SOLVES THE
C      STORAGE EQUATION BY USING THE RELATIONSHIP BETWEEN 2*S/DT+O AND
C      O.  THIS RELATIONSHIP IS CONSTRUCTED FROM THE VARIABLE K CURVE
C      USING THE EQUATION DELTA S = K * DELTA O.
C
C     COMMON BLOCK FATLGK DETERMINES WHICH OF THE TWO METHODS IS USED.
C     IF IATL=0, THE MCP2.0 METHOD IS USED.
C     IF IATL=1, THE ATLANTA RFC METHOD IS USED.
C       IF THE ATLANTA RFC METHOD IS USED TWO ADDITIONAL VALUES ARE
C       REQUIRED TO SPECIFY THE DELTA O INCREMENTS USED IN COMPUTING
C       THE 2*S/DT+O VS O TABLE.  THESE VALUES ARE C1 AND C2.
C
C     IATL,C1,AND C2 ARE SET IN BLOCK DATA ROUTINES FOR
C       MCP 3.0, OPT 3.0, ESP 3.0, AND VERSION 5.0 OF
C       THE NWSRFS OPERATIONAL PROGRAM.
C     IATL IS RESET BASED ON WHETHER OR NOT FT. WORTH
C       TRANSMISSION LOSS COMPUTATIONS ARE DONE FOR
C       THE CURRENT LAG/K OPERATION
C       IATL = 1 IF TRANS LOSS COMP ARE OFF (I.E., P(11).EQ.0.0)
C       IATL = 0 IF TRANS LOSS COMP ARE ON  (I.E., P(11).GT.0.0)
C.......................................................................
C
      INCLUDE 'common/fdbug'
      COMMON/FCTIME/IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD,
     1  NOW(5),LOCAL,NOUTZ,NOUTDS,NLSTZ,IDA,IHR,LDA,LHR,IDADAT
      COMMON/FCARY/IFILLC,NCSTOR,ICDAY(20),ICHOUR(20)
      INCLUDE 'common/ionum'
      COMMON/FATLGK/IATL,C1,C2
C
      DIMENSION P(1),C(1),QA(1),QB(1),CT(1),QT(1)
C
      LOGICAL FOP7
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_lagk/RCS/fexa7.f,v $
     . $',                                                             '
     .$Id: fexa7.f,v 1.3 2003/09/30 11:59:10 edwin Exp $
     . $' /
C    ===================================================================
C
C
      IF(IB.EQ.1)WRITE(IODBUG,*) 'ENTER FEXA7'
C
C.......................................................................
C
C     LOOP THROUGH LAG/K ONCE FOR EACH CARRYOVER TO BE SAVED
C     AND ONCE FOR A FINAL RUN OF WHOLE PERIOD.
C.......................................................................
C
      ITA=P(5)
      NTIMES=1
      IF(IFILLC.GT.0)NTIMES=NCSTOR+1
C
      DO 1000 IIX=1,NTIMES
C.......................................................................
C
C     COPY C ARRAY INTO TEMPORARY SPACE.
C.......................................................................
C
      NC=C(1)
      CALL UMEMOV(C,CT,NC)
C
      IF(IIX.EQ.NTIMES)GO TO 10
      NDT=(ICDAY(IIX)-IDA)*24/ITA + (ICHOUR(IIX)-IHR)/ITA + 1
      COTIME=NDT*ITA
      GO TO 20
C
   10 NDT=(LDA-IDA)*24/ITA + (LHR-IHR)/ITA + 1
      COTIME=NDT*ITA
C
   20 IF(FOP7(P(19),P(20)))GO TO 30
C
C.......................................................................
C
C     IF LAG NOT ENABLED COPY QA INTO QB.
C.......................................................................
C
      CALL UMEMOV(QA,QB,NDT)
C
      GO TO 500
C
C.......................................................................
C
C     FLAG7 PERFORMS LAG OPERATION.
C     RETURNS QB WITH CONSTANT TIME STEP EQUAL TO TIME STEP OF INFLOW.
C     CT HAS NEW LAG CARRYOVER - IF K NOT ENABLED C(3) NEEDS TO BE
C     CHANGED BEFORE SAVING CARRYOVER.
C.......................................................................
C
   30 CALL FLAG7(P,CT,QA,QB,QT,NDT,COTIME,IB)
C
  500 IBK=P(18)
C
      TLRC = P(11)
      IATL = 1
      IF(TLRC .GT. 0.0) IATL = 0
C
C.......................................................................
C
C     IF K ENABLED - CALL FK7 TO PERFORM K OPERATION.
C.......................................................................
C
      IF(FOP7(P(IBK),P(IBK+1)).AND.IATL.EQ.0)
     1  CALL FK7(P,CT,QB,NDT,COTIME,IB)
      IF(FOP7(P(IBK),P(IBK+1)).AND.IATL.EQ.1)
     1  CALL FKA7(P,CT,QB,NDT,COTIME,IB)
C
      IF(.NOT.FOP7(P(IBK),P(IBK+1)).AND.NDT.GT.0)CT(3)=QB(NDT)
C
      IF(IFILLC.EQ.0)RETURN
C
C.......................................................................
C
C     SAVE CARRYOVER ONCE PER LOOP AND ONCE FOR FINAL RUN OF ENTIRE
C     PERIOD.
C     ALWAYS SAVE CARRYOVER FOR FINAL RUN UNLESS IFILLC = 0.
C.......................................................................
C
C Modified by RTi Sep 2003. This check is needed to keep carryover
C from being written when the julian dates do not correspond to
C the requested carryover date.
      if(icday(1) .lt. ida .or. icday(1) .gt. lda) go to 999
        IF(IIX.LT.NTIMES.AND.NCSTOR.GT.0) THEN
          CALL FCWTCO(ICDAY(IIX),ICHOUR(IIX),CT,NC)
        ENDIF
C
 999  CONTINUE
      IF(IIX.EQ.NTIMES)CALL UMEMOV(CT,C,NC)
C
 1000 CONTINUE
C
      IF(IB.EQ.1)WRITE(IODBUG,*) 'EXIT FEXA7'
C
      RETURN
      END
