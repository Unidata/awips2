C MODULE EX7
C-----------------------------------------------------------------------
C
      SUBROUTINE EX7(P,C,QA,QB)
C.......................................................................
C
C     THIS IS THE EXECUTION SUBROUTINE FOR THE LAG/K OPERATION.
C
C     THIS SUBROUTINE PERFORMS THE LAG AND/OR K FUNCTIONS AND
C     RETURNS A TIME SERIES OF OUTFLOW WITH TIME STEP EQUAL TO
C     THE TIME STEP OF THE INFLOW TIME SERIES.  THE INFLOW TIME
C     SERIES IS QA.  THE OUTFLOW TIME SERIES IS THE FIRST PORTION
C     OF QB.  QB IS ALSO USED FOR WORKING SPACE IN THIS OPERATION.
C.......................................................................
C
C     THIS OPERATION ORIGINALLY PROGRAMMED BY
C                    GEORGE F. SMITH - HRL  NOVEMBER, 1979
C          UPDATED MARCH 1982 TO PRINT WITH METRIC OR ENGLISH UNITS
C          UPDATED FEB 1990 TO SET IATL VARIABLE BASED ON FT. WORTH 
C                  TRANS LOSS COMPUTATIONS
C.......................................................................
C
C     VARIABLES IN ARGUMENT LIST
C
C        1. P  - THE P ARRAY
C        2. C  - THE C ARRAY
C        3. QA - THE INFLOW TIME SERIES
C        4. QB - WORK SPACE - THE FIRST NDT LOCATIONS OF THIS SPACE
C                WILL CONTAIN THE OUTFLOW TIME SERIES AT END OF
C                EXECUTION
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
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      COMMON/FCTIME/IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD,
     1  NOW(5),LOCAL,NOUTZ,NOUTDS,NLSTZ,IDA,IHR,LDA,LHR,IDADAT
      COMMON/FCARY/IFILLC,NCSTOR,ICDAY(20),ICHOUR(20)
C
      CHARACTER*8 SNAME/'EX7'/
C
      DIMENSION P(1),C(1),QA(1),QB(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_lagk/RCS/ex7.f,v $
     . $',                                                             '
     .$Id: ex7.f,v 1.2 2000/03/13 20:50:44 page Exp $
     . $' /
C    ===================================================================
C
C
C  PRINT TRACE INFORMATION AND SET DEBUG PRINT FLAG
      LTRACE=1
      NOP=7
      CALL FPRBUG (SNAME,LTRACE,NOP,IBUG)
C
      NP=P(16)
      NC=C(1)
      IF(IBUG.EQ.1)CALL FPRPC7(NP,P,NC,C)
C
C.......................................................................
C
C     FIND BEGINNING OF DATA IN QA AND NUMBER OF PERIODS TO BE RUN.
C.......................................................................
C
      ITA=P(5)
      LDATA=(IDA-IDADAT)*24/ITA + (IHR-1)/ITA + 1
      NDT=(LDA-IDA)*24/ITA + (LHR-IHR)/ITA + 1
      LQB=LDATA
      LCT=LQB+NDT
      LQT=LCT+C(1)
      NC=C(1)
      LEND=LQT-1
      IF(NC.GT.4)LEND=LQT + (C(5)+NDT)*2 + 3
      IF(NC.LE.4)LQT=1
C
C.......................................................................
C
C      CLEAR WORKING SPACE.
C.......................................................................
C
      Z=0.0
      CALL UMEMST (Z,QB,LEND)
C
C.......................................................................
C
C      PARTITION WORKING SPACE AND PASS TO FEXA7.
C.......................................................................
C
      CALL FEXA7(P,C,QA(LDATA),QB(LQB),QB(LCT),QB(LQT),IBUG)
C
      IF(IBUG.EQ.1)WRITE(IODBUG,901)(QB(I),I=1,NDT)
  901 FORMAT(1H0,10X,13HIN EX7 - QB =/(1X,10F12.3))
C
      RETURN
      END
