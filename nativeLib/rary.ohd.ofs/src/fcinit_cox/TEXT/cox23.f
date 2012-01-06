C MEMBER COX23
C  (from old member FCCOX23)
C
C  DESC -- CARRYOVER TRANSFER ROUTINE FOR STAGE-Q OPERATION
C
C.......................................................................
C
      SUBROUTINE COX23(POLD,COLD,PONEW,CONEW)
C
C....................................................................
C
C  THIS IS THE CARRYOVER TRANSFER ROUTINE FOR THE STAGE-Q OPERATION
C
C    ARGUMENT LIST
C    POLD   - THE ORIGINAL PARAMETER ARRAY
C             CONTENTS:
C               POSITION     DESCRIPTION
C                  1         VERSION NUMBER
C                 2-6        20-CHAR NAME OF GAGING STATION OR FORECAST
C                             POINT
C                  7         CONVERSION INDICATOR
C                            =1, STAGE TO DISCHARGE
C                            =2, DISCHARGE TO STAGE
C                            (DEFAULT IS 1)
C                 8-9        8-CHAR STAGE T.S. IDENTIFIER
C                  10        4-CHAR STAGE T.S. DATA TYPE CODE
C                  11        TIME INTERVAL IN HOURS FOR STAGE T.S.
C                12-13       8-CHAR DISCHARGE T.S. IDENTIFIER
C                  14        4-CHAR DISCHARGE T.S. DATA TYPE CODE
C                  15        TIME INTERVAL IN HOURS FOR DISCHARGE T.S.
C                16-17       8-CHAR RATING CURVE IDENTIFIER
C                  18        DEFAULT CARRYOVER INDICATOR
C                            =0, DEFAULT CARRYOVER (ALL ZEROES) USED
C                            =1, INITIAL CARRYOVER READ IN
C
C    COLD   - THE ORIGINAL CARRYOVER ARRAY
C             CONTENTS:
C
C               POSITION      DESCRIPTION
C                  1          PREVIOUS STAGE VALUE
C                  2          PREVIOUS DISCHARGE VALUE
C                  3          RATE OF CHANGE IN DISCHARGE (ICNVRT=1)
C                             RATE OF CHANGE IN STAGE  (ICNVRT=2)
C                  4          NO. OF MISSING VALUES IMMEDIATLY PRECEDING
C                             FIRST VALUE OF T.S. TO CONVERT
C
C    PONEW  - THE NEW PARAMETER ARRAY
C    CONEW  - THE NEW CARRYOVER ARRAY
C
C......................................................................
C
C  SUBROUTINE ORIGINALLY WRITTEN BY --
C      JONATHAN WETMORE - HRL - 810415
C
C***********************************************************************
C
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
C
      DIMENSION POLD(1),COLD(1),PONEW(1),CONEW(1)
      DIMENSION SUBNAM(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cox23.f,v $
     . $',                                                             '
     .$Id: cox23.f,v 1.1 1995/09/17 18:47:14 dws Exp $
     . $' /
C    ===================================================================
C
      DATA SUBNAM/4HCOX2,4H3   /
      DATA LTRACE,NOP/1,23/
C
C
      CALL FPRBUG(SUBNAM,LTRACE,NOP,IBUG)
C
      IF(IBUG.LT.1) GO TO 5
      WRITE(IODBUG,93) (COLD(I),I=1,4)
 93   FORMAT(1H0,2X,22HOLD CO ARRAY CONTENTS:,//,2X,07HCO(1)= ,F10.2,3X,
     1   07HCO(2)= ,F10.0,3X,07HCO(3)= ,F10.2,3X,07HCO(4)= ,F10.2)
C
 5    CONTINUE
C
      DO 10 I=1,4
 10   CONEW(I)=COLD(I)
C
C  SEE IF CONVERSION FACTOR IS THE SAME
      IF(PONEW(7).EQ.POLD(7)) GO TO 20
C  MUST ZERO RATE OF CHANGE
      CONEW(3)=0.
C
  20  CONTINUE
C
C  SEE IF DELTA T IS SAME FOR BOTH
      IF(PONEW(11).EQ.POLD(11)) GO TO 30
C  MUST ADJUST RATE OF CHANGE AND NO. MISSING
      CONEW(3)=CONEW(3)*(PONEW(11)-0.01)/(POLD(11)-0.01)
      CONEW(4)=CONEW(4)*(POLD(11)-0.01)/(PONEW(11)-0.01)
C
  30  CONTINUE
      IF(IBUG.LT.1) GO TO 999
      WRITE(IODBUG,94) (CONEW(I),I=1,4)
 94   FORMAT(1H0,2X,22HNEW CO ARRAY CONTENTS:,//,2X,07HCO(1)= ,F10.2,3X,
     1   07HCO(2)= ,F10.0,3X,07HCO(3)= ,F10.2,3X,07HCO(4)= ,F10.2)
C
C***********************************************************************
C
 999  CONTINUE
      IF(ITRACE.GE.1) WRITE (IODBUG,608)
 608  FORMAT(1H0,2X,18H *** EXIT COX23 **)
C
      RETURN
      END
