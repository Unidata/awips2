C MEMBER PRC23
C  (from old member FCPRC23)
C
C  DESC -- PRINT CARRYOVER ROUTINE FOR STAGE-Q OPERATION
C
C.......................................................................
C
      SUBROUTINE PRC23(PO,CO)
C
C....................................................................
C
C  THIS IS THE PRINT CARRYOVER ROUTINE FOR THE STAGE-Q OPERATION
C
C    ARGUMENT LIST
C    PO     - THE PARAMETER ARRAY
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
C    CO     - THE CARRYOVER ARRAY
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
      INCLUDE 'common/fconit'
C
      DIMENSION PO(1),CO(1)
      DIMENSION STAGE(2),DISCHG(2)
C
      DIMENSION SUBNAM(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc23.f,v $
     . $',                                                             '
     .$Id: prc23.f,v 1.1 1995/09/17 18:49:40 dws Exp $
     . $' /
C    ===================================================================
C
      DATA SUBNAM/4HPRC2,4H3   /
      DATA STAGE,DISCHG/4HSTAG,4HE   ,4HDISC,4HHARG/
      DATA LTRACE,NOP/1,23/
C
      CALL FPRBUG(SUBNAM,LTRACE,NOP,IBUG)
C
      IF(IBUG.LT.1) GO TO 5
      WRITE(IODBUG,93) (CO(I),I=1,4)
 93   FORMAT(01H0,2X,19H CO ARRAY CONTENTS: // 2X,07HCO(1)= ,F10.2,3X,
     1   07HCO(2)= ,F10.0,3X,07HCO(3)= ,F10.2,3X,07HCO(4)= ,F10.2)
C
 5    CONTINUE
      WRITE (IPR,1)(PO(J),J=2,6)
 1    FORMAT(1H0,12X,32HSTAGE-Q OPERATION CARRYOVER FOR ,5A4)
C
C  CHECK FOR DEFAULTING OF CO VALUES
       ICO=PO(18)
C
      IF(IVALUE.EQ.1.AND.ICO.EQ.0) GO TO 20
C
      ICONV=PO(7)
      IF(ICONV.EQ.1) WRITE(IPR,2)(CO(KK),KK=1,2),DISCHG,(CO(KL),KL=3,4)
      IF(ICONV.EQ.2) WRITE(IPR,2)(CO(KK),KK=1,2),STAGE,(CO(KL),KL=3,4)
  2   FORMAT(1H0,03X,19HPREVIOUS STAGE (M):,F10.2,03X,25HPREVIOUS DISCHA
     1RGE (CMS):,F10.2 /18X,18HRATE OF CHANGE IN ,2A4,1H:,F10.2 /12X,
     144HNUMBER OF MISSING VALUES PRIOR TO RUN START:,F5.0)
C
C
      GO TO 999
C
 20   CONTINUE
      WRITE(IPR,3)
 3    FORMAT(1H0,2X,10(1H*),40H ALL CARRYOVER VALUES DEFAULTED TO ZERO.)
C***********************************************************************
C
 999  CONTINUE
      IF(ITRACE.GE.1) WRITE (IODBUG,608)
 608  FORMAT(1H0,2X,18H *** EXIT PRC23 **)
C
      RETURN
      END
