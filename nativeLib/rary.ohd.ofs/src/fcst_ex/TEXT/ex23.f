C MODULE EX23
C
C
C  DESC -- EXECUTION SUBROUTINE FOR THE STAGE-Q OPERATION
C
C
C.....................................................................
C
      SUBROUTINE EX23(PO,CO,QDATA,HDATA,LOCPTR,T1)
C
C.....................................................................
C
C  THIS IS THE EXECUTION SUBROUTINE FOR THE STAGE-Q OPERATION.  IT
C  DEFINES THIS INPUT ARGUMENTS FOR SUBROUTINE FSTGQ WHICH MAKES THE
C  CONVERSIONS AND RETURNS THE CONVERTED VALUES.  CARRYOVER VALUES FOR
C  SPECIFIC DATES ARE THEN SAVED AS REQUIRED.
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
C
C    QDATA  - THE DISCHARGE TIME SERIES ARRAY
C    HDATA  - THE STAGE TIME SERIES ARRAY
C    LOCPTR - A POINTER ARRAY FOR LOOP RATING SUBROUTINE
C    T1     - A TIMING ARRAY FOR LOOP RATING SUBROUTINE
C
C......................................................................
C
C  SUBROUTINE ORIGINALLY WRITTEN BY --
C      JONATHAN WETMORE - HRL - 810415
C
C     Modified by Scott Townsend RTi July 2003
C         Added code to fix a bug which kept an esp verification run
C         from completing since expected carryover values were not
C         written to the carryover file which was needed by the ESP
C         historic traces run. Bundled checks from fdriv2 into this
C         code since the checks in fdriv2 kept this subroutine from 
C         running and writing carryover.
C***********************************************************************
C
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/where'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fcary'
      INCLUDE 'common/fratng'
C RTi July 2003
C Add these common blocks for condition checks to ensure that 
C StageQ Conversion is not happening even though carryover is
C being written when the operation is requested to be skipped.
      INCLUDE 'common/fprog'
      INCLUDE 'common/esprun'

      COMMON /TSID/TSID(2)
C
      DIMENSION PO(*),CO(1),HDATA(1),QDATA(1),LOCPTR(1),T1(1)
      DIMENSION RCID(2),CARRYO(4),COLD(4)
C
      DIMENSION SUBNAM(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ex/RCS/ex23.f,v $
     . $',                                                             '
     .$Id: ex23.f,v 1.3 2003/08/12 13:05:48 hank Exp $
     . $' /
C    ===================================================================
C
      DATA SUBNAM/4HEX23,4H    /
      DATA LTRACE,NOP/1,23/
C
      CALL FPRBUG(SUBNAM,LTRACE,NOP,IBUG)
C
      IF(IBUG.LT.1) GO TO 5
      WRITE(IODBUG,90) (PO(I),I=1,6)
      WRITE(IODBUG,91) (PO(I),I=7,11)
      WRITE(IODBUG,92) (PO(I),I=12,18)
      WRITE(IODBUG,93) (CO(I),I=1,4)
 90   FORMAT(1H0,2X,18HPO ARRAY CONTENTS: // 2X,6HPO(1)=,F4.2,3X,07HPO(2
     1)= ,A4,3X,06HPO(3)=,A4,3X,07HPO(4)= ,A4,3X,07HPO(5)= ,A4,3X,07HPO(
     .6)= ,A4)
 91   FORMAT(01H0,2X,07HPO(7)= ,F4.2,3X,07HPO(8)= ,A4,3X,07HPO(9)= ,A4,3
     1X,  08HPO(10)= ,A4,3X,08HPO(11)= ,F5.2)
 92   FORMAT(01H0,2X,07HPO(12)=,A4,2X,07HPO(13)=,A4,2X,07HPO(14)=,A4,
     13X, 07HPO(15)=,F5.2,3X,07HPO(16)=,A4,3X,07HPO(17)=,A4,08H PO(18)=,
     1F5.2)
 93   FORMAT(01H0,2X,19H CO ARRAY CONTENTS: // 2X,07HCO(1)= ,F10.2,3X,
     1   07HCO(2)= ,F10.0,3X,07HCO(3)= ,F10.2,3X,07HCO(4)= ,F10.2)
C
C    PROPER RATING CURVE GUARANTEED TO BE IN /FRATNG/ CB
C    /FRATNG/ FILLED IN FDRIV2 JUST BEFORE CALL TO EX23
C
  5   CONTINUE

CRTi July 2003
C A check is done here to see first if this run is an esp run
C then a check is done to see if the segment definition has
C this operation turned off. If so the main code is skipped
C and just carryover is written to the esp carryover file.
C The code checks NCSTOR to determine whether or not to
C skip code. Also blank carry over is put into the array
C that will be used to write to the carryover file.
      IF (MAINUM.EQ.2) THEN
        IF(NCSTOR.GE.100) THEN
          NCSTOR=NCSTOR-100
          DO 6 I=1,4
            COLD(I)=0.0
  6       CONTINUE
          GO TO 35
        ENDIF
      ENDIF

      RCID(1)=PO(16)
      RCID(2)=PO(17)
C
C  SET ITSPOS
C
      INTVAL=PO(11)
      ITSPOS=(IDA-IDADAT)*24/INTVAL+IHR/INTVAL
      LASPOS=(LDA-IDADAT)*24/INTVAL+LHR/INTVAL
      NVALS=(LASPOS-ITSPOS)+1
C
C  SET ARGUMENT LIST FOR FSTGQ SUBROUTINE
C
      TSDELT=PO(11)
      ICONV=PO(7)
      JULDAY=IDA
      INITHR=IHR
      CURVLO=-999.
      CURVUP=-999.
      XSECLO=-999.
      XSECUP=-999.
      METHOD=-999
      FLSTAG=-999.
      NEEDEX=-999
      IRCHNG=-999
      IPRWRN=1
C
      DO 10 I=1,4
      CARRYO(I)=CO(I)
      COLD(I)=CO(I)
 10   CONTINUE
      IF(ICONV.EQ.1) THEN
        TSID(1)=PO(8)
        TSID(2)=PO(9)
      END IF
      IF(ICONV.EQ.2) THEN
        TSID(1)=PO(12)
        TSID(2)=PO(13)
      END IF
C
C  PRINT DBUG INFO
C
      IF(IBUG.LT.1) GO TO 20
      WRITE(IODBUG,602)RCID,ICONV,ITSPOS,NVALS,TSDELT,CURVLO,CURVUP,
     1XSECLO,XSECUP,METHOD,FLSTAG,NEEDEX,JULDAY,INITHR,IRCHNG,
     1IERROR,IPRWRN,CARRYO
C
 602  FORMAT(///1X, 47H*** EX23 DBUG - ABOUT TO CALL FSTGQ...ARGUMENT ,
     1 19HLIST IS AS FOLLOWS: /1X,06HRCID= ,2A4,1X,06HICONV=,I2,09H ITSP
     1OS= ,I8,08H NVALS= ,I8 /1X,08HTSDELT= ,F8.2,09H CURVLO= ,F6.1,09H
     1CURVUP= ,F6.1,09H XSECLO= ,F6.1,09H XSECUP= ,F6.1 /1X,08HMETHOD= ,
     1I5,         09H FLSTAG= ,F6.1,09H NEEDEX= ,I6 /1X,08HJULDAY= ,I10,
     109H INITHR= ,I4,09H IRCHNG= ,I6,09H IERROR= ,I5 ,9H IPRWRN= ,I5,
     ./2X, 17HCARRYOVER VALUES: /2X,4F10.2)
C
C
 20   CALL FSTGQ(RCID,ICONV,ITSPOS,NVALS,TSDELT,QDATA,HDATA,LOCPTR,T1,
     1CURVLO,CURVUP,XSECLO,XSECUP,METHOD,FLSTAG,NEEDEX,CARRYO,JULDAY,
     1INITHR,IRCHNG,IERROR,IPRWRN)
C
C  POST CONVERSION DEBUG
C
      IF(IBUG.LT.1) GO TO 25
      WRITE(IODBUG,603)RCID,ICONV,ITSPOS,NVALS,TSDELT,CURVLO,CURVUP,
     1XSECLO,XSECUP,METHOD,FLSTAG,NEEDEX,JULDAY,INITHR,IRCHNG,
     1IERROR,IPRWRN,CARRYO
C
 603  FORMAT(///1X, 47H*** EX23 DBUG - RETURNED FROM FSTGQ...ARGUMENT ,
     1 19HLIST IS AS FOLLOWS: /1X,06HRCID= ,2A4,1X,06HICONV=,I2,09H ITSP
     1OS= ,I8,07HNVALS= ,I8 /1X,08HTSDELT= ,F8.2,09H CURVLO= ,F9.2,09H C
     1URVUP= , F9.2,09H XSECLO= ,F9.2,09H XSECUP= ,F9.2 /1X,08HMETHOD= ,
     1I5,         09H FLSTAG= ,F9.2,09H NEEDEX= ,I6 /1X,08HJULDAY= ,I10,
     109H INITHR= ,I4,09H IRCHNG= ,I6,09H IERROR= ,I5 ,9H IPRWRN= ,I5,
     ./2X, 17HCARRYOVER VALUES: /2X,4F10.2)
C
C  SAVE CARRYOVER AS REQUIRED
C
 25   IF(IFILLC.LT.1) GO TO 999
C  UPDATE CO ARRAY FOR LDA,LHR
      DO 30  I=1,4
 30   CO(I)=CARRYO(I)
C
C  SAVE CO FOR SPECIFIC DATE(S)?
 35   IF(NCSTOR.LT.1) GO TO 999

cew  Must check to be sure you are in the correct month saving carryover.
cew  Only required in ESP because you are always in the right month
cew  in FCEXEC.
cew  Don't save carryover unless the carryover dates are between 
cew  ida and lda.
cew  At this time all carryover dates are in the same month, so need
cew  to check only the first date.  If the first date is in the 
cew  correct period, then all dates should be in the correct period.

      if(icday(1) .lt. ida .or. icday(1) .gt. lda) go to 999
C
      CALL FSTQCO(HDATA,QDATA,ICONV,ITSPOS,IDADAT,INTVAL,COLD,IBUG)
C
C***********************************************************************
C
 999  CONTINUE
      IF(ITRACE.GE.1) WRITE (IODBUG,608)
 608  FORMAT(1H0,2X,18H *** EXIT EX23 ***)
C
      RETURN
      END
