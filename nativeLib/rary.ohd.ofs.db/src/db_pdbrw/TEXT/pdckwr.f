C MEMBER PDCKWR
C  (from old member PDWPDRRS)
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE PDCKWR (MIN,LMIN,IMPOS,IMIN,IRRBUF,IFUT,IREV,IFHOUR,
     *   ITIME,VAL,ISTAT)
C
C          ROUTINE:  PDCKWR
C
C         VERSION: 1.0.1  JF. CHANGE TO WRITE DATA IF REVISION FLAG IS
C                  0 OR 1.  7-20-84
C
C             VERSION:  1.0.0
C
C                DATE:  12-15-83
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE CHECKS FOR VALID PARAMETERS FOR A CALL TO THE
C    RRS WRITE ROUTINE.  IT CHECKS THE MINUTES AND UPDATES THE POINTER *
C    TO THE MINUTES ARRAY, THE RANGE OF THE VALUE, WRITING FUTURE
C    DATA IN RANGE OF OBSERVED, REVISING FUTURE WITH OVSERVED, SETS
C    THE FIRST HOUR OF WRITE AND THE LAST OBSERVED TIME.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       MIN        I     I   LMIN   ARRAY CONTAINING MINUTES
C       LMIN       I     I     1    LENGTH OF MIN
C       IMPOS      I    I/O    1    POSITION IN MIN
C       IMIN       I     O     1    MINUTES TO USE FOR VALUE
C       IRRBUF     I    I/O    ?    RRS RECORD
C       IFUT       I     I     1    FUTURE WRITE FLAG
C       IREV       I     I     1    REVISION WRITE FLAG
C       IFHOUR     I    I/O    1    FIRST HOUR IN WRITE
C       ITIME      I     I     1    TIME OF OBSERVATION
C       VAL        R     I     1    VALUE TO WRITE
C       ISTAT      I     O     1    STATUS INDICATOR
C                                     0=NORMAL RETURN
C                                     2=DATA TYPE NOT FOUND
C                                     5=FUTURE BEFORE OBSERVED
C                                     6=BAD REVISION FLAG
C                                     7=BAD MINUTES
C                                     8=BAD VALUE
C                                     9=OUT OF MINUTES
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'pdbcommon/pdtrrx'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER IRRBUF(*)
      INTEGER MIN(LMIN)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdckwr.f,v $
     . $',                                                             '
     .$Id: pdckwr.f,v 1.1 1995/09/17 18:43:33 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA CNOVAL/-999.00/
C
C***********************************************************************
C
C
      IF (IPDTR.GT.1) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER PDCKWR')
C
      ISTAT=0
C
C  CHECK MINUTES
      ITIM=IABS(ITIME)
      IF (IRRBUF(14).EQ.3) GO TO 40
      IMPOS=IMPOS+1
      IF (IMPOS.LE.LMIN) GO TO 20
         ISTAT=9
         GO TO 120
20    IMIN=MIN(IMPOS)
      IF (IPDDB.GT.1) WRITE (IOGDB,30) IMIN
30    FORMAT (' USING MINUTE OF ',I3)
      IF (IMIN.GE.0.AND.IMIN.LT.60) GO TO 40
         ISTAT=7
         GO TO 120
C
C  CHECK RANGE OF VALUE
40    IF (ITIME.LT.0) GO TO 90
      IX=IPDCKR(IRRBUF(5))
      IF (IX.NE.0) GO TO 50
         ISTAT=2
         GO TO 120
C
C  CHECK FOR ALL MISSING
50    IF (VAL.EQ.CNOVAL) GO TO 90
      IF (CHKMIN(IX).NE.CNOVAL.AND.VAL.LT.CHKMIN(IX)) GO TO 60
      IF (CHKMAX(IX).EQ.CNOVAL.OR.VAL.LE.CHKMAX(IX)) GO TO 90
C
60    CALL JLMDYH (ITIME,IMO,IDAY,IYR,IHR)
      WRITE (LPE,70) IRRBUF(2),IRRBUF(3),IRRBUF(5),IMO,IDAY,IYR,IHR,
     *   IMIN,TIME(3),VAL
70    FORMAT ('0',4X,'*** STATION: ',2A4,' TYPE: ',A4,' DATE: ',
     *   2(I2.2,'/'),I2.2,'-',I2.2,':',I2.2,1X,A3 /
     *   5X,'*** VALUE: ',F15.3)
      WRITE (LPE,80)
80    FORMAT (' **ERROR** INVALID VALUE FOR RRS DATA.')
      IF (LPE.EQ.LP) CALL ULINE (LPE,5)
      ISTAT=8
      GO TO 120
C
C  SEE IF WRITING FUTURE IN OBSERVED
90    IF (IFUT.EQ.0) GO TO 100
      IF (IRRBUF(16).EQ.0) GO TO 100
      IF (ITIM.GT.IRRBUF(16)) GO TO 100
         ISTAT=5
         GO TO 120
C
C  CHECK REVISING FUTURE WITH OBSERVED
C170  IF (IREV.EQ.0) GO TO 200
C   IF (ITIM.LE.IRRBUF(16)) GO TO 200
C   ISTAT=6
C   GO TO 999
C
C  SET FIRST HOUR OF WRITE
100   IF (ITIME.LT.0) GO TO 110
      IF (IFHOUR.EQ.0) IFHOUR=ITIME
      IF (IFHOUR.GT.ITIME) IFHOUR=ITIME
C
C  SET LAST OBSERVED HOUR
110   IF (IFUT.EQ.1) GO TO 120
      IF (ITIME.LT.0) GO TO 120
      IF (ITIME.GT.IRRBUF(16)) IRRBUF(16)=ITIME
C
120   IF (IPDTR.GT.1) WRITE (IOGDB,130)
130   FORMAT (' *** EXIT PDCKWR')
C
      RETURN
C
      END
