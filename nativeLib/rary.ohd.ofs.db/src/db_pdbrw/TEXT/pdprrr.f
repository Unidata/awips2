C MEMBET PDPRRR
C  (from old member PPDBPRRR)
C***********************************************************************
C                                                                      *
C         MEMBER PPDBPRRR                                              *
C                                                                      *
C***********************************************************************
       SUBROUTINE PDPRRR(IRRBUF,NLINES,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDPRRR                                         *
C                                                                      *
C 7-30-84 FIX PRINT FOR EVEN RECORD
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  1-26-83                                        *
C                                                                      *
C              AUTHOR:  JANINE FRANZOI                                 *
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    THIS SUBROUTINE PRINTS AAND FORMATS A REPORT OF THE RIVER,        *
C    RESERVOIR AND SNOW DATA FILES, INCLUDING THE HEADER RECORD        *
C    AND ANY FREEPOOL RECORDS ASSOCIATED WITH THAT REPORTING           *
C    PERIOD.                                                           *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C         IRRBUF   I/A   I     1     CONTAINS RRS RECORD               *
C         NLINES    I     O   1      NUMBER OF LINES PRINTED           *
C         ISTAT    I     O    1      STATUS INDICATOR                  *
C                                      0 = NORMAL RETURN               *
C                                      1 = ERROR                       *
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'udebug'
C                                                                      *
C***********************************************************************
C                                                                      *
C          DIMENSION AND TYPE DECLARATIONS:                            *
C                                                                      *
      DIMENSION NRCBUF(20),IRRBUF(1),IFRBUF(16)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdprrr.f,v $
     . $',                                                             '
     .$Id: pdprrr.f,v 1.1 1995/09/17 18:44:10 dws Exp $
     . $' /
C    ===================================================================
C
C                                                                      *
C***********************************************************************
C
C
      ISTAT = 0
C
C  SET PROGRAM VARIABLES
C
      NLINES = 0
      NPRT = 0
C
C  DEBUG
C
      IF(IPDTR.GT.1) WRITE(IOGDB,100)
  100 FORMAT(' SUBROUTINE PDPRRR ENTERED')
C
C  PRINT HEADERS FOR OUTPUT
C
      WRITE(LP,110) (IRRBUF(I),I=1,9),IRRBUF(11),(IRRBUF(J),
     1                J=13,17)
  110 FORMAT('0# WRDS RRS DATAREC=',I4,2X,'STA ID=',2A4,2X,
     1       'STA # =',I6,2X,'TYPE=',A4,2X,'MINDAY=',I2,2X,
     2       'MAX # OBVS=',I3,2X,'# OBVS REC=',I3/' WRD EVAL=',
     3       I4,2X,'WRD LVAL=',I4,2X,'REC # FPREC=',I4,2X,
     4       '# VAL/OBV=',I2,2X,'FREE T=',I8,2X,'LST OB=',
     5       I8,2X,'# STAT=',I2)
C
C  PRINT DATA STATS FOR THIS STATION
C
      IF(IRRBUF(17).EQ.0) GO TO 125
      NUM = IRRBUF(17) + 17
      WRITE(LP,120)(IRRBUF(I),I=18,NUM)
  120 FORMAT('0STATISTICS START ',I7,' LAST=',I7,' #=',I7,' LV=',F7.2,
     1       1X,I7,' SLV=',F7.2,1X,I7,' SV=',F7.2,1X,I7,' SSV=',F7.2,
     2       1X,I7)
      NL = IRRBUF(17) + 14/15
      NLINES = 5 + NL
  125 CONTINUE
      NLINES = 3
C
C  CHECK FOR FREEPOOL RECORDS
C
      NWPL = 14
      IF(IRRBUF(14).EQ.3) NWPL = 18
      IF(IRRBUF(13).EQ.0) GO TO 240
C
      IPOS = 1
      IREC = IRRBUF(13)
C
C  OBTAIN FREEPOOL RECORD
C
  130 CONTINUE
      CALL PDRDFR(IREC,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      IPOS1 = 3
C
C CALCULATE # OF WORDS IN OBSERVATION SET
C
      NWOBV = IFRBUF(2) * IRRBUF(14)
      NMOV = NWOBV
      IF(NWOBV + IPOS.LE.NWPL + 1) GO TO 140
      NMOV = NWPL - IPOS + 1
  140 CONTINUE
C
C  MOVE INTO PRINT BUFFER AND RESET POINTER
C
      CALL UMEMOV(IFRBUF(IPOS1),NRCBUF(IPOS),NMOV)
      IPOS = IPOS + NMOV
C
C  SEE IF BUFFER IS FULL
C
      IF(IPOS.LE.NWPL) GO TO 225
  150 CONTINUE
C
C  PRINT BUFFER
C
      N = IPOS - 1
      IF(NPRT.EQ.1) GO TO 170
      WRITE(LP,160)
  160 FORMAT('0FREEPOOL RECORD DATA:')
  170 IF(IRRBUF(14).EQ.2) GO TO 190
      WRITE(LP,180) (NRCBUF(I),I=1,N)
  180 FORMAT(' ',6(I8,2X,F7.2,1X,I2))
      GO TO 210
  190 WRITE(LP,200) (NRCBUF(I),I=1,N)
  200 FORMAT(' ',7(I8,2X,F7.2))
  210 CONTINUE
C
C  INCREMENT LINE COUNTER
C
      NLINES = NLINES + 1
      IF(NPRT.EQ.0) NLINES = NLINES + 2
      NPRT = 1
      IPOS = 1
  225 CONTINUE
C
C  SEE IF MORE IN RECORD
C
      IF(NMOV.EQ.NWOBV) GO TO 227
      IPOS1 = IPOS1 + NMOV
      NMOV = NWOBV - NMOV
      NWOBV = NMOV
      GO TO 140
 227  CONTINUE
C
C  CHECK FOR ANOTHER FREEPOOL RECORD
C
      IF(IFRBUF(1).EQ.0) GO TO 230
      IREC = IFRBUF(1)
      GO TO 130
C
C  PRINT THE REMAINING FREEPOOL BUFFER
C
  230 IF(IPOS.GT.1) GO TO 150
  240 CONTINUE
C
C  PROCESS RRS DATA RECORDS
C  RESET POINTERS
C
      IF(IRRBUF(8).EQ.0) GO TO 990
      NNPRT = 0
      IPOS = 1
      IPOS1 = IRRBUF(9)
      NPRT = 0
      IWRAP = 0
      IF(IRRBUF(9).GT.IRRBUF(11)) IWRAP = 1
  245 CONTINUE
C
C  CALCULATE # OF WORDS TO MOVE INTO PRINT BUFFER
C
      IF(IPOS1.LE.IRRBUF(1)) GO TO 247
      IPOS1 = IRRBUF(1) - IRRBUF(7) * IRRBUF(14) + 1
      IWRAP = 0
 247  CONTINUE
      NMOV = NWPL
      IF(IPOS1 + NMOV.LE.IRRBUF(1) + 1) GO TO 250
      NMOV = IRRBUF(1) - IPOS1 + 1
      IWRAP = 0
      GO TO 260
  250 CONTINUE
      IF(IWRAP.EQ.1) GO TO 260
      IF(IPOS1 + NMOV.LE.IRRBUF(11) + IRRBUF(14)) GO TO 260
      NMOV = IRRBUF(11) + IRRBUF(14) - 1 - IPOS1 + 1
  260 CALL UMEMOV(IRRBUF(IPOS1),NRCBUF(IPOS),NMOV)
      IPOS1 = IPOS1 + NMOV
      IPOS = IPOS + NMOV
      IF(IPOS.EQ.NWPL + 1) GO TO 280
      IF(IPOS1.EQ.IRRBUF(11) + IRRBUF(14)) GO TO 280
      IPOS1 = IRRBUF(1) - IRRBUF(7) * IRRBUF(14) + 1
      NMOV = NWPL - NMOV
      GO TO 250
  280 CONTINUE
C
C  PRINT BUFFER
C
      N = IPOS - 1
      IF(NPRT.EQ.1) GO TO 300
      WRITE(LP,290)
  290 FORMAT('0RRS RECORD DATA:')
  300 IF(IRRBUF(14).EQ.2) GO TO 310
      WRITE(LP,180) (NRCBUF(I),I=1,N)
      GO TO 320
  310 WRITE(LP,200) (NRCBUF(I),I=1,N)
  320 CONTINUE
C
C  INCREMENT LINECOUNTER
C
      NLINES = NLINES + 1
      IF(NPRT.EQ.0) NLINES = NLINES + 2
      IPOS = 1
      NPRT = 1
      NNPRT = NNPRT + (N/IRRBUF(14))
      IF(NNPRT .GE. IRRBUF(8)) GO TO 990
C
C  SEE IF ENTIRE BUFFER WAS PRINTED
C
      IF(IPOS1.EQ.IRRBUF(11) + IRRBUF(14)) GO TO 990
      GO TO 245
  990 CONTINUE
C
C  DEBUG AND RETURN
C
      IF(IPDTR.GT.1.OR.IPDDB.GT.1) WRITE(IOGDB,995)
  995 FORMAT(' PDPRRR EXECUTED. PROCESSING COMPLETE')
  999 CONTINUE
      RETURN
      END
