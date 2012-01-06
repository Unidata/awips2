C MEMBER PDCMND
C  (from old member PDCHSPEC)
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
       SUBROUTINE PDCMND (ISTAFL,ISTAT)
C
C          ROUTINE:  PDCMND
C
C             VERSION:  1.0.0
C
C                DATE:  6-30-83
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C     THIS ROUTINE CHANGES THE MINIMUM NUMBER OF DAYS FOR
C     DATA RETENTION FOR A RRS STATION AND DATA TYPE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C      ISTAFL       I      I     1    STATION FLAG
C                                       0=8-CHAR ID
C                                       1=STATION NUMBER
C      ISTAT        I      O     1    STATUS CODE
C                                       0=NORMAL RETURN
C                                       OTHER=ERROR
C
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
      INCLUDE 'udatas'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdrrsc'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdunts'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER*2 ISIBUF(128)
C
      DIMENSION IDSTA(2),IRRBUF(1000)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil/RCS/pdcmnd.f,v $
     . $',                                                             '
     .$Id: pdcmnd.f,v 1.1 1995/09/17 19:09:18 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
C
C  DEBUG
C
      IF (IPDTR.EQ.1) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER PDCMND')
C
      ISTAT=0
      LEN=128
C
C  GET STATION
C
      IF=2
      NUM=IFSTOP(IF)-IFSTRT(IF)+1
      IF (NUM.GT.8) NUM=8
      IF (IFTYPE(IF).EQ.1) GO TO 20
      CALL UPACK1 (IBUF(IFSTRT(IF)),IDSTA,NUM)
      GO TO 30
20    CALL UNUMIC (IBUF,IFSTRT(IF),IFSTOP(IF),IDSTA)
30    CONTINUE
C
C  GET RRS DATA TYPE
C
      IF=IF+1
      NUM=IFSTOP(IF)-IFSTRT(IF)+1
      IF (NUM.GT.4) NUM=4
      CALL UPACK1 (IBUF(IFSTRT(IF)),IRSTYP,NUM)
C
C  SEE IF LEGAL TYPE
C
      IRX=IPDCKR(IRSTYP)
      IF (IRX.NE.0) GO TO 50
      WRITE (LPE,40) IRSTYP
40    FORMAT (' **ERROR** INVALID RRS TYPE ',A4)
      GO TO 230
50    CONTINUE
C
C  GET THE NEW MIN # OF DAYS FOR STATION & TYPE
C
      IF=IF+1
      IF (IFTYPE(IF).EQ.1) GO TO 70
      WRITE (LPE,60) IF
60    FORMAT (' **ERROR** FIELD ',I2,' IS NOT AN INTEGER')
      GO TO 230
70    CALL UNUMIC (IBUF,IFSTRT(IF),IFSTOP(IF),MNDY)
80    CONTINUE
C
C  READ SIF RECORD FOR STATION
C
      IF (ISTAFL.EQ.1) GO TO 90
      CALL PDFNDR (IDSTA,LEN,IFIND,ISIREC,ISIBUF,IFREE,ISTAT)
      IF (ISTAT.NE.0) GO TO 210
      GO TO 100
90    CALL PDFNDI (IDSTA,LEN,IFIND,ISIREC,ISIBUF,IFREE,ISTAT)
      IF (ISTAT.NE.0) GO TO 210
100   CONTINUE
C
C  LOOK FOR RRS TYPE
C
      IF (IFIND.EQ.0) GO TO 210
      J=11
      NT=ISIBUF(10)
      DO 110 I=1,NT
      CALL UMEMOV (ISIBUF(J),IRRS,1)
      IF (IRRS.EQ.IRSTYP) GO TO 130
      J=J+3
110   CONTINUE
      WRITE (LPE,120) IRSTYP
120   FORMAT (' **ERROR** RRS TYPE ',A4,' NOT FOUND IN THIS ',
     *      'STATION.')
      GO TO 230
130   CONTINUE
C
C  FIND RRS TYPE IN DIRECTORY
C
      IRX=IPDCKR(IRSTYP)
      IF (IRX.NE.0) GO TO 150
      WRITE (LPE,140) IRSTYP
140   FORMAT (' **ERROR** INVALID DATA TYPE ',A4,' NOT FOUND ',
     *       'ON PPDB.')
      GO TO 230
150   CONTINUE
C
C  READ THE RRS RECORD
C
      NREC=ISIBUF(J+2)
      CALL RPDLRS (LWNEED,ISTAT)
      IF (ISTAT.NE.0) GO TO 210
      LRRBUF=LWNEED
      CALL PDRRRR (NREC,LRCPDR,LRRBUF,IRRBUF,ISTAT)
      IF (IPDDB.EQ.1) WRITE (IOGDB,160) (IRRBUF(I),I=1,8)
160   FORMAT (' IRRBUF=',I4,1X,2A4,1X,I6,1X,A4,1X,3I4)
      IF (ISTAT.NE.0) GO TO 210
      IF (IRRBUF(5).EQ.IRSTYP) GO TO 170
      WRITE (LPE,120) IRSTYP
      GO TO 230
170   CONTINUE
C
C  CHANGE MIN # OF DAYS FOR DATA RETENTION
C
      IRRBUF(6)=MNDY
C
C  RE-WRITE RRS RECORD BACK TO FILE
C
      CALL PDWRRR (NREC,LRCPDR,IRRBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 210
180   CONTINUE
C
C  DEBUG AND RETURN
C
      IF (IPDTR.EQ.1.OR.IPDDB.EQ.1) WRITE (IOGDB,190) (IRRBUF(I),I=2,8)
190   FORMAT (' *** EXIT PDCMND : STAID=',2A4,2X,
     *       'STA#=',I6,'  TYPE=',A4,'  MINDAY=',I2,2X,
     *       'MAXOBS=',I2,'  NUMOBS=',I2)
      WRITE (LP,200) IRSTYP,IRRBUF(2),IRRBUF(3)
200   FORMAT (' MINIMUM DAYS RESET FOR RRS TYPE ',A4,' IN STATION ',2A4)
      GO TO 230
210   CONTINUE
C
C  SYSTEM ERROR
C
      WRITE (LPE,220) ISTAT
220   FORMAT (' **ERROR** IN PDCMND - STATUS =',I2)
C
230   RETURN
C
      END
