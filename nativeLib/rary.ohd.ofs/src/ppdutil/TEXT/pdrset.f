C MODULE PDRSET
C-----------------------------------------------------------------------
C
       SUBROUTINE PDRSET (ISET,IRSTYP,ISTAFL)
C
C             ROUTINE:  PDRSET
C             VERSION:  1.0.0
C                DATE:  6-24-83
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE RESETS THE PP24 AND RRS STATISTICS THAT ARE KEPT
C    FOR DATA ON THE PPDB.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C        ISET     A     I     1     'PP24' OR 'RRS' TYPE
C        IRSTYP    A     I     1     AN RRS TYPE OR 'ALL'
C        ISTAFL    I     I     1     STATION FLAG
C                                      0=8-CHAR ID
C                                      1=STATION NUMBER
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'pdbcommon/pdrrsc'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdbdta'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      PARAMETER (LSIBUF=128)
C
      DIMENSION IDRAY(2,40),IRRBUF(1000)
      INTEGER*2 ISIBUF(LSIBUF)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil/RCS/pdrset.f,v $
     . $',                                                             '
     .$Id: pdrset.f,v 1.3 1998/04/07 14:37:00 page Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA LALL/4HALL /,IRRS/4HRRS /
      DATA VMISS/-999.0/
C
C***********************************************************************
C
C  DEBUG
      IF (IPDTR.GT.0) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER PDRSET')
C
C  SET PROGRAM VARIABLES
      LEN=128
      IREC=INFREC+1
C
C  GET CURRENT JULIAN DAY
      CALL HSYSDA (JULDAY)
      JULHR=(JULDAY-1)*24+NHOPDB
      IF (IPDDB.GT.0) WRITE (IOGDB,20) JULDAY,JULHR
20    FORMAT (' HSYSDA CALLED - JULDAY=',I6,3X,'JULHR=',I6)
C
C  READ STATION ID CARD
30    CALL RIDCRD (ISTAFL,IDRAY,NUMID,LASTCD,ISTAT)
      IF (ISTAT.NE.0) GO TO 240
C
C  SEE IF ALL STATIONS ARE TO BE RESET
      I=NUMID
      IF (IDRAY(1,1).EQ.LALL) GO TO 60
C
40    IF (ISTAFL.EQ.1) GO TO 50
C
C  FIND SIF RECORD FOR STATION ID
      CALL PDFNDR (IDRAY(1,I),LEN,IFIND,IREC,ISIBUF,IFREE,ISTAT)
      IF (ISTAT.NE.0) GO TO 240
      IF (IFIND.EQ.0) GO TO 170
      GO TO 70
C
C  FIND SIF FOR STATION NUMBER
50    CALL PDFNDI (IDRAY(1,I),LEN,IFIND,IREC,ISIBUF,IFREE,ISTAT)
      IF (ISTAT.NE.0) GO TO 240
      IF (IFIND.EQ.0) GO TO 170
      GO TO 70
C
C  READ A SIF RECORD TO RESET ALL STATIONS
60    CALL PDRSIF (IREC,NXREC,LSIBUF,ISIBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 240
C
C  SEE IF PP24 OR RRS
70    IF (ISET.EQ.IRRS) GO TO 100
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF HAS PP24 DATA
      IF (ISIBUF(8).EQ.0) GO TO 190
C
C  CALCULATE WHERE STATISTICS BEGIN IN FILE
      IST=ISIBUF(10)*3+10
C
C  RESET PP24 STATISTICS
      CALL UMEMS2 (0,ISIBUF(IST+1),1,NDSTAT)
      CALL UMEMOV (JULDAY,ISIBUF(IST+1),1)
      IF (IPDDB.GT.0) WRITE (IOGDB,80) NDSTAT
80    FORMAT (' NDSTAT=',I2)
C
C  UPDATE SIF RECORD
      LRCP=LRCPDI*2
      N=ISIBUF(1)
      NUMRC=IUNRCD(N,LRCP)
      CALL WVLRCD (KPDSIF,IREC,NUMRC,ISIBUF,LRCPDI,ISTAT)
      IF (ISTAT.NE.0) GO TO 240
      LS=IST+1
      LSS=LS+NDSTAT
      IF (IPDDB.GT.0) WRITE (IOGDB,90) (ISIBUF(K),K=1,IST),
     *  (ISIBUF(KK),KK=LS,LSS)
90    FORMAT (' PP24 STATS RESET=',I4,4A2,5I4,4(2A2,I4)/16I4)
      GO TO 210
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  RESET RRS STATISTICS
100   LL=LHDRRS+1
      L=LHDRRS+12
C
C  SET UP WORK BUFFER
      CALL RPDLRS (LWNEED,ISTAT)
      IF (ISTAT.NE.0) GO TO 240
      MAXBUF=LWNEED
C
C  FIND RRS TYPE IN RECORD
      NUM=ISIBUF(10)
      J=11
      DO 150 K=1,NUM
         CALL UMEMOV (ISIBUF(J),IDTYP,1)
         IF (IRSTYP.NE.LALL) GO TO 110
            IX=IPDCKR(IDTYP)
            IF (IX.NE.0) GO TO 120
            GO TO 140
110      IF (IRSTYP.NE.IDTYP) GO TO 140
C     SET RECORD NUMBER OF RRS DATA
120      IRREC=ISIBUF(J+2)
C     READ THE RRS RECORD
         CALL PDRRRR (IRREC,LRCPDR,MAXBUF,IRRBUF,ISTAT)
         IF (ISTAT.NE.0) GO TO 240
C     RESET RRS STATISTICS
         CALL USWITC (VMISS,IMISS)
         IRRBUF(LHDRRS+1)=JULHR
         IRRBUF(LHDRRS+2)=0
         IRRBUF(LHDRRS+3)=0
         IRRBUF(LHDRRS+4)=IMISS
         IRRBUF(LHDRRS+5)=0
         IRRBUF(LHDRRS+6)=IMISS
         IRRBUF(LHDRRS+7)=0
         IRRBUF(LHDRRS+8)=IMISS
         IRRBUF(LHDRRS+9)=0
         IRRBUF(LHDRRS+10)=IMISS
         IRRBUF(LHDRRS+11)=0
         IRRBUF(LHDRRS+12)=0
C     UPDATE RRS RECORD
         CALL PDWRRR (IRREC,LRCPDR,IRRBUF,ISTAT)
         IF (ISTAT.NE.0) GO TO 240
         IF (IPDDB.GT.0) WRITE (IOGDB,130) (IRRBUF(IP),IP=LL,L)
130   FORMAT (' RRS STATS RESET=',3I3,1X,4(F6.1,I3,1X),I3)
         IF (IRSTYP.NE.LALL) GO TO 210
140      J=J+3
150      CONTINUE
C
C  CHECK IF ALL RECORDS TO BE RESET
      IF (IRSTYP.EQ.LALL) GO TO 210
      WRITE (LP,160) IDRAY(1,I),IDRAY(2,I)
160   FORMAT ('0**WARNING** RRS TYPES DEFINED FOR STATION ',2A4,'.')
      GO TO 210
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  STATION NOT DEFINED
170   WRITE (LP,180)
180   FORMAT ('0**ERROR** STATION NOT DEFINED.')
      GO TO 210
C
190   IF (IDRAY(1,1).EQ.LALL) GO TO 210
      WRITE (LP,200)
200   FORMAT ('0**ERROR** STATION EXISTS BUT DOES NOT HAVE PP24 DATA.')
C
C  CHECK IF PROCESSING ALL STATIONS
210   IF (IDRAY(1,1).NE.LALL) GO TO 220
      IF (NXREC.GT.LSTSIF) GO TO 260
      IREC=NXREC
      GO TO 60
C
C  CHECK FOR NEXT STATION
220   I=I-1
      IF (I.GT.0) GO TO 40
C
C  SEE IF MORE ID CARDS TO READ
C
      IF (LASTCD.EQ.0) GO TO 260
      GO TO 30
C
C  SYSTEM ERROR
240   WRITE (LP,250) ISTAT
250   FORMAT ('0**ERROR** IN PDRSET - STATUS=',I2)
      GO TO 300
C
260   IF (ISET.EQ.IRRS) GO TO 280
C
      WRITE (LP,270)
270   FORMAT ('0**NOTE** PP24 STATISTICS SUCCESSFULLY RESET.')
      GO TO 300
280   WRITE (LP,290)
290   FORMAT ('0**NOTE** RRS STATISTICS SUCCESSFULLY RESET.')
C
300   IF (IPDTR.GT.0) WRITE (IOGDB,310)
310   FORMAT (' *** EXIT PDRSET')
C
      RETURN
C
      END
