C MEMBER PDCMXD
C  (from old member PDCHSPEC)
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
       SUBROUTINE PDCMXD (IFLAG,ISTAT)
C
C          ROUTINE:  PDCMXD
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
C    THIS ROUTINE CHANGES THE MAXIMUM NUMBER OF DAYS FOR THE
C    SPECIFIED DATA TYPE ON THE PPDB.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IFLAG      I     O     1     WRITE FLAG
C                                     0=DO NOT REWRITE CONTROLS
C                                     1=RE-WRITE CONTROLS
C       ISTAT      I     O     1     STATUS CODE
C                                     0=NORMAL RETURN
C                                     OTHER=ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
      INCLUDE 'udatas'
      INCLUDE 'pdbcommon/pdddfc'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdbdta'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdunts'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER*2 IBUFF(3000)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil/RCS/pdcmxd.f,v $
     . $',                                                             '
     .$Id: pdcmxd.f,v 1.1 1995/09/17 19:09:19 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C       DATA :
C
      DATA IPP24/4HPP24/,IPPSR/4HPPSR/
C
C******************************************************************
C
      ISTAT=0
      IFLAG=0
      LRCP=LRCPDD*2
C
C  DEBUG
C
      IF (IPDTR.EQ.1) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER PDCMXD ')
C
C READ CARD WITH DAILY DATA TYPE
C
      IF=2
      NUM=IFSTOP(IF)-IFSTRT(IF)+1
      IF (NUM.LE.4) GO TO 30
      WRITE (LPE,20)
20    FORMAT (' **ERROR** INVALID # OF CHARS FOR DAILY DATA TYPE',
     *       ' CARD IGNORED.')
      GO TO 230
30    CALL UPACK1 (IBUF(IFSTRT(IF)),IDTYP,NUM)
C
C  SEE IF VALID TYPE
C
      IDX=IPDCKD(IDTYP)
      IF (IDX.NE.0) GO TO 50
      WRITE (LPE,40) IDTYP
40    FORMAT (' **ERROR** DAILY DATA TYPE ',A4,' INVALID.')
      GO TO 230
50    CONTINUE
C
C  GET FILE AND MAX # OF DAYS
C
      IDF=IDDTDR(4,IDX)
      IF=IF+1
      IF (IFTYPE(IF).EQ.1) GO TO 70
      WRITE (LPE,60) IF
60    FORMAT (' **ERROR** FIELD ',I2,' IS NOT AN INTEGER')
      GO TO 230
70    CALL UNUMIC (IBUF,IFSTRT(IF),IFSTOP(IF),MXDY)
      IF (MXDY.EQ.0) GO TO 160
C
C  COMPARE OLD VALUE WITH NEW # FOR MAX DAYS
C
      IF (MXDY.GT.IDDTDR(7,IDX)) GO TO 90
C
C  NEW MAX DAYS IS SMALLER THAN PREVIOUS ONE
C
      WRITE (LPE,80)
80    FORMAT (' **ERROR** MAXDAY IS SMALLER THAN PREVIOUS MAXIMUM ',
     *       ' DAYS. NO CHANGE TO PPDB .')
      GO TO 230
90    CONTINUE
C
C  CALCULATE # OF RECORDS FOR THIS TYPE ON FILE
C
      CALL PDVALS (IDX,NREC1D,NFLREC,LSTREC)
C
C  SEE HOW MANY NEW RECORDS WILL BE ADDED TO PPDB
C
      NUMREC=MXDY*IDDTDR(21,IDX)
C
C  SEE IF ROOM TO ADD NEW RECORDS
C
      NUMADD=IPDDFC(2,IDF)+NUMREC
      IF (NUMADD.LE.IPDDFC(1,IDF)) GO TO 110
C
C  FILE IS TOO SMALL CANNOT CHANGE MAXDAY
C
      WRITE (LPE,100) IDTYP
100   FORMAT (' **ERROR** THE MAXIMUM # OF DAYS MAY NOT BE CHANGED',
     *       ' FOR DATA TYPE ',A4,' FILE IS TOO SMALL.')
      GO TO 230
110   CONTINUE
C
C  GET FIRST DATA RECORD FOR THIS TYPE
C
      NDAY=0
      IREC=IDDTDR(10,IDX)
      LREC=IPDDFC(2,IDF)+1
      ISAV=LREC
      IF (IREC.EQ.0) GO TO 130
      NDAY=IDDTDR(7,IDX)
      DO 120 I=1,NDAY
       CALL RVLRCD (KPDDDF(IDF),IREC,NREC1D,IBUFF,LRCPDD,ISTAT)
       IF (ISTAT.NE.0) GO TO 180
       IREC=IREC+NREC1D
       IF (IREC.GT.LSTREC) IREC=IDDTDR(15,IDX)
       CALL WVLRCD (KPDDDF(IDF),LREC,NREC1D,IBUFF,LRCPDD,ISTAT)
       IF (ISTAT.NE.0) GO TO 180
       LREC=LREC+NREC1D
120   CONTINUE
      NREC=LREC-NREC1D
C
C  RESET DATA POINTERS AND LAST USED RECORD
C
      IDDTDR(10,IDX)=ISAV
      IDDTDR(13,IDX)=NREC
      IDDTDR(7,IDX)=MXDY
      IPDDFC(2,IDF)=NUMADD
130   CONTINUE
      IDDTDR(15,IDX)=ISAV
      IF (IDTYP.NE.IPP24.OR.IDTYP.NE.IPPSR) GO TO 200
C
C  RESET REAMINING RECORDS TO MISSING
C
      IDAY=MXDY-NDAY
      N=NREC1D*LRCP
      DO 140 I=1,N
       IBUFF(I)=MISSPP
140   CONTINUE
      DO 150 K=1,IDAY
       CALL WVLRCD (KPDDDF(IDF),LREC,NREC1D,IBUFF,LRCPDD,ISTAT)
       IF (ISTAT.NE.0) GO TO 180
       LREC=LREC+NREC1D
150   CONTINUE
C
      GO TO 200
160   CONTINUE
C
C  INVALID INPUT
C
      WRITE (LPE,170)
170   FORMAT (' **ERROR** MAXDAY CANNOT BE ZERO.')
      GO TO 230
180   CONTINUE
C
C  SYSTEM ERROR
C
      WRITE (LPE,190) ISTAT
190   FORMAT (' **ERROR** IN PDCMXD - STATUS =',I2)
      GO TO 230
200   CONTINUE
C
C   DEBUG AND RETURN
C
      IF (IPDTR.EQ.1.OR.IPDDB.EQ.1) WRITE (IOGDB,210)
210   FORMAT (' *** EXIT PDCMXD')
      WRITE (LP,220) IDTYP,IDF
220   FORMAT (' MAX # OF DAYS FOR DATA TYPE ',A4,' CHANGED IN DAILY ',
     *       'DATA FILE ',I2)
      IFLAG=1
230   CONTINUE
      RETURN
      END
