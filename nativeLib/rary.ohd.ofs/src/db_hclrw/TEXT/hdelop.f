C MEMBER HDELOP
C  (from old member HCLDELOP)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/29/94.08:12:28 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HDELOP (IGL,NAME,IPASS)
C
C
C          ROUTINE:  HDELOP
C
C             VERSION:  1.0.0
C
C                DATE:  4-20-82
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE EXECUTES THE DELETE NAMED OPTION COMMAND
C    IT SEARCHES ALL NAMED OPTIONS TO SEE IF IT IS INCLUDED IN
C    OTHER NAMED OPTIONS AND IT SEARCHES ALL LOCAL PROCS TO
C    SEE IF INCLUDED.  IF FOUND, IT IS NOT DELETED. PASSWORD MUST
C    BE CORRECT
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IGL        I     I     1    DELG/DELL INDICATOR
C
C       NAME       A8    I     2    NAME OF N. O. TO DELETE
C
C       IPASS      A4    I     1    PASSWORD
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'common/where'
      INCLUDE 'ufreei'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hindx'
      INCLUDE 'hclcommon/hunits'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER NAME(2),IWORD(2),ITEMP(2),IROUT(2),INCLUD(2),IXBUF(4)
      INTEGER ISAVE(16),NOBUF(600),ISTRNG(20),LDELET(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hdelop.f,v $
     . $',                                                             '
     .$Id: hdelop.f,v 1.1 1995/09/17 18:41:56 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA IROUT(1)/4HHDEL/,IROUT(2)/4HOP  /,LDELET(1)/4HDELE/
      DATA LDELET(2)/4HTED /,INCLUD(1)/4HINCL/,INCLUD(2)/4HUDE /
      DATA MAXN/600/,MAXS/20/
C
C***********************************************************************
C
C
      CALL UMEMOV (OPNAME,ITEMP,2)
      CALL UMEMOV (IROUT,OPNAME,2)
C
      IERR=0
C
C        CHECK IGL
C
      IF (IGL.GT.0) GO TO 20
      WRITE (LPE,10)
10    FORMAT (' **ERROR** @DELG AND @DELETEG COMMANDS NOT VALID FOR',
     1       ' NAMED OPTIONS')
      IERR=1
      GO TO 270
20    CONTINUE
C
C        FIND THE RECORD
C
      ITYPE=4
      CALL HFNDDF(NAME,IREC,ITYPE,IXREC)
      IF (IREC.NE.0) GO TO 40
      WRITE (LPE,30) NAME
30    FORMAT (' **ERROR** NAMED OPTION ',2A4,' NOT DEFINED')
      IERR=1
      GO TO 270
40    CONTINUE
C
C        GET RECORD AND CHECK PASSWORD
C
      CALL HGTRDN(KDEFNL,IREC,NOBUF,MAXN,ISTAT)
      IF (ISTAT.NE.0) GO TO 250
      IF (IPASS.EQ.NOBUF(6)) GO TO 60
      WRITE (LPE,50) NAME
50    FORMAT (' **ERROR** PASSWORD SUPPLIED FOR NAMED OPTION ',2A4,
     1       ' IS NOT CORRECT')
      IERR=1
      GO TO 270
60    CONTINUE
      CALL UMEMOV (NOBUF,ISAVE,16)
      NONUM=-NOBUF(2)
C
C        SEE IF NAMED OPTION USED IN OTHER NAMED OPTIONS
C
      IS=HINDEX(2,4)
      IE=HINDEX(3,4)
      IF (IE.LT.IS) GO TO 130
      DO 120 I=IS,IE
          CALL UREADT (KINDXL,I,IXBUF,ISTAT)
          IF (ISTAT.NE.0) GO TO 250
C  GET NAMED OPTION RECORD IF NOT DELETED
          CALL UNAMCP(LDELET,IXBUF,ISTAT)
          IF (ISTAT.EQ.0) GO TO 120
          CALL HGTRDN(KDEFNL,IXBUF(3),NOBUF,MAXN,ISTAT)
          IF (ISTAT.NE.0) GO TO 250
C  SEARCH FOR INCLUDE OPTION IN THIS OPTION
          IPOS=7
70        CONTINUE
          IF (NOBUF(IPOS).EQ.0) GO TO 100
          IF (NOBUF(IPOS).EQ.NONUM) GO TO 80
          IF (NOBUF(IPOS).LT.0) IPOS=IPOS+1
          IF (NOBUF(IPOS).GT.0) IPOS=IPOS+NOBUF(IPOS)+1
          GO TO 70
80        CONTINUE
C
C  FOUND IT
C
          IERR=1
          WRITE (LPE,90) NAME,NOBUF(4),NOBUF(5)
90        FORMAT (' **ERROR** NAMED OPTION ',2A4,' FOUND INCLUDED IN',
     1            ' NAMED OPTION ',2A4)
100       CONTINUE
          IF (IHCLDB.EQ.3) WRITE (IOGDB,110) NOBUF(4),NOBUF(5)
110       FORMAT ('  SEARCHED NAMED OPTION ',2A4)
120       CONTINUE
C
C      SEARCHED ALL NAMED OPTIONS NEXT DO PROCS
C
130   IF (IHCLDB.EQ.3) WRITE (IOGDB,140)
140   FORMAT ('  SEARCHED ALL NAMED OPTIONS')
C
C        SEARCH LOCAL PROCS FOR INCLUDE
C
      IS=HINDEX(2,1)
      IE=HINDEX(3,1)
      IF (IE.LT.IS) GO TO 230
      DO 220 I=IS,IE
          CALL UREADT (KINDXL,I,IXBUF,ISTAT)
          IF (ISTAT.NE.0) GO TO 250
C  GET PROC IF NOT DELETED
          CALL UNAMCP(LDELET,IXBUF,ISTAT)
          IF (ISTAT.EQ.0) GO TO 220
          CALL HGTRDN(KDEFNL,IXBUF(3),NOBUF,MAXN,ISTAT)
          IF (ISTAT.NE.0) GO TO 250
C
C  SEARCH COMMAND STRINGS IN PROC
C
          IPOS=12+NOBUF(9)*2
          NUMSTR=NOBUF(IPOS - 2)
          IF (NUMSTR.EQ.0) GO TO 200
          DO 170 II=1,NUMSTR
              CALL HGTSTR(MAXS,NOBUF(IPOS),ISTRNG,LEN,ISTAT)
              IF (ISTAT.NE.0) GO TO 250
C  EXPAND STRING INTO IBUF (80A1) AND PARSE
              NUM=(LEN+3)/4
              CALL UNPAKS(ISTRNG,IBUF,NUM,80,ISTAT)
              IF (ISTAT.NE.0) GO TO 250
              CALL UFREE(1,80)
C  SEE IF FIRST WORD IS INCL OR INCLUDE
              IF (NFIELD.EQ.1) GO TO 160
              K=IFSTRT(1)
              NUM=IFSTOP(1) - K+1
              IF (NUM.GT.8) GO TO 160
              IWORD(2)=IBLNK
              CALL UPACK1(IBUF(K),IWORD,NUM)
              CALL UNAMCP(INCLUD,IWORD,ISTAT)
              IF (ISTAT.EQ.0) GO TO 150
              IF (IWORD(1).EQ.INCLUD(1).AND.IWORD(2).EQ.IBLNK) GO TO 150
              GO TO 160
150           CONTINUE
C
C  HAVE INCLUDE CHECK THE NAME
C
              K=IFSTRT(2)
              NUM=IFSTOP(2) - K+1
              IF (NUM.GT.8) GO TO 160
              IWORD(2)=IBLNK
              CALL UPACK1(IBUF(K),IWORD,NUM)
              CALL UNAMCP(NAME,IWORD,ISTAT)
              IF (ISTAT.EQ.0) GO TO 180
160           CONTINUE
              IPOS=IPOS+NOBUF(IPOS - 1)+1
170           CONTINUE
          GO TO 200
C
C  FOUND IT
C
180       CONTINUE
          WRITE (LPE,190) NAME,NOBUF(4),NOBUF(5)
190       FORMAT (' **ERROR** NAMED OPTION ',2A4,' INCLUDED IN ',
     1           'PROCEDURE ',2A4)
          IERR=1
200       CONTINUE
          IF (IHCLDB.EQ.3) WRITE (IOGDB,210) NOBUF(4),NOBUF(5)
210       FORMAT ('  SEARCHED PROC ',2A4)
220       CONTINUE
C
C     SEARCHED ALL PROCS
C
230   IF (IHCLDB.EQ.3) WRITE (IOGDB,240)
240   FORMAT ('  SEARCHED ALL PROCS')
C
C        TIME TO DELETE NOW
C
      IF (IERR.EQ.1) GO TO 270
      CALL UREADT (KINDXL,IXREC,IXBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 250
      CALL UMEMOV (LDELET,IXBUF,2)
      CALL UWRITT(KINDXL,IXREC,IXBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 250
      ISAVE(2)=0
      CALL UWRITT(KDEFNL,IREC,ISAVE,ISTAT)
      IF (ISTAT.EQ.0) GO TO 270
250   CONTINUE
C
C        SYSTEM ERROR
C
      WRITE (LPE,260)
260   FORMAT (' **ERROR**  SYSTEM ERROR')
      IERR=1
270   CONTINUE
      IF (IERR.EQ.0) GO TO 290
      WRITE (LP,280) NAME
280   FORMAT (' ***  NAMED OPTION ',2A4,' NOT DELETED')
      CALL ERROR
      RETURN
290   CONTINUE
      WRITE (LP,300) NAME
300   FORMAT (' ***  NAMED OPTION ',2A4,' DELETED')
C
      RETURN
C
      END
