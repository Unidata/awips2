C MEMBER HDLLD1
C  (from old member HCLDELLD)
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE HDLLD1 (ITYPE,NAME,NAMED,IDFREC,ISTAT)
C
C          ROUTINE:  HDLLD1
C
C             VERSION:  1.0.0
C
C                DATE:  8-9-82
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE DELETES ONE DEFAULT VALUE FROM A LOCAL
C    DEFAULT FOR A GLOBAL DEFINITION RECORD.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ITYPE      I     I     1    TYPE OF DEFINITION (-)
C
C       NAME      A8     I     2    NAME OF DEFINITION
C
C       NAMED     A8     I     2    NAME OF DEFAULT TO DELETE
C
C       IDFREC    I      I     1    LOCAL DEFAULT RECORD #
C
C       ISTAT     I      O     1    STATUS INDICATOR
C                                    0=NORMAL RETURN
C                                    1=AN ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hunits'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER NAME(2),NAMED(2),IRCBUF(600),IXBUF(4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hdlld1.f,v $
     . $',                                                             '
     .$Id: hdlld1.f,v 1.1 1995/09/17 18:42:03 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA MAX/600/
C
C***********************************************************************
C
C
      ISTAT=0
C
C        GET DEFINITION RECORD IF NOT FUNCTION
C
      IF (ITYPE.EQ.-2) GO TO 90
      CALL HGTRCD(ITYPE,NAME,MAX,IRCBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 280
C
C        FIND INTERNAL POINTER IF PROC
C
      IF (ITYPE.EQ.-3) GO TO 40
      IPOS=10
      NUM=IRCBUF(9)
      IF (NUM.EQ.0) GO TO 20
      DO 10 IPOINT=1,NUM
          CALL UNAMCP(IRCBUF(IPOS),NAMED,ISTAT)
          IF (ISTAT.EQ.0) GO TO 120
          IPOS=IPOS+2
10    CONTINUE
C
C       DIDNT FIND IT
C
20    WRITE (LP,30) NAMED,NAME
30    FORMAT ('0**ERROR** PARAMETER ',2A4,' NOT FOUND IN GLOBAL PROC',
     1       'EDURE ',2A4)
      ISTAT=1
      GO TO 280
40    CONTINUE
C
C        FIND INTERNAL POINTER IF TECHNIQUE
C
      IPOS=11
      NUM=IRCBUF(10)
      IF (NUM.EQ.0) GO TO 60
      DO 50 I =1,NUM
          CALL UNAMCP(IRCBUF(IPOS),NAMED,ISTAT)
          IF (ISTAT.EQ.0) GO TO 80
          IPOS=IPOS+4
50    CONTINUE
C
C       DIDNT FIND IT
C
60    WRITE (LP,70) NAMED,NAME
70    FORMAT ('0**ERROR** ARGUMENT ',2A4,' NOT FOUND IN GLOBAL TECH',
     1       'NIQUE ',2A4)
      ISTAT=1
      GO TO 280
80    CONTINUE
C
C        SET POINTER FOR ARGUMENT
C
      IPOINT=IRCBUF(IPOS+3)
      GO TO 120
90    CONTINUE
C
C        FIND THE TECHNIQUE IN FILES
C
      ITYPT=3
      CALL HFNDDF(NAMED,IREC,ITYPT,IXREC)
      IF (ITYPT.LT.0.AND.IXREC.NE.0) GO TO 110
      WRITE (LP,100) NAMED
100   FORMAT ('0**ERROR** GLOBAL TECHNIQUE ',2A4,' NOT FOUND')
      ISTAT=1
      GO TO 280
110   CONTINUE
C
C        GET TECHNIQUE POINTER
C
      CALL UREADT (KINDXG,IXREC,IXBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 260
      IPOINT=-IXBUF(4)
120   CONTINUE
C
C        GET THE DEFAULT RECORD
C
      CALL HGTRDN(KLDFGD,IDFREC,IRCBUF,MAX,ISTAT)
      IF (ISTAT.NE.0) GO TO 260
      IF (IRCBUF(1).GT.0) GO TO 140
      WRITE (LP,130) NAME
130   FORMAT ('0**ERROR** THE LOCAL DEFAULT FOR ',2A4,
     *   ' HAS NO DEFAULTS.')
      ISTAT=1
      GO TO 280
140   CONTINUE
C
C        FIND THE INTERNAL NUMBER IN DEFAULT RECORD
C
      LL=IRCBUF(1) * 16
      IF (IHCLDB.EQ.3) WRITE (IOGDB,150) IDFREC,(IRCBUF(L),L=1,LL)
150   FORMAT (' DEFAULT RECORD #=',I6,' DEFAULT RECORD='/1X,3I4,
     1       1X,2A4,2I4/(1X,10I6))
      IF (IHCLDB.EQ.3) WRITE (IOGDB,160) IPOINT
160   FORMAT (' LOOKING FOR ',I4,' IN DEFAULT RECORD')
      IPOS=8
      NUM=IRCBUF(7)
      DO 170 I=1,NUM
          IF (IPOINT.EQ.IRCBUF(IPOS)) GO TO 190
          INCR=2
          IF (ITYPE.NE.-2) INCR=INCR+IRCBUF(IPOS+1)
          IPOS=IPOS+INCR
170   CONTINUE
C
C        DIDNT FIND IT
C
      WRITE (LP,180) NAMED,NAME
180   FORMAT ('0**ERROR** NO DEFAULT FOR ',2A4,' FOUND IN DEFAULT ',
     1   'RECORD FOR ',2A4)
      ISTAT=1
      GO TO 280
190   CONTINUE
C
C        TAKE IT OUT OF THE DEFAULT RECORD
C
       IRCBUF(7)=IRCBUF(7) - 1
       IF (IRCBUF(7).GT.0) GO TO 200
C
C        NONE LEFT  ZAP THE RECORD
C
      CALL UMEMST(0,IRCBUF,16)
      CALL UWRITT(KLDFGD,IDFREC,IRCBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 260
      GO TO 280
200   CONTINUE
C
C        DO IT FOR A FUNCTION RECORD
C
      IF (ITYPE.NE.-2) GO TO 220
      NUMTMV=(NUM - I) * 2
      IF (NUMTMV.EQ.0) GO TO 210
      CALL UMEMOV (IRCBUF(IPOS+2),IRCBUF(IPOS),NUMTMV)
210   CONTINUE
C
C        ZAP OUT LAST 2 WORDS
C
      IEND=NUM * 2+6
      CALL UMEMST(0,IRCBUF(IEND),2)
      GO TO 250
220   CONTINUE
C
C        DO PROC AND TECHNIQUE RECORD
C
      NUMLST=IRCBUF(IPOS+1)+2
      NUMTMV=NUM - I
      IF (NUMTMV.EQ.0) GO TO 240
C
C        SEE HOW MANY WORDS TO MOVE UP IN RECORD
C
      IPOSM=IPOS+IRCBUF(IPOS+1)+2
      IPOSMM=IPOSM
      N=0
      DO 230 J=1,NUMTMV
          NWDS=IRCBUF(IPOSM+1)+2
          IPOSM=IPOSM+NWDS
          N=N+NWDS
230   CONTINUE
C
C        MOVE IT
C
      CALL UMEMOV (IRCBUF(IPOSMM),IRCBUF(IPOS),N)
      IPOS=IPOS+N
240   CONTINUE
C
C        ZAP OUT LAST ONE
C
      CALL UMEMST(0,IRCBUF(IPOS),NUMLST)
250   CONTINUE
C
C        WRITE THE DEFAULT RECORD
C
      IF (IHCLDB.EQ.3) WRITE (IOGDB,150) IDFREC,(IRCBUF(L),L=1,LL)
      CALL WVLRCD(KLDFGD,IDFREC,IRCBUF(1),IRCBUF,16,ISTAT)
      IF (ISTAT.EQ.0) GO TO 280
260   CONTINUE
C
C        SYSTEM ERROR
C
      WRITE (LP,270)
270   FORMAT ('0**ERROR** SYSTEM ERROR')
280   CONTINUE
C
C       THE END
C
      RETURN
C
      END
