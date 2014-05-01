C MEMBER HDELLD
C  (from old member HCLDELLD)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 11/18/94.12:45:06 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HDELLD
C
C
C          ROUTINE:  HDELLD
C
C             VERSION:  1.0.1 8-9-82 JCE ADD DEL ALL OR JUST ONE PARM
C
C             VERSION:  1.0.0
C
C                DATE:  6-2-82
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE DELETES THE LOCAL DEFAULT FOR A GLOBAL
C    PROC, FUNC, OR TECHNIQUE.  IT CHECKS TO SEE THAT THERE
C    IS A DEFAULT. THE RECORD IS DELETED BY WRITING THE FIRST
C    LOGICAL RECORD OF THE DEFINITION SET TO 0.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C             NONE
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
      INCLUDE 'common/where'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hcomnd'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER ITEMP(2),IROUT(2),ITYP(3),NAME(2),IRCBUF(16),NAME2(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hdelld.f,v $
     . $',                                                             '
     .$Id: hdelld.f,v 1.1 1995/09/17 18:41:55 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA IROUT/4HHDEL,4HLD  /,ITYP/4HPROC,4HFUNC,4HTECH/,LALL/4HALL /
C
C***********************************************************************
C
C
      IERR=0
      IADD1=0
C
      CALL UMEMOV (OPNAME,ITEMP,2)
      CALL UMEMOV (IROUT,OPNAME,2)
C
C        READ THE FIRST CARD
C
      NNCARD=1
      CALL HCARDR (NNCARD,ISTAT)
      IF (ISTAT.NE.0) GO TO 190
C
C        CHECK THE KEYWORD
C
      ITYPE=KEYWRD
      IF (ITYPE.EQ.3) IADD1=1
      IF (ITYPE.GE.1.AND.ITYPE.LE.3) GO TO 20
      WRITE (LP,10)
10    FORMAT ('0**ERROR** INVALID DEFINITION TYPE IN FIELD 2')
      IERR=1
      GO TO 210
20    CONTINUE
C
C        DO NEXT FIELD - ALL OR NAME
C
      IFLD=3
      IF (NFIELD.GE.IFLD) GO TO 50
30    WRITE (LP,40) IFLD
40    FORMAT ('0**ERROR** NAME OR ALL REQUIRED IN FIELD ',I2)
      IERR=1
      GO TO 210
50    CONTINUE
      NAME(2)=IBLNK
      NUM=IFSTOP(IFLD) - IFSTRT(IFLD)+1
      IF (NUM.GT.8) NUM=8
      CALL UPACK1(IBUF(IFSTRT(IFLD)),NAME,NUM)
C
C        SEE IF NAME OR ALL
C
      IF (NAME(1).EQ.LALL.AND.NAME(2).EQ.IBLNK) GO TO 160
      IFLD=4
C
C        HAD A NAME - SEE IF NEXT IS ALL OR JUST ONE
C
      IF (NFIELD.LT.IFLD) GO TO 30
      NAME2(2)=IBLNK
      NUM=IFSTOP(IFLD) - IFSTRT(IFLD)+1
      IF (NUM.GT.8) NUM=8
      CALL UPACK1(IBUF(IFSTRT(IFLD)),NAME2,NUM)
C
C        CHECK THE NAME
C
      CALL HFNDDF(NAME,IREC,ITYPE,IXREC)
      IF (ITYPE.GT.0) GO TO 60
      IF (IREC.NE.0) GO TO 80
60    WRITE (LP,70) ITYP(KEYWRD),NAME
70    FORMAT ('0**ERROR** GLOBAL ',A4,1X,2A4,' NOT FOUND')
      IERR=1
      GO TO 210
80    CONTINUE
C
C        GET THE DEFAULT RECORD NUMBER
C
      CALL UREADT (KDEFNG,IREC,IRCBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 190
      IDFREC=IRCBUF(7+IADD1)
      IF (IDFREC.LT.0) GO TO 190
      IF (IDFREC.EQ.0) GO TO 110
      IF (IHCLDB.EQ.3) WRITE (IOGDB,90) IDFREC
90    FORMAT (' DEFAULT RECORD #=',I6)
C
C        DELETE IT IF ALL
C
      IF (NAME2(1).NE.LALL.OR.NAME2(2).NE.IBLNK) GO TO 130
      CALL UMEMST(0,IRCBUF,16)
      CALL UWRITT(KLDFGD,IDFREC,IRCBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 190
      WRITE (LP,100) ITYP(KEYWRD),NAME
100   FORMAT (' **  LOCAL DEFAULTS FOR GLOBAL ',A4,1X,2A4,' DELETED')
      GO TO 230
110   CONTINUE
C
C        THERE WERE NO DEFAULTS
C
      WRITE (LP,120) NAME
120   FORMAT ('0**ERROR** ',2A4,' HAS NO DEFAULTS')
      IERR=1
      GO TO 210
130   CONTINUE
C
C        DELETE A SINGLE VALUE IN THE DEFAULT
C
      CALL HDLLD1(ITYPE,NAME,NAME2,IDFREC,IERR)
      IF (IERR.EQ.0) WRITE (LP,140) NAME2,ITYP(KEYWRD),NAME
140   FORMAT (' **  LOCAL DEFAULT FOR ',2A4,' DELETED FOR GLOBAL ',
     1       A4,1X,2A4)
      IF (IERR.NE.0) WRITE (LP,150) NAME2,ITYP(KEYWRD),NAME
150   FORMAT ('0**ERROR** LOCAL DEFAULT FOR ',2A4,' NOT DELETED FOR',
     1     ' GLOBAL ',A4,1X,2A4)
      GO TO 230
160   CONTINUE
C
C       DELETE ALL LOCAL DEFAULTS FOR A DEFINITION TYPE
C
      CALL HDLLDA(ITYPE,IERR)
      IF (IERR.EQ.0) WRITE (LP,170) ITYP(KEYWRD)
170   FORMAT (' **  ALL ',A4,' LOCAL DEFAULTS DELETED')
      IF (IERR.NE.0) WRITE (LP,180) ITYP(KEYWRD)
180   FORMAT ('0**ERROR** ALL ',A4,' LOCAL DEFAULTS NOT DELETED')
      GO TO 230
190   CONTINUE
C
C        SYSTEM ERROR
C
      WRITE (LP,200)
200   FORMAT ('0**ERROR** SYSTEM ERROR')
      IERR=1
210   CONTINUE
C
C        TELL WHAT HAPENED
C
      WRITE (LP,220)
220   FORMAT ('0**ERROR** LOCAL DEFAULTS NOT DELETED')
230   IF (IERR.EQ.1) CALL ERROR
      CALL UMEMOV (ITEMP,OPNAME,2)
C
      RETURN
C
      END
