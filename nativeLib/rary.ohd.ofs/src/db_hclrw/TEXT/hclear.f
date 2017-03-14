C MEMBER HCLEAR
C  (from old member HCLCLEAR)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 11/18/94.12:41:33 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HCLEAR
C
C          ROUTINE:  HCLEAR
C
C             VERSION:  1.1.0  11-30-81   ADD CALLS TO ERROR
C
C             VERSION:  1.0.0
C
C                DATE:  9 30 81
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    ROUTINE TO CLEAR RUN-TIME OPTIONS FOR LISTED FUNCTION(S),
C    ALL FUNCTIONS, AND CONDITION CODE
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       NONE
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'udatas'
      INCLUDE 'ufreei'
      INCLUDE 'common/killcd'
      INCLUDE 'hclcommon/hword2'
      INCLUDE 'hclcommon/hcomnd'
      INCLUDE 'hclcommon/hseg1'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'common/where'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION ICONCD(2),NAME(2),IFUNRC(128),ITEMP(2),IROUT(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hclear.f,v $
     . $',                                                             '
     .$Id: hclear.f,v 1.1 1995/09/17 18:41:46 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA ICONCD/4HCOND,4HCODE/
      DATA IROUT/4HHCLE,4HAR  /
C
C***********************************************************************
C
C
      MAX=128
C
      CALL UMEMOV (OPNAME,ITEMP,2)
      CALL UMEMOV (IROUT,OPNAME,2)
C
C          READ FIRST CARD
C
      NUMOPT=0
      NNCARD=1
      CALL HCARDR (NNCARD,ISTAT)
      IF (ISTAT.NE.0) GO TO 170
      IF (NFIELD.EQ.1.AND.NCARD.EQ.NNCARD) GO TO 130
      IFLD=1
C
C          CHECK FOR MORE FIELDS
C
10    CONTINUE
      IF (NUMOPT.NE.1) GO TO 30
      WRITE (LP,20)
20    FORMAT ('0**ERROR** NO RUNTIME OPTIONS DEFINED FOR FUNCTION')
      CALL ERROR
      NUMOPT=0
30    CONTINUE
      IFLD=IFLD+1
      IF (IFLD.LE.NFIELD) GO TO 40
C
C          READ MORE CARDS
C
      NNCARD=NNCARD+1
      IF (NNCARD.GT.NCARD) GO TO 190
      CALL HCARDR (NNCARD,ISTAT)
      IF (ISTAT.NE.0) GO TO 170
      IFLD=1
40    CONTINUE
C
C          PACK NEXT FIELD
C
      NAME(2)=IBLNK
      K=IFSTRT(IFLD)
      N=IFSTOP(IFLD)
      NUM=N-K+1
      IF (NUM.LE.8) GO TO 60
      WRITE (LP,50) IFLD
50    FORMAT ('0**ERROR** IN HCLEAR - INVALID NAME IN FIELD ',I2)
      CALL ERROR
      GO TO 10
60    CONTINUE
      CALL UPACK1(IBUF(K),NAME,NUM)
C
C          CHECK NAME
C
      IF (NAME(1).NE.LALL.OR.NAME(2).NE.IBLNK) GO TO 70
      NUMOPT=1
      GO TO 90
70    CALL UNAMCP(NAME,ICONCD,ISTAT)
      IF (ISTAT.EQ.0) GO TO 150
C
C          READ FUNCTION RECORD
C
      ITYPE=2
      CALL HGTRCD (ITYPE,NAME,MAX,IFUNRC,ISTAT)
      IREC=1
      IF (ISTAT.NE.0) GO TO 10
      NUMOPT=1
      IFNUM=IFUNRC(2)
      IF (MAXOPT.EQ.0) WRITE (LP,80)
80    FORMAT (' THERE ARE NO RUN-TIME OPTIONS TO CLEAR')
      GO TO 100
90    CONTINUE
C
C        CLEAR ALL COMMAND
C
      IFNUM=0
      IREC =1
      IF (MAXOPT.EQ.0) WRITE (LP,80)
C
C          SEE IF HAVE MORE OPTIONS
C
100   CONTINUE
      IF (IREC.GT.MAXOPT) GO TO 10
C
C          READ OPTIONS AND CHECK FOR MATCH
C
      CALL UREADT (KHDFLT,IREC,IOPTRC,ISTAT)
      IF (ISTAT.NE.0) GO TO 170
      IF (IOPTRC(2).NE.IFNUM) GO TO 120
      IOPTRC(3)=0
      NUMOPT=NUMOPT+1
      CALL UWRITT (KHDFLT,IREC,IOPTRC,ISTAT)
      IF (ISTAT.NE.0) GO TO 170
      WRITE (LP,110) NAME
110   FORMAT (' ** RUN-TIME OPTIONS CLEARED FOR FUNCTION ',2A4)
120   CONTINUE
      IREC=IREC+IOPTRC(1)
      GO TO 100
C
C          CLEAR ALL OPTIONS
C
130   CONTINUE
      MAXOPT=0
      WRITE (LP,140)
140   FORMAT (' ** ALL RUN-TIME OPTIONS CLEARED')
      GO TO 190
C
C          CLEAR CONDITION CODE
C
150   CONTINUE
      KLCODE=0
      WRITE (LP,160)
160   FORMAT (' ** CONDITION CODE RESET TO 0')
      GO TO 190
C
C          SYSTEM ERROR
C
170   CONTINUE
      WRITE (LP,180)
180   FORMAT ('0**ERROR** IN HCLEAR - SYSTEM ERROR')
      CALL ERROR
C
C          WRITE DEBUG AND RETURN
C
190   CONTINUE
      IF (IHCLDB.EQ.3) WRITE (IOGDB,200)
200   FORMAT ('  HCLEAR EXECUTED')
      CALL UMEMOV (ITEMP,OPNAME,2)
C
      RETURN
C
      END
