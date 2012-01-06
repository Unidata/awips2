C MEMBER HFEXCP
C  (from old member HCLCKDEF)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/29/94.08:12:16 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HFEXCP (IGL,INTNUM,IFLAG,IERR)
C
C          ROUTINE:  HFEXCP
C
C             VERSION:  1.0.0
C
C                DATE:  3-16-82
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE SEARCHES A CARD IMAGE FOR AN EXECUTE, COMPUTE,
C    OR INCLUDE. IF FOUND,  THE DEFINITION RECORD IS FOUND.  IF FOUND,
C    AND IT IS GLOBAL, THE INTERNAL NUMBER IS PLACED IN INTNUM. 500
C    IS ADDED TO INTNUM FOR PROCEDURES. IF DEFINITION IS NOT FOUND,
C    IERR IS SET TO 1 AND AN ERROR MESSAGE IS PRINTED
C    IF NAME IS A PARM A MESSAGE IS PRINTED
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IGL        I     I     1    LOCAL/GLOBAL IDICATOR (PROC)
C
C       INTNUM     I     O     1    INTERNAL NUMBER OF DEFINITION
C
C       IFLAG      I     I     1    PRINT FLAG FOR CHANGE PROC
C                                     0=PRINT MESSAGE
C                                     1=DONT PRINT MESSAGE
C
C       IERR       I     O     1    ERROR INDICATOR
C                                     0=NO ERROR
C                                     1=DEFINITION NOT FOUND
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hunits'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER ICOMP(2),IEXEC(2),ITEMP(2),NAME(2),IXBUF(4),LFUN(2),
     *        LPROC(2),IWHAT(2),INCLUD(2),LNOP(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hfexcp.f,v $
     . $',                                                             '
     .$Id: hfexcp.f,v 1.1 1995/09/17 18:42:15 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA IEXEC(1)/4HEXEC/,IEXEC(2)/4HUTE /,ICOMP(1)/4HCOMP/,
     *     ICOMP(2)/4HUTE /,LAT/4H@   /,LFUN(1)/4HFUNC/,LFUN(2)/4HTION/,
     *     LPROC(1)/4HPROC/,LPROC(2)/4H    /,INCLUD(1)/4HINCL/,
     *     INCLUD(2)/4HUDE /,LNOP(1)/4HNAME/,LNOP(2)/4H OPT/,
     *     LAMPER/4H&   /
C
C***********************************************************************
C
C
      IERR=0
      INTNUM=0
C
C         PUT FIRST FIELD IN ITEMP
C
      K=IFSTRT(1)
      IF (IBUF(K).EQ.LAT) K=K+1
      NUM=IFSTOP(1)-K+1
      IF (NUM.GT.8) GO TO 130
      ITEMP(2)=IBLNK
      CALL UPACK1 (IBUF(K),ITEMP,NUM)
C
C         PACK NAME AND SEE WHAT WE HAVE
C
      IF (NFIELD.EQ.1) GO TO 130
      KK=IFSTRT(2)
      IF (IBUF(KK).EQ.LAMPER) KK=KK+1
      NUM=IFSTOP(2)-KK+1
      IF (NUM.GT.8) NUM=8
      NAME(2)=IBLNK
      CALL UPACK1 (IBUF(KK),NAME,NUM)
C
      CALL UNAMCP (ITEMP,ICOMP,ISTAT)
      IF (ISTAT.EQ.0) GO TO 10
      CALL UNAMCP (ITEMP,IEXEC,ISTAT)
      IF (ISTAT.EQ.0) GO TO 20
      CALL UNAMCP (ITEMP,INCLUD,ISTAT)
      IF (ISTAT.EQ.0) GO TO 30
      IF (ITEMP(1).EQ.ICOMP(1).AND.ITEMP(2).EQ.IBLNK) GO TO 10
      IF (ITEMP(1).EQ.IEXEC(1).AND.ITEMP(2).EQ.IBLNK) GO TO 20
      IF (ITEMP(1).EQ.INCLUD(1).AND.ITEMP(2).EQ.IBLNK) GO TO 30
      GO TO 130
10    CONTINUE
C
C         HAVE A COMPUTE
C
      IF (IBUF(IFSTRT(1)).NE.LAT) GO TO 130
      ITYPE=2
      CALL UMEMOV (LFUN,IWHAT,2)
      GO TO 40
20    CONTINUE
C
C         HAVE AN EXECUTE
C
      IF (IBUF(IFSTRT(1)).NE.LAT) GO TO 130
      ITYPE=1
      CALL UMEMOV (LPROC,IWHAT,2)
      GO TO 40
30    CONTINUE
C
C        HAVE AN INCLUDE
C
      IF (K.NE.IFSTRT(1)) GO TO 130
      ITYPE=4
      CALL UMEMOV (LNOP,IWHAT,2)
40    CONTINUE
C
C        CHECK FOR A SUBSTITUTED PARAMETER
C
      IF (IBUF(IFSTRT(2)).NE.LAMPER) GO TO 60
      IF (IFLAG.EQ.1) GO TO 130
      WRITE (LP,50) NAME,IWHAT
50    FORMAT (' **NOTE** DEFAULT VALUE FOR PARAMETER ',2A4,' HAS NOT',
     *       ' BEEN CHECKED TO BE AN EXISTING ',2A4)
      GO TO 130
60    CONTINUE
C
C         GET THE RECORD
C
      CALL HFNDDF (NAME,IREC,ITYPE,IXREC)
      IF (IREC.NE.0) GO TO 80
      WRITE (LPE,70) IWHAT,NAME
70    FORMAT (' **ERROR** ',2A4,1X,2A4,' HAS NOT BEEN DEFINED')
      IERR=1
      GO TO 130
80    CONTINUE
C
C        CHECK FOR GLOBAL USING LOCAL
C
      IF (IGL.GT.0.OR.ITYPE.LT.0) GO TO 100
      WRITE (LPE,90) IWHAT,NAME
90    FORMAT (' **ERROR** A GLOBAL PROC CANT USE THE LOCAL ',2A4,1X,2A4)
      IERR=1
      GO TO 130
100   CONTINUE
C
C         IF GLOBAL PUT IT IN INTNUM
C
      IF (ITYPE.GT.0) GO TO 130
C
      CALL UREADT (KINDXG,IXREC,IXBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 110
      INTNUM=IXBUF(4)
      IF (ITYPE.EQ.-1) INTNUM=INTNUM+500
      GO TO 130
110   CONTINUE
C
C        DAIO OR SYSTEM ERROR
C
      WRITE (LPE,120)
120   FORMAT (' **ERROR**  DAIO OR SYSTEM ERROR')
      IERR=1
130   CONTINUE
C
C         DEBUG AND RETURN
C
      IF (IHCLDB.EQ.3) WRITE (IOGDB,140) IWHAT,NAME,INTNUM,IERR
140   FORMAT (' HFEXCP EXECUTED : ITYPE,NAME,INTNUM,STATUS=',2A4,
     *     2X,2A4,I5,I3)
C
      RETURN
C
      END
