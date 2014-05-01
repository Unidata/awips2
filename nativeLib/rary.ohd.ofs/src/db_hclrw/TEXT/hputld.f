C MEMBER HPUTLD
C  (from old member HCLPUTLD)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/28/94.12:28:59 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HPUTLD (ITYPE,NAME,NUM,IGARAY,ISTAT)
C
C
C          ROUTINE:  HPUTLD
C
C             VERSION:  1.0.0
C
C                DATE:  3-18-82
C
C              AUTHOR:  SONJA R SIEGEL
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE WILL PUT A LOCAL DEFINITION REFERENCE RECORD INTO THE*
C    SPACE RESERVED IN THE GLOBAL INDEX FILE.  THESE RECORDS CONTAIN
C    THE NAME OF THE LOCAL DEFINITION, AND ANY REFERENCE TO GLOBAL
C    DEFINITIONS MADE BY THE LOCAL.  PROC CAN REFERENCE FUNC, TECHS AND*
C    OTHER PROCS, FUNCS CAN REFERENCE TECHS,  TECHS CANNOT REF ANYTHING*
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ITYPE     I      I     1    TYPE OF RECORD (1=PROC, 2=FUNC,
C                                       3=TECH
C       NAME     A8     I     2    DEFINITION NAME 8CHARS MAX
C       NUM      I      I     1    NUMBER OF ENTRIES IN IGARAY
C       IGARAY   I      I     NUM  ARRAY CONTAINING GLOBAL POINTERS
C       ISTAT    I      O     1    STATUS, =0 OK
C                                     NOT ZERO, ERROR
C
C
C
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'common/where'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hindx'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION ITEMP(2),IROUT(2),IBUF(50),IGARAY(1)
      DIMENSION NAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hputld.f,v $
     . $',                                                             '
     .$Id: hputld.f,v 1.1 1995/09/17 18:42:58 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA IROUT/4HHPUT,4HLD  /,MAXNUM/43/
C
C***********************************************************************
C
C
      ISTAT=0
      CALL UMEMOV (OPNAME,ITEMP,2)
      CALL UMEMOV (IROUT,OPNAME,2)
C INITIALIZE BUFFER
C
      CALL UMEMST(0,IBUF,50)
      IF (ITYPE.GE.1.AND.ITYPE.LE.3) GO TO 20
      WRITE (LPE,10) ITYPE,NAME
10    FORMAT (' **SYSTEM ERROR** INVALID TYPE PASSED TO HPUTLD',I6,1X,
     1  2A4)
      GO TO 100
C
20    IBUF(2)=ITYPE
      CALL UMEMOV (NAME,IBUF(3),2)
      CALL UMEMOV (HNAMRF(1),IBUF(5),2)
      IF (NUM.EQ.0) GO TO 50
C
C NOW DO GLOBAL POINTERS
C
      IF (NUM.LE.MAXNUM) GO TO 40
      WRITE (LPE,30) NAME,NUM
30    FORMAT (' **SYSTEM ERROR IN HPUTLD, BUFFER TOO SMALL NAME=',2A4,
     1       ' NUM=',I6)
      GO TO 100
C
40    IBUF(7)=NUM
      CALL UMEMOV (IGARAY,IBUF(8),NUM)
C
C SET NUMBER OF 4 WORD RECORDS
C
50    IBUF(1)=(NUM+10)/4
C
C NOW WRITE THEM
C
      IF (HINDEX(3,8)+IBUF(1).LE.HINDEX(1,8)) GO TO 70
      WRITE (LPE,60) HINDEX(1,8)
60    FORMAT (' **SYSTEM ERROR** LOCAL DEFINITION REFERENCE RECORDS ',
     1       'ARE FULL AT ',I6)
      GO TO 100
70    IREC=HINDEX(3,8)+1
      CALL WVLRCD(KINDXG,IREC,IBUF(1),IBUF,4,ISTAT)
      IF (ISTAT.EQ.0) GO TO 90
C
      WRITE (LPE,80) KINDXG,IREC
80    FORMAT (' **ERROR** WRITING LOCAL DEFINITION REFERENCE RECORD',
     1   2I6)
      GO TO 100
C
90    HINDEX(3,8)=HINDEX(3,8)+IBUF(1)
      GO TO 110
100   CALL ERROR
      ISTAT=1
110   J=NUM+7
      IF (IHCLDB.EQ.3) WRITE (IOGDB,120) IREC,(IBUF(I),I=1,J)
120   FORMAT (' HPUTLD,IREC=',I5,' BUF=',2I3,4A4,14I4/(30I4))
      CALL UMEMOV (ITEMP,OPNAME,2)
C
      RETURN
C
      END
