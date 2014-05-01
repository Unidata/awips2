C MODULE HDFLTP
C-----------------------------------------------------------------------
C
      SUBROUTINE HDFLTP (IGL,IF,NAME,IPASS,NNCARD)
C
C          ROUTINE:  HDFLTP
C          VERSION:  1.0.0
C             DATE:  4-12-82
C           AUTHOR:  JIM ERLANDSON
C                    DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE SETS DEFAULTS FOR PROCEDURE DEFINITIONS.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IGL        I     I     1    TYPE INDICATOR
C                                     1=SET LOCAL DEFAULTS
C                                    -1=SET GLOBAL DEFAULTS
C       IF         I    I/O    1    FIELD POSITION
C       NAME       A8    I     1    NAME OF PROC
C       IPASS      A4    I     1    SUPPLIED PASSWORD
C       NNCARD     I     I     1    LAST INPUT CARD READ
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
      INCLUDE 'hclcommon/hcomnd'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      PARAMETER (LMAX=2000)
      INTEGER PBUF(LMAX),DBUF(LMAX)
      INTEGER NAME(2),DNAME(2),RIGHT(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hdfltp.f,v $
     . $',                                                             '
     .$Id: hdfltp.f,v 1.2 1998/04/07 12:54:17 page Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA LQUOTE/4H4H   /
C
C***********************************************************************
C
C
      IF (IHCLTR.GT.1) WRITE (IOGDB,5)
5     FORMAT (' ENTER HDFLTP')
C
      LDFFLG=0
      IERR=0
C
C  GET THE PROC RECORD
      ITYPE=1
      CALL HGTRCD (ITYPE,NAME,LMAX,PBUF,ISTAT)
      IF (ISTAT.EQ.0) GO TO 10
         IERR=1
         GO TO 220
C
C  CHECK THE PASSWORD
10    IF (ITYPE.LT.0.AND.IGL.EQ.1) GO TO 30
      IF (IPASS.EQ.PBUF(6)) GO TO 30
         CALL ULINE (LP,2)
         WRITE (LP,20) NAME
20    FORMAT ('0**ERROR** INVALID PASSWORD FOR PROCEDURE ',2A4,'.')
         IERR=1
C
C  CHECK THE TYPE
30    IF (IGL.NE.-1.OR.ITYPE.NE.1) GO TO 50
         CALL ULINE (LP,2)
         WRITE (LP,40) NAME
40    FORMAT ('0**ERROR** PROCEDURE ',2A4,' IS A LOCAL PROCEDURE. ',
     *   '@SETGDFLT IS NOT VALID.')
         IERR=1
         GO TO 220
C
C  GET THE DEFAULT RECORD
50    IF (PBUF(9).EQ.0) GO TO 280
      IDFREC=PBUF(7)
      IDFUNT=KDEFNL
      IF (ITYPE.LT.0) IDFUNT=KLDFGD
      IF (IGL.GT.0) GO TO 60
         IDFREC=PBUF(8)
         IDFUNT=KDEFNG
60    IF (IDFUNT.EQ.KLDFGD) LDFFLG=1
      CALL HGTRDN (IDFUNT,IDFREC,DBUF,LMAX,ISTAT)
      IF (ISTAT.NE.0) GO TO 300
      IF (DBUF(1).NE.0) GO TO 70
C
C  NO PREVIOUS LOCAL DEFAULT FOR GLOBAL
      CALL UMEMST (0,DBUF,LMAX)
      DBUF(1)=(PBUF(9)*14+22)/16
      DBUF(2)=1
      DBUF(3)=5
      CALL UMEMOV (PBUF(4),DBUF(4),2)
      DBUF(6)=-1
C
C  READ ANOTHER CARD
70    IPOINT=8
      IF (IF.LE.NFIELD) GO TO 90
80       NNCARD=NNCARD+1
         IF (NNCARD.GT.NCARD) GO TO 220
         CALL HCARDR (NNCARD,ISTAT)
         IF (ISTAT.NE.0) GO TO 300
         IF=1
90    IF (IF.GT.NFIELD) GO TO 80
C
C  GET NAME OF PARAMETER AND CHECK IT
      CALL HFEQLS (IF,DNAME,RIGHT,ISTRT)
      IF (ISTRT.EQ.IFSTRT(IF)) GO TO 120
      IPOINT=10
      NUM=PBUF(9)
      DO 100 I=1,NUM
         CALL UNAMCP (PBUF(IPOINT),DNAME,ISTAT)
         IF (ISTAT.EQ.0) GO TO 130
         IPOINT=IPOINT+2
100      CONTINUE
C
      CALL ULINE (LP,2)
      WRITE (LP,110) DNAME,NAME
110   FORMAT ('0**ERROR** PARAMETER ',2A4,' NOT FOUND IN PROCEDURE ',
     *    2A4,'.')
      IERR=1
      IF=IF+1
      GO TO 90
C
C  POSITIONAL VALUE
120   IPOINT=IPOINT+2
C
C  CHECK LENGTH OF STRING
130   LENGTH=IFSTOP(IF)-ISTRT+1
      IF (IBUF(ISTRT).NE.LQUOTE) GO TO 140
         ISTRT=ISTRT+1
         LENGTH=LENGTH-1
140   IF (LENGTH.LE.43) GO TO 160
         CALL ULINE (LP,2)
         WRITE (LP,150) IF
150   FORMAT ('0**ERROR** STRING IN FIELD ',I2,' MUST BE LESS THAN ',
     *       '44 CHARACTERS')
         IERR=1
         IF=IF+1
         GO TO 90
C
C  CHECK WHERE TO PUT DEFAULT
160   NUM=(IPOINT-8)/2
      IF (NUM.LE.PBUF(9)) GO TO 180
         CALL ULINE (LP,2)
         WRITE (LP,170) IF
170   FORMAT ('0**ERROR** ERROR IN POSITIONAL ASSIGNMENT FOR FIELD ',I2,
     *   '.')
         IERR=1
         IF=IF+1
         GO TO 90
180   IPOS=10+(NUM-1)*14
C
      IF (LDFFLG.EQ.0) GO TO 210
C
C  USING LOCAL DEFAULT FOR GLOBAL RECORD
      IPOS=10
      NUMDEF=DBUF(7)
      IF (NUMDEF.EQ.0) GO TO 200
      DO 190 I=1,NUMDEF
         IF (DBUF(IPOS-2).EQ.NUM) GO TO 210
         IPOS=IPOS+14
190      CONTINUE
C
C  NOT FOUND - PUT AT END
200   DBUF(7)=DBUF(7)+1
      DBUF(IPOS-2)=NUM
      DBUF(IPOS-1)=12
C
C  PUT THE STRING IN
210   CALL UMEMST (IBLNK,DBUF(IPOS),12)
      CALL HPTSTR (IBUF(ISTRT),LENGTH,DBUF(IPOS),ISTAT)
      IF (ISTAT.NE.0) GO TO 300
      IF=IF+1
      GO TO 90
C
220   IF (IERR.EQ.0) GO TO 240
         CALL ULINE (LP,2)
         WRITE (LP,230) NAME
230   FORMAT ('0**ERROR** DEFAULTS NOT SET FOR PROCEDURE ',2A4,'.')
         GO TO 320
C         
C  WRITE THE DEFAULT
240   CALL WVLRCD (IDFUNT,IDFREC,DBUF(1),DBUF,16,ISTAT)
      IF (ISTAT.EQ.0) GO TO 260
         CALL ULINE (LP,2)
         WRITE (LP,250)
250   FORMAT ('0**ERROR** UNABLE TO WRITE DEFAULT RECORD')
         IERR=1
         GO TO 220
C
C  PRINT IT
260   CALL ULINE (LP,2)
      WRITE (LP,270) NAME
270   FORMAT ('0**NOTE** NEW DEFAULTS SET FOR PROCEDURE ',2A4,'.')
      IF (IGL.LT.0) PBUF(1)=-PBUF(1)
      CALL HPPRCG (PBUF)
      GO TO 320
C
C   ERROR - PROCEURE HAS NO PARMATERS
280   CALL ULINE (LP,2)
      WRITE (LP,290) NAME
290   FORMAT ('0**ERROR** THERE ARE NO DEFAULTS FOR PROCEDURE ',2A4,'.')
      IERR=1
      GO TO 220
C
C  SYSTEM ERROR
300   CALL ULINE (LP,2)
      WRITE (LP,310)
310   FORMAT ('0**ERROR** SYSTEM ERROR.')
      IERR=1
      GO TO 220
C
320   IF (IHCLTR.GT.1) WRITE (IOGDB,330) NAME,IERR
330   FORMAT (' EXIT HDFLTP - NAME=',2A4,' IERR=',I2)
C
      RETURN
C
      END
