C MODULE HDFLTF
C-----------------------------------------------------------------------
C
      SUBROUTINE HDFLTF (IGL,IF,NAME,IPASS,NNCARD)
C
C          ROUTINE:  HDFLTF
C             VERSION:  1.0.0
C                DATE:  4-12-82
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE SETS DEFAULTS FOR FUNCTION DEFINITIONS.
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
C       NAME       A8    I     1    NAME OF FUNCTION
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
      INCLUDE 'hclcommon/hwords'
      INCLUDE 'hclcommon/hindx'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      PARAMETER (LMAX=2000)
      INTEGER FBUF(LMAX),DBUF(LMAX)
      INTEGER NAME(2),TNAME(2),IXBUF(4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hdfltf.f,v $
     . $',                                                             '
     .$Id: hdfltf.f,v 1.2 1998/04/07 12:54:06 page Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C
C***********************************************************************
C
C
C
      IF (IHCLTR.GT.1) WRITE (IOGDB,5)
5     FORMAT (' ENTER HDFLTF')
C
      LDFFLG=0
      IERR=0
C
C  GET THE FUNCTION RECORD
      ITYPE=2
      CALL HGTRCD (ITYPE,NAME,LMAX,FBUF,ISTAT)
      IF (ISTAT.EQ.0) GO TO 10
         IERR=1
         GO TO 230
C
C  CHECK THE PASSWORD
10    IF (ITYPE.LT.0.AND.IGL.EQ.1) GO TO 30
         IF (IPASS.EQ.FBUF(6)) GO TO 30
         CALL ULINE (LP,2)
         WRITE (LP,20) NAME
20    FORMAT ('0**ERROR** INVALID PASSWORD FOR FUNCTION ',2A4,'.')
         IERR=1
C
C  CHECK THE TYPE
30    IF (IGL.NE.-1.OR.ITYPE.NE.2) GO TO 50
         CALL ULINE (LP,2)
         WRITE (LP,40) NAME
40    FORMAT ('0**ERROR** FUNCTION ',2A4,' IS A LOCAL FUNCTION. ',
     *   '@SETGDFLT IS NOT VALID.')
         IERR=1
         GO TO 230
C
C  GET THE DEFAULT RECORD
50    IF (FBUF(9).EQ.0) GO TO 290
      IDFREC=FBUF(7)
      IDFUNT=KDEFNL
      IF (ITYPE.LT.0) IDFUNT=KLDFGD
      IF (IGL.GT.0) GO TO 60
        IDFREC=FBUF(8)
        IDFUNT=KDEFNG
60    IF (IDFUNT.EQ.KLDFGD) LDFFLG=1
      CALL HGTRDN (IDFUNT,IDFREC,DBUF,LMAX,ISTAT)
      IF (ISTAT.NE.0) GO TO 310
      IF (DBUF(1).NE.0) GO TO 70
C
C  NO PREVIOUS LOCAL DEFAULT FOR GLOBAL
      CALL UMEMST (0,DBUF,LMAX)
      DBUF(1)=(FBUF(9)*2+22)/16
      DBUF(2)=1
      DBUF(3)=5
      CALL UMEMOV (FBUF(4),DBUF(4),2)
      DBUF(6)=-2
C
C  READ ANOTHER CARD
70    IF (IF.LE.NFIELD) GO TO 90
80       NNCARD=NNCARD+1
         IF (NNCARD.GT.NCARD) GO TO 230
         CALL HCARDR (NNCARD,ISTAT)
         IF (ISTAT.NE.0) GO TO 310
         IF=1
90    IF (IF.GT.NFIELD) GO TO 80
C
C  GET TECHNIQUE NAME
      CALL HFLPRN (IFSTRT(IF),IFSTOP(IF),ISTRT)
      IEND=ISTRT-1
      IF (ISTRT.EQ.0) IEND=IFSTOP(IF)
      NUM=IEND-IFSTRT(IF)+1
      IF (NUM.LE.8) GO TO 110
         CALL ULINE (LP,2)
         WRITE (LP,100) IF
100   FORMAT ('0**ERROR** INVALID TECHNIQUE NAME IN FIELD ',2A4,'.')
         IERR=1
         IF=IF+1
         GO TO 90
110   TNAME(2)=IBLNK
      CALL UPACK1 (IBUF(IFSTRT(IF)),TNAME,NUM)
C
C  CHECK IF TECHNIQUE IS IN FUNCTION
      IPOS=10
      NUM=FBUF(9)
      DO 120 I=1,NUM
         IXREC=IABS(FBUF(IPOS))+HINDEX(2,3)-1
         IXUNIT=KINDXL
         IF (FBUF(IPOS).LT.0) IXUNIT=KINDXG
         CALL UREADT (IXUNIT,IXREC,IXBUF,ISTAT)
         IF (ISTAT.NE.0) GO TO 310
         CALL UNAMCP (TNAME,IXBUF,ISTAT)
         IF (ISTAT.EQ.0) GO TO 140
         IPOS=IPOS+1
120      CONTINUE
C
C  TECHNIQUE NOT FOUND IN RECORD
      CALL ULINE (LP,2)
      WRITE (LP,130) TNAME,NAME
130   FORMAT ('0**ERROR** TECHNIQUE ',2A4,' NOT FOUND IN FUNCTION ',2A4,
     *   '.')
      IERR=1
      IF=IF+1
      GO TO 90
C
C  GET THE DEFAULT VALUE
140   IDFLT=1
      IF (ISTRT.EQ.0) GO TO 180
      CALL HFRPRN (ISTRT,IFSTOP(IF),IEND)
      IF (IEND.NE.0) GO TO 160
         CALL ULINE (LP,2)
         WRITE (LP,150) IF
150   FORMAT ('0**ERROR** UNBALANCED PARENTHESIS IN FIELD ',I2,'.')
         IERR=1
         IF=IF+1
         GO TO 90
160   IDFLT=-1
      IF (IBUF(ISTRT+1).EQ.LETY) IDFLT=1
      IF (IBUF(ISTRT+1).EQ.LETN) IDFLT=0
      IF (IDFLT.GE.0) GO TO 180
C
C  PUT IN INTEGER VALUE
      ISTAT=0
      CALL UINTFX (IDFLT,ISTRT+1,IEND-1,ISTAT)
      IF (ISTAT.EQ.0) GO TO 180
         CALL ULINE (LP,2)
         WRITE (LP,170) IF
170   FORMAT ('0**ERROR** INVALID TECHNIQUE VALUE IN FIELD ',I2,'.')
         IERR=1
         IF=IF+1
         GO TO 90
C
C  SET POSITION FOR DEFAULT IN RECORD
180   IF (LDFFLG.EQ.1) GO TO 190
      IPOINT=(IPOS-10)*2+9
      GO TO 220
C
C  USING LOCAL DEFAULT FOR GLOBAL DEFINITION
190   IPOINT=9
      NUMDEF=DBUF(7)
      IF (NUMDEF.EQ.0) GO TO 210
      DO 200 I=1,NUMDEF
         IF (DBUF(IPOINT-1).EQ.FBUF(IPOS)) GO TO 220
         IPOINT=IPOINT+2
200      CONTINUE
C
C  NOT FOUND - PUT AT END
210   DBUF(7)=DBUF(7)+1
      DBUF(IPOINT-1)=FBUF(IPOS)
C
C   PUT THE DEFAULT IN RECORD
220   DBUF(IPOINT)=IDFLT
      IF=IF+1
      GO TO 90
C
230   IF (IERR.EQ.0) GO TO 250
         CALL ULINE (LP,2)
         WRITE (LP,240) NAME
240   FORMAT ('0**ERROR** DEFAULTS NOT SET FOR FUNCTION ',2A4,'.')
         GO TO 330
C         
C  WRITE THE DEFAULT
250   CALL WVLRCD (IDFUNT,IDFREC,DBUF(1),DBUF,16,ISTAT)
      IF (ISTAT.EQ.0) GO TO 270
         CALL ULINE (LP,2)
         WRITE (LP,260)
260   FORMAT ('0**ERROR** ERROR ENCOUNTERED WRITING DEFAULT RECORD.')
         IERR=1
         GO TO 230
C
C  PRINT IT
270   CALL ULINE (LP,2)
      WRITE (LP,280) NAME
280   FORMAT ('0**NOTE** NEW DEFAULTS SET FOR FUNCTION ',2A4,'.')
      IF (IGL.LT.0) FBUF(1)=-FBUF(1)
      CALL HPFUNG (FBUF)
      GO TO 330
C
C   ERROR - FUNCTION HAS NO TECHNIQUES
290   CALL ULINE (LP,2)
      WRITE (LP,300) NAME
300   FORMAT ('0**ERROR** THERE ARE NO DEFAULTS FOR FUNCTION ',2A4,'.')
      IERR=1
      GO TO 230
C
C  SYSTEM ERROR
310   CALL ULINE (LP,2)
      WRITE (LP,320)
320   FORMAT ('0**ERROR** SYSTEM ERROR')
      IERR=1
      GO TO 230
C
330   IF (IHCLTR.GT.1) WRITE (IOGDB,340) NAME,IERR
340   FORMAT (' EXIT HDFLTF - NAME=',2A4,' IERR=',I2)
C
      RETURN
C
      END
