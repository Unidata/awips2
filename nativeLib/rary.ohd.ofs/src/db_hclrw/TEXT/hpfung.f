C MODULE HPFUNG
C-----------------------------------------------------------------------
C          ROUTINE:  HPFUNG
C             VERSION:  1.0.0
C                DATE:  8-25-81
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C***********************************************************************
C          DESCRIPTION:
C
C   ROUTINE TO PRINT FUNCTION DEFINITIONS.
C***********************************************************************
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       KBUF       I     I     16    ARRAY CONTAINING FUNCTION RECORD
C***********************************************************************
      SUBROUTINE HPFUNG (KBUF)
C
      INCLUDE 'udebug'
      INCLUDE 'uio'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hwords'
      INCLUDE 'hclcommon/hword2'
      INCLUDE 'hclcommon/hindx'
C
      INTEGER FPAS,FNUM,FNAME(2),FTYPE(2)
      INTEGER FTECTP(2),FTECNM(2),FTECPT,FTECDF
      DIMENSION KBUF(16),INDBUF(4),IWHAT(2)
      PARAMETER (LLBUF=600,LLBUF2=600)
      DIMENSION LBUF(LLBUF),LBUF2(LLBUF2)
      PARAMETER (LNBUF=512)
      DIMENSION NBUF(LNBUF)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hpfung.f,v $
     . $',                                                             '
     .$Id: hpfung.f,v 1.3 1998/07/06 11:33:15 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IHCLTR.GT.1) WRITE (IOGDB,10)
10    FORMAT (' ENTER HPFUNG')
C
      LGFLAG=0
      LDF=0
C
      CALL UMEMOV (KBUF(4),FNAME,2)
      CALL UMEMOV (LLOCAL,FTYPE,2)
      IF (KBUF(3).GT.0) LGFLAG=1
      IF (LGFLAG.EQ.0) CALL UMEMOV (LGLOBL,FTYPE,2)
      FPAS=KBUF(6)
      IF (FPAS.EQ.IBLNK) FPAS=LNONE
      FNUM=KBUF(2)
C
      CALL ULINE (LP,2)
      WRITE (LP,15)
15    FORMAT ('0',132('-'))
C
      CALL ULINE (LP,2)
      WRITE (LP,40) FNAME,FTYPE,FPAS,FNUM
40    FORMAT ('0',
     *   'FUNCTION = ',2A4,3X,
     *   'TYPE = ',A4,A3,3X,
     *   'PASSWORD = ',A4,3X,
     *   'INTERNAL NUMBER = ',I3.3)
C
C  CHECK IF THERE ARE ANY TECHNIQUES
      NUM=KBUF(9)
      IF (NUM.EQ.0) GO TO 190
C
C  GET THE DEFAULT
      LPOS=9
      IREC=KBUF(7)
      IUNIT=KDEFNL
      IF (LGFLAG.EQ.1) GO TO 80
         IREC=KBUF(8)
         IUNIT=KDEFNG
80    IF (IREC.LE.0) GO TO 250
      CALL HGTRDN (IUNIT,IREC,LBUF,LLBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 250
      CALL UNAMCP (LBUF(4),FNAME,ISTAT)
      IF (ISTAT.NE.0) GO TO 250
      IF (LGFLAG.EQ.1.OR.KBUF(1).LT.0) GO TO 90
      IREC=KBUF(7)
      IF (IREC.LE.0) GO TO 250
      CALL HGTRDN (KLDFGD,IREC,LBUF2,LLBUF2,ISTAT)
      IF (ISTAT.NE.0) GO TO 250
      IF (LBUF2(1).EQ.0) GO TO 90
      CALL UNAMCP (LBUF2(4),FNAME,ISTAT)
      IF (ISTAT.NE.0) GO TO 250
      LDF=1
C
C  PRINT THE HEADING
90    CALL ULINE (LP,3)
      WRITE (LP,100)
100   FORMAT ('0',15X,'TECHNIQUE',3X,'TECHNIQUE TYPE',3X,'DEFAULT',3X,
     *     'DEFAULT TYPE',3X,'ARG ARRAY LENGTH' /
     *     16X,9('-'),3X,14('-'),3X,7('-'),3X,12('-'),3X,16('-'))
C
C  PRINT TECHNIQUES     
      DO 180 I=1,NUM
          FTECPT=KBUF(9+I)
          IF (FTECPT.LT.0) GO TO 150
C     LOCAL TECHNIQUE
          ISUB=3
          IXUNIT=KINDXL
          IUNIT=KDEFNL
          CALL UMEMOV (LLOCAL,FTECTP,2)
110       IF (FTECPT.NE.LBUF(LPOS-1)) GO TO 250
          IREC=HINDEX(2,ISUB)+IABS(FTECPT)-1
          IF (IREC.LT.2005.OR.IREC.GT.3004) GO TO 250
          CALL UREADT (IXUNIT,IREC,INDBUF,ISTAT)
          IF (ISTAT.NE.0) GO TO 230
          CALL UMEMOV (INDBUF,FTECNM,2)
          CALL HPFLDF (LDF,LGFLAG,LBUF(LPOS - 1),LBUF2,IWHAT)
          FTECDF=LBUF(LPOS)
          LPOS=LPOS+2
          FTECPT=IABS(FTECPT)
          CALL HGTRDN (IUNIT,INDBUF(3),NBUF,LNBUF,ISTAT)
          IF (ISTAT.NE.0) GO TO 230
          CALL HGARSZ (NBUF,ISIZE)
          IF (FTECDF.LT.0.OR.FTECDF.GT.1) GO TO 130
          IF (FTECDF.EQ.0) ITECDL=LNO
          IF (FTECDF.EQ.1) ITECDL=LYES
          CALL ULINE (LP,1)
          WRITE (LP,120) I,FTECNM,FTECTP,ITECDL,FTECDF,IWHAT,ISIZE
120       FORMAT (13X,I2,1X,2A4,8X,2A4,5X,A4,2X,I1,6X,2A4,8X,I3)
          GO TO 160
130       CALL ULINE (LP,1)
          WRITE (LP,140) I,FTECNM,FTECTP,FTECDF,IWHAT,ISIZE
140       FORMAT (13X,I2,1X,2A4,8X,2A4,5X,I7,6X,2A4,8X,I3)
          GO TO 160
C     GLOBAL TECHNIQUE
150       ISUB=7
          IXUNIT=KINDXG
          IUNIT=KDEFNG
          CALL UMEMOV (LGLOBL,FTECTP,2)
          GO TO 110
160       IF (IHCLDB.GT.1) WRITE (IOGDB,170) FTECPT
170       FORMAT (26X,'TECHNIQUE INDEX = ',I3)
180       CONTINUE
      GO TO 280
C
C  NO TECHNIQUES
190   CALL ULINE (LP,2)
      WRITE (LP,200)
200   FORMAT ('0',15X,'NO TECHNIQUES')
      GO TO 280
C
230   CALL ULINE (LP,2)
      WRITE (LP,240)
240   FORMAT ('0**ERROR** IN HPFUNG - SYSTEM ERROR')
      GO TO 280
C
250   CALL ULINE (LP,4)
      WRITE (LP,260) INDBUF,KBUF
260   FORMAT ('0**ERROR** IN HPFUNG - SYSTEM ERROR' / ' INDEX RECORD ',
     *     2A4,2I5 / ' FUNCTION RECORD ',3I4,1X,3A4,(1X,10I6))
      CALL ULINE (LP,2)
      WRITE (LP,270) LBUF
270   FORMAT (' LBUF=',3I4,1X,2A4 / (1X,20I5))
C
280   IF (IHCLTR.GT.1) WRITE (IOGDB,290) NUM
290   FORMAT (' EXIT HPFUNG - NUMBER OF TECHNIQUES=',I2)
C
      RETURN
C
      END
