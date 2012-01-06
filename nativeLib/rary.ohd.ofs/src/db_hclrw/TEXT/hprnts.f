C MODULE HPRNTS
C-----------------------------------------------------------------------
C
      SUBROUTINE HPRNTS
C
C          ROUTINE:  HPRNTS
C             VERSION:  1.0.0
C                DATE:  8-26-81
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C  THIS ROUTINE DOES THE DUMP COMMAND FOR PROCEDURES, FUNCTIONS,
C  TECHNIQUES AND NAMED OPTIONS.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C        NONE
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
      INCLUDE 'hclcommon/hwords'
      INCLUDE 'hclcommon/hword2'
      INCLUDE 'hclcommon/hindx'
      INCLUDE 'hclcommon/hcomnd'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      PARAMETER (LKBUF=2000)
      DIMENSION KBUF(LKBUF)
      PARAMETER (LWORK=400)
      DIMENSION IWORK(LWORK)
      DIMENSION ITYPE(2),INDBUF(4),INAME(2)
      DIMENSION IDELET(2),IFNAM(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hprnts.f,v $
     . $',                                                             '
     .$Id: hprnts.f,v 1.2 1998/04/07 13:00:50 page Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA IDELET/4HDELE,4HTED /
C
C***********************************************************************
C
C
C
      IF (IHCLTR.GT.0) WRITE (IOGDB,390)
C
      IGDF=0
      IALL=0
      IFLD=3
      NUMCRD=1
C
C  GET CARD
      NNCARD=1
      CALL HCARDR (NNCARD,ISTAT)
      IF (ISTAT.NE.0) GO TO 370
      IF (NFIELD.EQ.2) GO TO 50
C
10    IF (NFIELD.LT.IFLD) GO TO 50
      INAME(2)=IBLNK
      ISTRT=IFSTRT(IFLD)
      NCARS=IFSTOP(IFLD)-ISTRT+1
      IF (NCARS.LE.8) GO TO 30
20    CALL ULINE (LP,2)
      WRITE (LP,400) IFLD
      IFLD=IFLD+1
      GO TO 10
C
C  CHECK FOR 'GLOBAL', 'LOCAL' OR 'ALL'
30    CALL UPACK1 (IBUF(ISTRT),INAME,NCARS)
      IF (NUMCRD.GT.2) GO TO 40
      IF (NUMCRD.EQ.1.AND.IFLD.GT.4) GO TO 40
      IF (NUMCRD.EQ.2.AND.IFLD.GT.1) GO TO 40
      CALL UNAMCP (INAME,LLOCAL,IMATCH)
      IF (IMATCH.EQ.0) THEN
         IGDF=1
         GO TO 70
         ENDIF
      IALL=2
      CALL UNAMCP (INAME,LGLOBL,IMATCH)
      IF (IMATCH.EQ.0) GO TO 70
      IF (INAME(1).EQ.LALL.AND.INAME(2).EQ.IBLNK) GO TO 60
C
40    IFLD=IFLD+1
      GO TO (300,310,350,360),KEYWRD
      CALL ULINE (LP,2)
      WRITE (LP,410) KEYWRD
      GO TO 10
C
C  GET NEXT CARD
50    NNCARD=NNCARD+1
      NUMCRD=NUMCRD+1
      IF (NNCARD.GT.NCARD) THEN
         IF (NUMCRD.EQ.2) THEN
            IF (IFLD.EQ.4.AND.IGDF.EQ.1) GO TO 60
            IF (IFLD.EQ.3) GO TO 60
            ENDIF
         GO TO 380
         ENDIF
      CALL HCARDR (NNCARD,ISTAT)
      IF (ISTAT.NE.0) GO TO 370
      IFLD=1
      GO TO 10
C
60    IALL=1
C
70    GO TO (80,140,200,260),KEYWRD
      CALL ULINE (LP,2)
      WRITE (LP,410) KEYWRD
      GO TO 380
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT ALL ENTRIES
C
C  PRINT PROCEDURE DEFINITIONS
C
80    ISTOP=0
      IF (IALL.EQ.0) GO TO 120
C
      CALL UPAGE (LP)
C
      ISUB=5
      IUNIT=KDEFNG
      IXUNIT=KINDXG
      CALL UMEMOV (LGLOBL,ITYPE,2)
C
90    IS=HINDEX(2,ISUB)
      IE=HINDEX(3,ISUB)
      IF (IE.LT.IS) GO TO 130
C
      CALL ULINE (LP,2)
      WRITE (LP,420) ITYPE,'PROCEDURES'
C
      DO 100 I=IS,IE
          CALL UREADT (IXUNIT,I,INDBUF,ISTAT)
          IF (ISTAT.NE.0) GO TO 370
          IREC=INDBUF(3)
          IF (IREC.LE.0) GO TO 370
          CALL UNAMCP (IDELET,INDBUF(1),IMATCH)
          IF (IMATCH.EQ.0) GO TO 100
          CALL HGTRDN (IUNIT,IREC,KBUF,LKBUF,ISTAT)
          IF (ISTAT.NE.0) GO TO 370
          IF (IGDF.EQ.1.AND.ISUB.EQ.6) KBUF(1)=-KBUF(1)
          CALL ULINEL (LP,NLINES,IRETRN)
          IF (IRETRN.EQ.1) THEN
             CALL UPAGE (LP)
             CALL ULINE (LP,2)
             WRITE (LP,420) ITYPE,'PROCEDURES'
             ENDIF
          CALL HPPRCG (KBUF)
100       CONTINUE
C
110   IF (IALL.EQ.2) GO TO 380
      IF (ISTOP.EQ.1) GO TO 380
C
120   IXUNIT=KINDXL
      IUNIT=KDEFNL
      ISUB=1
      ISTOP=1
      IF (IALL.EQ.1) CALL UPAGE (LP)
      CALL UMEMOV (LLOCAL,ITYPE,2)
      GO TO 90
C
130   CALL ULINE (LP,2)
      WRITE (LP,430) ITYPE,'PROCEDURES'
      GO TO 110
C
C          -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
C
C  PRINT FUNCTION DEFINITIONS
C
140   ISTOP=0
      IF (IALL.EQ.0) GO TO 180
C
      CALL UPAGE (LP)
C
      ISUB=6
      IUNIT=KDEFNG
      IXUNIT=KINDXG
      CALL UMEMOV (LGLOBL,ITYPE,2)
C
150   IS=HINDEX(2,ISUB)
      IE=HINDEX(3,ISUB)
      IF (IE.LT.IS) GO TO 190
C
      CALL ULINE (LP,2)
      WRITE (LP,420) ITYPE,'FUNCTIONS'
C
      DO 160 I=IS,IE
          CALL UREADT (IXUNIT,I,INDBUF,ISTAT)
          IF (ISTAT.NE.0) GO TO 370
          IREC=INDBUF(3)
          IF (IREC.LE.0) GO TO 370
          CALL UNAMCP (IDELET,INDBUF(1),IMATCH)
          IF (IMATCH.EQ.0) GO TO 160
          CALL HGTRDN (IUNIT,IREC,KBUF,LKBUF,ISTAT)
          IF (ISTAT.NE.0) GO TO 370
          IF (IGDF.EQ.1.AND.ISUB.EQ.6) KBUF(1)=-KBUF(1)
          NLINES=2
          CALL ULINEL (LP,NLINES,IRETRN)
          IF (IRETRN.EQ.1) THEN
             CALL UPAGE (LP)
             CALL ULINE (LP,2)
             WRITE (LP,420) ITYPE,'FUNCTIONS'
             ENDIF
          CALL HPFUNG (KBUF)
160       CONTINUE
C
170   IF (IALL.EQ.2) GO TO 380
      IF (ISTOP.EQ.1) GO TO 380
C
180   IXUNIT=KINDXL
      IUNIT=KDEFNL
      ISUB=2
      ISTOP=1
      IF (IALL.EQ.1) CALL UPAGE (LP)
      CALL UMEMOV (LLOCAL,ITYPE,2)
      GO TO 150
C
190   CALL ULINE (LP,2)
      WRITE (LP,430) ITYPE,'FUNCTIONS'
      GO TO 170
C
C          -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
C
C  PRINT TECHNIQUE DEFINITIONS
C
200   ISTOP=0
      IF (IALL.EQ.0) GO TO 240
C
      CALL UPAGE (LP)
C
      ISUB=7
      IUNIT=KDEFNG
      IXUNIT=KINDXG
      CALL UMEMOV (LGLOBL,ITYPE,2)
C
210   IS=HINDEX(2,ISUB)
      IE=HINDEX(3,ISUB)
      IF (IE.LT.IS) GO TO 250
C
      CALL ULINE (LP,2)
      WRITE (LP,420) ITYPE,'TECHNIQUES'
C
      DO 220 I=IS,IE
          CALL UREADT (IXUNIT,I,INDBUF,ISTAT)
          IF (ISTAT.NE.0) GO TO 370
          IREC=INDBUF(3)
          IF (IREC.LE.0) GO TO 370
          CALL UNAMCP (IDELET,INDBUF(1),IMATCH)
          IF (IMATCH.EQ.0) GO TO 220
          CALL HGTRDN (IUNIT,IREC,KBUF,LKBUF,ISTAT)
          IF (ISTAT.NE.0) GO TO 370
          IF (IGDF.EQ.1.AND.ISUB.EQ.7) KBUF(1)=-KBUF(1)
          NLINES=2
          CALL ULINEL (LP,NLINES,IRETRN)
          IF (IRETRN.EQ.1) THEN
             CALL UPAGE (LP)
             CALL ULINE (LP,2)
             WRITE (LP,420) ITYPE,'TECHNIQUES'
             ENDIF
          CALL HPTECG (KBUF)
220       CONTINUE
C
230   IF (IALL.EQ.2) GO TO 380
      IF (ISTOP.EQ.1) GO TO 380
C
240   IXUNIT=KINDXL
      IUNIT=KDEFNL
      ISUB=3
      ISTOP=1
      IF (IALL.EQ.1) CALL UPAGE (LP)
      CALL UMEMOV (LLOCAL,ITYPE,2)
      GO TO 210
C
250   CALL ULINE (LP,2)
      WRITE (LP,430) ITYPE,'TECHNIQUES'
      GO TO 230
C
C          -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
C
C  PRINT NAMED OPTIONS DEFINITIONS
C
260   IF (IALL.EQ.2) GO TO 280
C
      IS=HINDEX(2,4)
      IE=HINDEX(3,4)
      IF (IE.LT.IS) GO TO 290
C
      CALL ULINE (LP,2)
      WRITE (LP,440)
C
      DO 270 I=IS,IE
          CALL UREADT (KINDXL,I,INDBUF,ISTAT)
          IF (ISTAT.NE.0) GO TO 370
          IREC=INDBUF(3)
          IF (IREC.LE.0) GO TO 370
          CALL UNAMCP (IDELET,INDBUF(1),IMATCH)
          IF (IMATCH.EQ.0) GO TO 270
          CALL HGTRDN (KDEFNL,IREC,KBUF,LKBUF,ISTAT)
          IF (ISTAT.NE.0) GO TO 370
          CALL HPOPTG (KBUF,IWORK,LWORK)
270       CONTINUE
C
      GO TO 380
C
280   CALL ULINE (LP,2)
      WRITE (LP,450)
      GO TO 380
C
290   CALL ULINE (LP,2)
      WRITE (LP,460)
      GO TO 380
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT FOR SPECIFIED NAMES
C
C  PRINT PROCEDURE DEFINITIONS
300   ITYP=1
      CALL HGTRCD (ITYP,INAME,LKBUF,KBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 10
      IF (IGDF.EQ.1.AND.ITYP.LT.0) KBUF(1)=-KBUF(1)
      CALL HPPRCG (KBUF)
      GO TO 10
C
C  PRINT FUNCTION DEFINITIONS
C  CHECK IF NUMBER OR NAME
310   IF (NCARS.NE.6) GO TO 330
      CALL UPACK1 (IBUF(ISTRT),IFUN,3)
      IF (IFUN.NE.LFUN) GO TO 330
      CALL UCKINT (IBUF(ISTRT+3),1,3,ISTAT)
      IF (ISTAT.NE.0) GO TO 330
      CALL UNUMIC (IBUF(ISTRT+3),1,3,IFNUM)
C  GET THE RECORD FROM FUNCTION NUMBER
      IF (IFNUM.LT.1.OR.IFNUM.GT.999) GO TO 20
      CALL HFDFNM (IFNUM,IFNAM,IFREC)
      IF (IFREC.EQ.0) GO TO 320
      ITYP=2
      CALL HGTRCD (ITYP,IFNAM,LKBUF,KBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 370
      GO TO 340
C
320   IFLD=IFLD-1
      CALL ULINE (LP,2)
      WRITE (LP,470) IFLD,IFUN,IFNUM
      IFLD=IFLD+1
      GO TO 10
C
C  GET THE RECORD FROM FUNCTION NAME
330   ITYP=2
      CALL HGTRCD (ITYP,INAME,LKBUF,KBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 10
C
C  PRINT FUNCTION
340   IF (IGDF.EQ.1.AND.ITYP.LT.0) KBUF(1)=-KBUF(1)
      CALL HPFUNG (KBUF)
      GO TO 10
C
C  PRINT TECHNIQUE DEFINITIONS
350   ITYP=3
      CALL HGTRCD (ITYP,INAME,LKBUF,KBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 10
      IF (IGDF.EQ.1.AND.ITYP.LT.0) KBUF(1)=-KBUF(1)
      CALL HPTECG (KBUF)
      GO TO 10
C
C  PRINT NAMED OPTIONS
360   ITYP=4
      CALL HGTRCD (ITYP,INAME,LKBUF,KBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 10
      CALL HPOPTG (KBUF,IWORK,LWORK)
      GO TO 10
C
370   CALL ULINE (LP,2)
      WRITE (LP,480)
C
380   IF (IHCLTR.GT.0) WRITE (IOGDB,490)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
390   FORMAT (' ENTER HPRNTS')
400   FORMAT ('0**ERROR** IN HPRNTS - INVALID NAME IN FIELD ',I2,'.')
410   FORMAT ('0**ERROR** IN HPRNTS - ',I2,' IS AN INVALID VALUE OF ',
     *   'VARIABLE ''KEYWORD''.')
420   FORMAT ('0',30X,'- ',2A4,' ',A,' -')
430   FORMAT ('0**NOTE** NO ',2A4,' ',A,' FOUND.')
440   FORMAT ('0',30X,'- NAMED OPTIONS -')
450   FORMAT ('0**NOTE** NO GLOBAL NAMED OPTIONS.')
460   FORMAT ('0**NOTE** NO NAMED OPTIONS FOUND.')
470   FORMAT ('0**ERROR** IN HPRNTS - FUNCTION IN FIELD ',I2,
     *   ' NOT FOUND  ',A3,I3,'.')
480   FORMAT ('0**ERROR** IN HPRNTS - SYSTEM ERROR.')
490   FORMAT (' EXIT HPRNTS')
C
      END
