C MODULE WPRDCT
C-----------------------------------------------------------------------
C
      SUBROUTINE WPRDCT (ITSID,ITYPE,IUNITS,RLOCT,DESCRP,ITSIDF,
     *   NX,XBUF,LWKBUF,IWKBUF,IRECR,IFUT,ISTAT)
C
C          ROUTINE:  WPRDC
C             VERSION:  1.0.0
C                DATE:  2-15-82
C              AUTHOR:  SONJA R SIEGEL
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE WILL CHANGE A TIME SERIES HEADER.
C
C    ANY ITEMS THAT ARE ZERO OR BLANK,WILL NOT BE CHANGED.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ITSID      A8    I     1    TIME SERIES IDENTIFIER
C       ITYPE      I     I     1    DATA TYPE
C       IUNITS     I     I     1    UNITS CODE OR BLANK IF NO CHANGE
C       RLOCT      R     I     2    NEW LAT/LONG OR ZERO IF NO CHANGE
C       DESCRP     A20   I     1    NEW DESCRIPTION OR BLANK IF NO
C                                     CHANGE
C       ITSIDF     IA8   I     1    NEW FUTURE ID OR BLANK IF NO CHANGE
C       NX         I     I     1    NEW NUMBER OF XBUF WORDS OR -1 IF
C                                     NO CHANGE
C       XBUF       R     I     ?    XBUF ARRAY
C       LWKBUF     I     I     1    NUMBER OF WORDS IN WORK BUFFER
C       IWKBUF     I    I/O    ?    WORK BUFFER FOR TIME SERIES
C       IRECR      I     O     1    RECORD NUMBER OF REGULAR TIME SERIES
C                                     HEADER RECORD IN FILE
C       IFUT       I     I     1    FUTURE TIME SERIES INDICATOR
C       ISTAT      I     O     1    STATUS
C                                     0=OK
C                                     1-7 ERRORS
C                                     8=NO CHANGES NEED TO BE MADE
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/pdatas'
      INCLUDE 'prdcommon/pdftbl'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      CHARACTER*8 ITSID,ITSIDF
      CHARACTER*20 DESCRP,DESCRPO
C
      DIMENSION IWKBUF(LWKBUF)
      DIMENSION RLOCT(2),XBUF(1)
      DIMENSION IHEADF(22),XB(1)
C
C
C***********************************************************************
C
C          DATA:
C
C
C***********************************************************************
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/wprdct.f,v $
     . $',                                                             '
     .$Id: wprdct.f,v 1.2 1998/04/07 11:47:40 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER WPRDCT')
C
      ISTAT=0
C
      IXTYPE=ITYPE
C
C  CHECK IF FUTURE TIME SERIES
      IF (IFUT.EQ.1) THEN
         CALL PFFTYP (ITYPE,IXTYPE,IXREG,IXFUT)
         IF (IXFUT.EQ.0) THEN
            IF (IPRDB.GT.0) WRITE (IOGDB,20) IXTYPE
20    FORMAT ('0**ERROR** IN WPRDC - ',A4,' IS AN INVALID DATA TYPE.')
            ISTAT=1
            GO TO 150
            ENDIF
         ENDIF
C
C  GET THE TIME SERIES
      CALL PGETTS (ITSID,IXTYPE,LWKBUF,IWKBUF,IREC,ISTAT)
      IF (ISTAT.NE.0) THEN
         IF (IPRDB.GT.0) WRITE (IOGDB,30) ITSID,IXTYPE
30    FORMAT ('0**ERROR** IN WPRDC - TIME SERIES FOR ID ',A,
     *   ' AND DATA TYPE ',A4,' DOES NOT EXIST.')
         ISTAT=1
         GO TO 150
         ENDIF
      IRECR=IREC
C
      ICHANGE=0
C
C  CHECK IF TO CHANGE LATITUDE
      IF (RLOCT(1).NE.0.0) THEN
         CALL UMEMOV (IWKBUF(12),CHKVAL,1)
         IF (RLOCT(1).NE.CHKVAL) THEN
            CALL UMEMOV (RLOCT(1),IWKBUF(12),1)
            ICHANGE=1
            ENDIF
         ENDIF
C
C  CHECK IF TO CHANGE LONGITUDE
      IF (RLOCT(2).NE.0.0) THEN
         CALL UMEMOV (IWKBUF(13),CHKVAL,1)
         IF (RLOCT(2).NE.CHKVAL) THEN
            CALL UMEMOV (RLOCT(2),IWKBUF(13),1)
            ICHANGE=1
            ENDIF
         ENDIF
C
C  CHECK IF TO CHANGE DESCRIPTION
      IF (DESCRP.NE.' ') THEN
         CALL UMEMOV (IWKBUF(18),DESCRPO,5)
         IF (DESCRP.NE.DESCRPO) THEN
            CALL UMEMOV (DESCRP,IWKBUF(18),5)
            ICHANGE=1
            ENDIF
         ENDIF
C
      LENDAT=IWKBUF(3)*IWKBUF(5)
C
C  CHECK IF TO CHANGE UNITS
      IF (IUNITS.NE.IBLNK) THEN
         IF (IUNITS.NE.IWKBUF(11)) THEN
C        CHECK FOR VALID UNITS CODE
            CALL UDUCNV (IWKBUF(11),IUNITS,2,1,CONVF1,CONVF2,ISTAT)
            IF (ISTAT.NE.0) GO TO 150
C        CONVERT DATA
            IPDATA=IWKBUF(6)
            CALL UDUCNN (1,ZAPR,ISTAT)
            CALL UDUCNV (IWKBUF(11),IUNITS,1,LENDAT,IWKBUF(IPDATA),
     *         IWKBUF(IPDATA),ISTAT)
            IF (ISTAT.NE.0) GO TO 150
            IWKBUF(11)=IUNITS
            ICHANGE=1
            ENDIF
         ENDIF
C
C  CHECK IF TO CHANGE FUTURE TIME SERIES ID
      IF (ITSIDF.NE.' '.AND.IFUT.EQ.0) THEN
C     CHECK IF TIME SERIES HAS A FUTURE TIME SERIES
         IF (IWKBUF(15).EQ.0) THEN
            IF (IPRDB.GT.0) WRITE (IOGDB,40)
40    FORMAT (' **ERROR** IN WPRDC - TIME SERIES DOES NOT HAVE ',
     *   'SEPARATE FUTURE DATA.')
            ISTAT=3
            GO TO 150
            ENDIF
C     CHECK IF NEW FUTURE TIME SERIES EXISTS
         MX=0
         CALL RPRDFH (ITSIDF,ITYPE,MX,IHEADF,IRECF,XB,ISTAT)
         IF (ISTAT.NE.0) THEN
         IF (IPRDB.GT.0) WRITE (IOGDB,50) ITSIDF,IXTYPE
50    FORMAT (' **ERROR** IN WPRDC - FUTURE TIME SERIES FOR ID ',A,
     *   ' AND DATA TYPE ',A4,' DOES NOT EXIST.')
            ISTAT=4
            GO TO 150
            ENDIF
C     CHECK NVALS AND DATA TIME INTERVAL
         IF (IWKBUF(3).NE.IHEADF(3)) THEN
            IF (IPRDB.GT.0) WRITE (IOGDB,60) IHEADF(3),IWKBUF(3)
60    FORMAT (' **ERROR** IN WPRDC - FUTURE TIME SERIES NVAL',I4,' ',
     *  'DOES NOT MATCH ',I4,'.')
            ISTAT=5
            GO TO 150
            ENDIF
         IF (MOD(IWKBUF(2),IHEADF(2)).NE.0) THEN
            IF (IPRDB.GT.0) WRITE (IOGDB,70) IWKBUF(2),IHEADF(2)
70    FORMAT (' **ERROR** IN WPRDC - DATA TIME INTERVAL OF ',
     *   'REQULAR TIME SERIES (',I2,') NOT COMPATIBLE WITH THAT OF ',
     *   'FUTURE TIME SERIES (',I2,').')
            ISTAT=6
            GO TO 150
            ENDIF
         ICHANGE=1
C     SET RECORD NUMBER OF FUTURE TIME SERIES
         IWKBUF(15)=IRECF
         ENDIF
C
C  CHECK IF TO CHANGE XBUF
      IMOVFL=0
      IF (NX.NE.-1) THEN
C     CHECK IF NUMBER OF WORDS CHANGED
         NXOLD=IWKBUF(1)-LENHED
         IF (NX.EQ.NXOLD) GO TO 120
         IF (NX.GT.NXOLD) GO TO 80
C     NX IS SMALLER - MOVE DATA TO LEFT - FIRST MOVE IN NEW XBUF
         IF (NX.GT.0) CALL UMEMOV (XBUF,IWKBUF(LENHED+1),NX)
C     MOVE UP DATA
         ITO=LENHED+NX+1
         IFROM=IWKBUF(6)
         CALL UMEMOV (IWKBUF(IFROM),IWKBUF(ITO),LENDAT)
C     UPDATE POINTERS
         IWKBUF(1)=LENHED+NX
         IWKBUF(6)=IWKBUF(1)+1
         IF (IWKBUF(7).NE.0) IWKBUF(7)=IWKBUF(7)-(NXOLD-NX)
         ICHANGE=1
         GO TO 130
C     NX IS LARGER - MOVE DATA TO RIGHT - CHECK FOR ENOUGH ROOM
80       NEED=IWKBUF(4)+NX+LENHED
         IF (NEED.GT.LWKBUF) THEN
            IF (IPRDB.GT.0) WRITE (IOGDB,90) NEED
90    FORMAT (' **ERROR** IN WPRDC - WORK BUFFER TOO SMALL. ',I5,
     *   ' WORDS NEEDED.')
            ISTAT=7
            GO TO 150
            ENDIF
C     CHECK NUMBER OF RECORDS
         NROLD=(IWKBUF(1)+IWKBUF(4)+LRECLT-1)/LRECLT
         NRNEW=(NEED+LRECLT-1)/LRECLT
         IF (NRNEW.GT.NROLD) IMOVFL=1
         IEND=LENDAT+IWKBUF(6)-1
C     UPDATE POINTERS
         IWKBUF(1)=LENHED+NX
         IWKBUF(6)=IWKBUF(1)+1
         IF (IWKBUF(7).NE.0) IWKBUF(7)=IWKBUF(7)+(NX-NXOLD)
C     COMPUTE NEW END OF DATA
         NEWEND=IWKBUF(1)+LENDAT
         IF (IPRDB.GT.0) WRITE (IOGDB,100) NEWEND,IEND,LENDAT
100   FORMAT (' MOVING DATA TO RIGHT, NEWEND=',I4,' IEND=',I4,
     *   ' LENDAT=',I4)
         DO 110 I=1,LENDAT
            IWKBUF(NEWEND)=IWKBUF(IEND)
            NEWEND=NEWEND-1
            IEND=IEND-1
110         CONTINUE
         ICHANGE=1
C     MOVE IN NEW XBUF
120      IF (NX.GT.0) THEN
            CALL UMEMOV (XBUF,IWKBUF(LENHED+1),NX)
            ICHANGE=1
            ENDIF
130      ENDIF
C
      IF (ICHANGE.EQ.0) THEN
         ISTAT=8
         GO TO 150
         ENDIF
C
C  GET LOGICAL UNIT
      CALL PFDTYP (IXTYPE,IX)
      LUNIT=DATFIL(2,IX)
      IF (IMOVFL.EQ.0) GO TO 140
         CALL PMOVTS (IREC,IX,IFUT,NRNEW,LUNIT,ITSID,IXTYPE,ISTAT)
         IRECR=IREC
         IWKBUF(17)=0
         IF (ISTAT.NE.0) GO TO 150
140   CALL WTSRCD (IREC,LUNIT,IWKBUF,ISTAT)
      IF (ISTAT.EQ.0) GO TO 150
C
150   IF (IPRTR.GT.0) WRITE (IOGDB,160)
160   FORMAT (' *** EXIT WPRDCT')
C
      RETURN
C
      END
