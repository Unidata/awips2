C MODULE RPRDHT
C-----------------------------------------------------------------------
C
      SUBROUTINE RPRDHT (TSID,ITYPE,MAXX,IHEAD,NUMX,XBUF,FTSID,IFUT,
     *   ISTAT)
C
C  ROUTINE TO READ THE A TIME SERIES HEADER.
C
C  ARGUMENT LIST:
C
C       NAME      TYPE  I/O   DIM   DESCRIPTION
C       ------    ----  ---   ---   -----------
C       TSID      A8     I     1     TIME SERIES ID
C       ITYPE     A4     I     1     DATA TYPE CODE
C       MAXX      I      I     1     ROOM IN XBUF
C       IHEAD     I      O    22     TIME SERIES HEADER
C       NUMX      I      O     1     WORDS IN XBUF
C       XBUF      I     I/O    ?     THE XBUF
C       FTSID     A8     O     1     FUTURE TS ID (BLANK IF NONE)
C       ISTAT     I      O     1     STATUS INDICATOR
C                                      0=NORMAL RETURN
C                                      1=TS NOT FOUND
C                                      2=XBUF TOO SMALL
C                                      3=ID/TYPE ON FILE DONT MATCH
C                                           INPUT
C                                      5=SYSTEM ERROR
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/punits'
      INCLUDE 'prdcommon/pdatas'
      INCLUDE 'urcommon/urftbl'
      INCLUDE 'urcommon/urunts'
C
      CHARACTER*8 TSID,FTSID
      DIMENSION XBUF(1),IHEAD(22)
      DIMENSION IXBUF(4),ITBUF(32),IFARR(3)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/rprdht.f,v $
     . $',                                                             '
     .$Id: rprdht.f,v 1.3 2002/02/11 14:27:14 michaelo Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,*) 'ENTER RPRDHT'
C
      ISTAT=0
C      
      IXTYPE=ITYPE
      FTSID=' '
C
C  CHECK IF DELETING FUTURE TIME SERIES
      IF (IFUT.EQ.0) GO TO 10
C
C  CHECK IF VALID FUTURE TYPE
      CALL PFDTYP (IXTYPE,IND)
      IF (IND.EQ.0) GO TO 70
      INDX=0
      IF (IAMORD.EQ.0) INDX=DATFIL(7,IND)
      IF (IAMORD.EQ.1) INDX=IDATFL(7,IND)
      IF (INDX.LE.0) GO TO 70
      IF (IAMORD.EQ.0) IXTYPE=DATFIL(1,INDX)
      IF (IAMORD.EQ.1) IXTYPE=IDATFL(1,INDX)
C
C  CHECK IF VALID REGULAR TYPE
10    CALL PFDTYP (IXTYPE,INDX)
      IF (INDX.NE.0) GO TO 30
C
C  TIME SERIES NOT FOUND
20    IF (IPRDB.GT.0) WRITE (IOGDB,130) TSID,IXTYPE
      ISTAT=1
      GO TO 100
C
C  GET LOGICAL UNIT
30    IUNIT=0
      IF (IAMORD.EQ.0) IUNIT=DATFIL(2,INDX)
      IF (IAMORD.EQ.1) IUNIT=IDATFL(2,INDX)-KUPRDO
      IF (IPRDB.GT.0) WRITE (IOGDB,140) IAMORD,IUNIT
C
C  FIND THE TIME SERIES
      CALL PSERCH (TSID,IXTYPE,IFREE,IXREC,IXBUF)
      IF (IXREC.EQ.0) GO TO 20
C
C  TIME SERIES FOUND
      CALL RVLRCD (IUNIT,IXBUF(4),2,ITBUF,LRECLT,IERR)
      IF (IERR.NE.0) GO TO 80
C
C  CHECK THAT ID/TYPE MATCH THOSE JUST READ
      CALL UNAMCP (TSID,ITBUF(4),IMATCH)
      IF (IMATCH.NE.0) GO TO 90
      IF (IXTYPE.NE.ITBUF(6)) GO TO 90
C
C  EXPAND THE HEADER
      CALL PEXPBT (ITBUF,IHEAD)
C
C  CHECK FOR ROOM FOR XBUF
      NUMX=IHEAD(1)-LENHED
      IF (NUMX.EQ.0) GO TO 60
      IF (MAXX.EQ.0) GO TO 60
      IF (NUMX.LE.MAXX) GO TO 40
      IF (IPRDB.GT.0) WRITE (IOGDB,150) TSID,IXTYPE
      ISTAT=2
      GO TO 100
C
C  MOVE IN XBUF FROM SECOND RECORD
40    IROOM=LRECLT*2-LENHDC
      IXPOS=1
      NUM=NUMX
      IF (NUM.GT.IROOM) NUM=IROOM
      CALL UMEMOV (ITBUF(LENHDC+1),XBUF(IXPOS),NUM)
      IF (NUMX.LE.IROOM) GO TO 60
      IXPOS=IXPOS+NUM
      NUM=NUMX-IROOM
      IREC=IXBUF(4)+2
C
C  HAVE MORE XBUF TO DO
50    CALL UREADT (IUNIT,IREC,ITBUF,IERR)
      IF (IERR.NE.0) GO TO 80
      NUM2=LRECLT
      IF (NUM2.GT.NUM) NUM2=NUM
      CALL UMEMOV (ITBUF,XBUF(IXPOS),NUM2)
      NUM=NUM-NUM2
      IREC=IREC+1
      IXPOS=IXPOS+NUM2
      IF (NUM.NE.0) GO TO 50
C
C  GET FUTURE ID
60    IF (MAXX.EQ.0) NUMX=IXBUF(4)
      IF (IFUT.EQ.1) GO TO 100
      CALL PFDFID (IHEAD(15),IXTYPE,IFARR,IERR)
      IF (IERR.NE.0) GO TO 80      
      CALL UMEMOV (IFARR,FTSID,2)
      GO TO 100
C
70    IF (IPRDB.GT.0) WRITE (IOGDB,160) ITYPE
      ISTAT=1
      GO TO 100
C
80    IF (IPRDB.GT.0) WRITE (IOGDB,170)
      ISTAT=5
      GO TO 100
C
90    IF (IPRDB.GT.0) WRITE (IOGDB,180) TSID,IXTYPE,(ITBUF(J),J=4,6),
     *   IXREC
      ISTAT=3
C
100   IF (IPRDB.GT.0) WRITE (IOGDB,190) IHEAD
      IF (IPRDB.GT.0.AND.MAXX.NE.0) WRITE (IOGDB,110)
     * (XBUF(I),I=1,NUMX)
110   FORMAT (' XBUF =',(1X,20A4))
C
      IF (IPRTR.GT.0) WRITE (IOGDB,*) 'EXIT RPRDHT'
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
130   FORMAT ('0**ERROR** IN RPRDHT - TIME SERIES ',
     *   'FOR IDENTIFIER ',A,' ',
     *   'AND ',
     *   'TYPE ',A4,' ',
     *   'NOT FOUND.')
140   FORMAT (' IAMORD=',I2,3X,'IUNIT=',I3)
150   FORMAT ('0**ERROR** IN RPRDHT - XBUF TOO LARGE ',
     *   'FOR TIME SERIES IDENTIFIER ',A,' ',
     *   'FOR DATA TYPE ',A4,'.')
160   FORMAT ('0**ERROR** IN RPRDHT - DATA TYPE ',A4,' IS INVALID OR ',
     *   'HAS NO FUTURE DATA TYPE.')
170   FORMAT ('0**ERROR** IN RPRDHT - SYSTEM OR DAIO ERROR.')
180   FORMAT ('0**ERROR** REQUESTED TIME SERIES: ',A,1X,A4,' ',
     *      'DOES NOT MATCH TIME SERIES ON FILE: ',2A4,1X,A4 /
     *   11X,'PROCESSED DATA BASE INDEX RECORD IS ',I6)
c
c     DR17865 
cfan
c     Integer array can't be printed out in F format in Fortran 90 
cfan
cf190 FORMAT (' HEADER=',7I6,1X,4A4,2F6.2,I7,3I6 / 8X,5A4)
190   FORMAT (' HEADER=',7I6,1X,4A4,2I8,I7,3I6 / 8X,5A4)     !cfan10/2006
      END
