C MEMBER URIPDB
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 05/12/95.13:00:39 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE URIPDB (IINIT)
C
C
C          ROUTINE:  URIPDB
C
C             VERSION:  1.0.0
C
C                DATE:  2-25-85
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C  THIS ROUTINE WILL INITIALIZE THE NEW PREPROCESSOR DATA
C  FILES BY COPYING THE CONTROL RECORDS FROM THE OLD FILES AND ZEROING
C  SPECIFIC CONTROL VARIABLES; OR BY ONLY ZEROING SPECIFIC CONTROL
C  VARIABLES.
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'pdbcommon/pdbdta'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdrrsc'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdddfc'
      INCLUDE 'urcommon/urpddt'
      INCLUDE 'urcommon/urpddd'
      INCLUDE 'urcommon/ursifc'
      INCLUDE 'urcommon/urrrsc'
      INCLUDE 'urcommon/urhshc'
      INCLUDE 'urcommon/urhshi'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'urcommon/urcdta'
C
C***********************************************************************
C
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      CHARACTER*4 XTYPE
      INTEGER*2 IDBUF(32)
C
      DIMENSION IRRSAR(16),IFPBUF(16),IXARR(16)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_reorder/RCS/uripdb.f,v $
     . $',                                                             '
     .$Id: uripdb.f,v 1.2 1996/07/12 17:31:20 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C***********************************************************************
C
C
      IF (IPDTR.GT.0) CALL SULINE (IOGDB,1)
      IF (IPDTR.GT.0) WRITE (IOGDB,210)
C
      CALL SULINE (LP,2)
      IF (IINIT.EQ.1) WRITE (LP,220)
      IF (IINIT.EQ.-1) WRITE (LP,230)
C
      LRCPD2=LRCPDD*2
C
C  COPY COMMONS TO NEW COMMONS
      IF (IINIT.EQ.-1) CALL UMEMOV (NWDCTL,NWCTL,16)
      LTSIFR=ISIFRC
      IF (IPDDB.GT.0) CALL SULINE (IOGDB,1)
      IF (IPDDB.GT.0) WRITE (IOGDB,240) NWCTL,LRLURD,LRLURS,LRLURI,
     *    MPDTYP,NPDTYP,IPTDTP,IHASHR,NH8CHR,NHINRC,ISIFRC,MAXSIF,
     *    LTSIFR,MAXDOD,MXDDF,NMDDF
C
C  RRS CONTROL RECORD
      IF (IINIT.EQ.-1) CALL UMEMOV (MXRRSF,MAXRSF,16)
      LRRSXR=1
      LFREEN=LFREE1
      CALL UMEMST (0,MXSIZE,3)
      IF (IPDDB.GT.0) CALL SULINE (IOGDB,1)
      IF (IPDDB.GT.0) WRITE (IOGDB,250) MAXRSF,LRRSXR,LFREE1,LFREEN,
     *   IFREEL,IUFREE,MAXFRC,MXSIZE,NUMSTA,INUSFL,LPUSER(1),LPUSER(2)
C
C  DAILY DATA FILE CONTROL RECORDS
      DO 20 I=1,NMDDF
         IF (IINIT.EQ.1) GO TO 10
            JPDDFC(1,I)=IPDDFC(1,I)
            JPDDFC(2,I)=IPDDFC(2,I)
10       JPDDFC(3,I)=0
         JPDDFC(4,I)=0
20       CONTINUE
C
C  DAILY DATA TYPE DIRECTORY
      DO 30 I=1,NPDTYP
         IF (IINIT.EQ.-1) CALL UMEMOV (IDDTDR(2,I),JDDTDR(2,I),14)
         JDDTDR(8,I)=0
         JDDTDR(9,I)=0
         JDDTDR(10,I)=0
         JDDTDR(11,I)=0
         JDDTDR(12,I)=0
         JDDTDR(13,I)=0
         JDDTDR(17,I)=0
         JDDTDR(18,I)=0
         JDDTDR(19,I)=0
30       CONTINUE
C
C  INITIALIZE SIF RECORDS
      CALL UMEMST (0,IXARR,LRLURI)
      DO 40 IREC=1,MAXSIF
         CALL UWRITT (KURSIF,IREC,IXARR,ISTAT)
         IF (ISTAT.NE.0) GO TO 170
40       CONTINUE
      CALL SULINE (LP,2)
      WRITE (LP,260) MAXSIF,KURSIF
C
      IF (IINIT.NE.1) GO TO 70
C
C  INITIALIZE CHARACTER INDEX
      NPOS=MURHSC*2
      DO 50 I=1,NPOS
         IURHSC(I)=0
50       CONTINUE
      CALL SULINE (LP,2)
      WRITE (LP,265) NPOS,'CHARACTER'
C
C  INITIALIZE INTEGER INDEX
      NPOS=MURHSI*2
      DO 60 I=1,NPOS
         IURHSI(I)=0
60       CONTINUE
      CALL SULINE (LP,2)
      WRITE (LP,265) NPOS,'INTEGER'
C
C  INITIALIZE DAILY DATA FILES
70    DO 80 I=1,LRCPD2
         IDBUF(I)=MISSNG
80       CONTINUE
      DO 100 I=1,NMDDF
         NUMREC=JPDDFC(1,I)
         IUNIT=KURDDF(I)
         IF (IPDDB.GT.0) CALL SULINE (IOGDB,1)
         IF (IPDDB.GT.0) WRITE (IOGDB,290) I,NUMREC
         DO 90 IREC=1,NUMREC
            CALL UWRITT (IUNIT,IREC,IDBUF,ISTAT)
            IF (ISTAT.NE.0) GO TO 170
90          CONTINUE
         CALL SULINE (LP,2)
         WRITE (LP,260) NUMREC,IUNIT
100      CONTINUE
C
C  INITITLIZE PP24 AND PPSR IF ANY STATIONS DEFINED
      DO 110 I=1,LRCPD2
         IDBUF(I)=MISSPP
110      CONTINUE
      XTYPE='PP24'
      IX=IPDCKD(XTYPE)
      IDONE=0
120   IF (IX.EQ.0) GO TO 140
      IF (JDDTDR(7,IX).LT.1) GO TO 140
      IBEG=JDDTDR(15,IX)
      IEND=JDDTDR(21,IX)*JDDTDR(7,IX)+IBEG-1
      IFILE=JDDTDR(4,IX)
      IUNIT=KURDDF(IFILE)
      NUMREC=IEND-IBEG+1
      DO 130 IREC=IBEG,IEND
         CALL UWRITT (IUNIT,IREC,IDBUF,ISTAT)
         IF (ISTAT.NE.0) GO TO 170
130      CONTINUE
      CALL SULINE (LP,2)
      WRITE (LP,270) NUMREC,IUNIT,XTYPE
140   IDONE=IDONE+1
      XTYPE='PPSR'
      IX=IPDCKD(XTYPE)
      IF (IDONE.LT.2) GO TO 120
C
C  SET FIRST WORD OF RRS FREEPOOL RECORDS
      CALL UMEMST (0,IFPBUF,LRLURD)
      IFPBUF(1)=-1
      IUNIT=KURDDF(LUFREE)
      NUMREC=MAXFRC-LFREE1+1
      DO 150 IREC=LFREE1,MAXFRC
         CALL UWRITT (IUNIT,IREC,IFPBUF,ISTAT)
         IF (ISTAT.NE.0) GO TO 170
150      CONTINUE
      CALL SULINE (LP,2)
      WRITE (LP,280) NUMREC,IUNIT
C
C  ZERO REMAINING RRS FILE
      CALL UMEMST (0,IRRSAR,LRLURS)
      DO 160 IREC=1,MAXRSF
         CALL UWRITT (KURRRS,IREC,IRRSAR,ISTAT)
         IF (ISTAT.NE.0) GO TO 170
160      CONTINUE
      CALL SULINE (LP,2)
      WRITE (LP,260) MAXRSF,KURRRS
C
C  WRITE CONTROL RECORDS TO NEW FILES
      IAMORD=1
      CALL WPPDCO (ISTAT)
      IF (ISTAT.NE.0) GO TO 170
C
      CALL SULINE (LP,2)
      WRITE (LP,300)
      GO TO 200
C
170   WRITE (LP,180)
      CALL SUERRS (LP,2,-1)
180   FORMAT ('0*** ERROR - IN URIPDB - SYSTEM ERROR.')
C
C  SET ERROR FLAG
      IWURFL=1
C
200   IF (IPDTR.GT.0) CALL SULINE (IOGDB,1)
      IF (IPDTR.GT.0) WRITE (IOGDB,310)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
210   FORMAT (' *** ENTER URIPDB')
220   FORMAT ('0*** BEGIN TO INITIALIZE PREPROCESSOR DATA ',
     *   'BASE NEW FILES.')
230   FORMAT ('0*** BEGIN TO INITIALIZE PREPROCESSOR DATA ',
     *   'BASE NEW FILES BY COPYING CONTROLS FROM OLD DATA FILES.')
240   FORMAT (' NEW INDEX CONTROL=',16I5)
250   FORMAT (' NEW RRS CONTROL=',10I5,2A4)
260   FORMAT ('0*** NOTE - ',I6,' RECORDS INITIALIZED FOR ',
     *   'UNIT ',I2,'.')
265   FORMAT ('0*** NOTE - ',I6,' POSITIONS INITIALIZED IN ',
     *   A,' INDEX.')
270   FORMAT ('0*** NOTE - ',I6,' RECORDS INITIALIZED FOR ',
     *   'UNIT ',I2,' FOR DATA TYPE ',A4,'.')
280   FORMAT ('0*** NOTE - ',I6,' RECORDS INITIALIZED FOR ',
     *   'UNIT ',I2,' RRS FREEPOOL.')
290   FORMAT (' MAX RECORDS IN DAILY DATA FILE ',I2,'=',I6)
300   FORMAT ('0*** NOTE - THE PREPROCESSOR DATA BASE NEW FILES ',
     *   'HAVE BEEN SUCCESSFULLY INITIALIZED.')
310   FORMAT (' *** EXIT URIPDB')
C
      END
