C MODULE URVCMP
C-----------------------------------------------------------------------
C
      SUBROUTINE URVCMP (LPNTRO,IPNTRO,NPNTRO,LPNTRN,IPNTRN,
     *   LDATAO,IDATAO,NDATAO,LDATAN,IDATAN,
     *   ISTAT)
C
C  ROUTINE TO COMPRESS PREPROCESSOR DATA BASE LESS THAN 24-HR TIME
C  INTERVAL DATA
C
      CHARACTER*4 DTYPE
      CHARACTER*50 STRING
C
      INTEGER*2 IPNTRO(LPNTRO),IPNTRN(LPNTRN)
      INTEGER*2 IDATAO(LDATAO),IDATAN(LDATAN)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdbdta'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'urcommon/ursifc'
      INCLUDE 'urcommon/urpddt'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urvcmp.f,v $
     . $',                                                             '
     .$Id: urvcmp.f,v 1.2 2003/11/10 18:37:39 scv Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.0) THEN
         WRITE (IOGDB,*) 'ENTER URVCMP'
         CALL SULINE (IOGDB,1)
         ENDIF
C
      IF (IPDDB.GT.0) THEN
         WRITE (IOGDB,*)
     *      ' LPNTRO=',LPNTRO,
     *      ' LPNTRN=',LPNTRN,
     *      ' LDATAO=',LDATAO,
     *      ' LDATAN=',LDATAN,
     *      ' '
         CALL SULINE (IOGDB,1)
         ENDIF
C
      ISTAT=0
C
      NDTYPE=0
      MDTYPE=2
C
C  SET NUMBER OF INTEGER*2 WORDS PER RECORD
      LRCPD2=LRCPDD*2
C
      IF (IPDDB.GT.0) THEN
         WRITE (IOGDB,*)
     *      ' LRCPD2=',LRCPD2,
     *      ' MISSNG=',MISSNG,
     *      ' IAMORD=',IAMORD,
     *      ' '
         CALL SULINE (IOGDB,1)
         ENDIF
C
      IF (IAMORD.EQ.0.OR.IAMORD.EQ.1) THEN
         ELSE
            WRITE (LP,60) DTYPE
            CALL SUERRS (LP,2,-1)
            ISTAT=1
            GO TO 50
         ENDIF
C
      NPNTRO=0
      NDATAO=0
C
10    NDTYPE=NDTYPE+1
      IF (NDTYPE.GT.MDTYPE) GO TO 50
C
      IF (NDTYPE.EQ.1) DTYPE='PPVR'
      IF (NDTYPE.EQ.2) DTYPE='TAVR'
C
      WRITE (LP,70) DTYPE
      CALL SULINE (LP,2)
C
      IF (IPDDB.GT.0) THEN
         WRITE (IOGDB,*)
     *      ' NMDTYP=',NMDTYP,
     *      ' IPTTYP=',IPTTYP,
     *      ' '
         CALL SULINE (IOGDB,1)
         ENDIF
C
C  FIND DATA TYPE IN DIRECTORY
      IXTYPE=IPDCKD(DTYPE)
      IF (IXTYPE.EQ.0) THEN
         WRITE (LP,110) DTYPE
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 40
         ENDIF
      IXTM24=IPDCKD('TM24')
      IF (IXTM24.EQ.0) THEN
         WRITE (LP,110) 'TM24'
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 40
         ENDIF
C
      IF (IPDDB.GT.0) THEN
         WRITE (IOGDB,*)
     *      ' DTYPE=',DTYPE,
     *      ' IXTYPE=',IXTYPE,
     *      ' '
         CALL SULINE (IOGDB,1)
         IF (IAMORD.EQ.0) THEN
            WRITE (IOGDB,*)
     *         ' KPDSIF=',KPDSIF,
     *         ' INFREC=',INFREC,
     *         ' LSTSIF=',LSTSIF,
     *         ' '
            CALL SULINE (IOGDB,1)
            WRITE (IOGDB,*)
     *         ' IDDTDR(4,IXTYPE)=',IDDTDR(4,IXTYPE),
     *         ' IDDTDR(5,IXTYPE)=',IDDTDR(5,IXTYPE),
     *         ' IDDTDR(7,IXTYPE)=',IDDTDR(7,IXTYPE),
     *         ' IDDTDR(14,IXTYPE)=',IDDTDR(14,IXTYPE),
     *         ' IDDTDR(15,IXTYPE)=',IDDTDR(15,IXTYPE),
     *         ' '
            CALL SULINE (IOGDB,1)
            WRITE (IOGDB,*)
     *         ' IDDTDR(17,IXTYPE)=',IDDTDR(17,IXTYPE),
     *         ' IDDTDR(18,IXTYPE)=',IDDTDR(18,IXTYPE),
     *         ' IDDTDR(19,IXTYPE)=',IDDTDR(19,IXTYPE),
     *         ' IDDTDR(21,IXTYPE)*LRCPD2=',IDDTDR(21,IXTYPE)*LRCPD2,
     *         ' '
            CALL SULINE (IOGDB,1)
            ENDIF
         IF (IAMORD.EQ.1) THEN
            WRITE (IOGDB,*)
     *         ' KURSIF=',KURSIF,
     *         ' ISIFRC=',ISIFRC,
     *         ' LTSIFR=',LTSIFR,
     *         ' '
            CALL SULINE (IOGDB,1)
            WRITE (IOGDB,*)
     *         ' JDDTDR(4,IXTYPE)=',JDDTDR(4,IXTYPE),
     *         ' JDDTDR(5,IXTYPE)=',JDDTDR(5,IXTYPE),
     *         ' JDDTDR(7,IXTYPE)=',JDDTDR(7,IXTYPE),
     *         ' JDDTDR(14,IXTYPE)=',JDDTDR(14,IXTYPE),
     *         ' JDDTDR(15,IXTYPE)=',JDDTDR(15,IXTYPE),
     *         ' '
            CALL SULINE (IOGDB,1)
            WRITE (IOGDB,*)
     *         ' JDDTDR(17,IXTYPE)=',JDDTDR(17,IXTYPE),
     *         ' JDDTDR(18,IXTYPE)=',JDDTDR(18,IXTYPE),
     *         ' JDDTDR(19,IXTYPE)=',JDDTDR(19,IXTYPE),
     *         ' JDDTDR(21,IXTYPE)*LRCPD2=',JDDTDR(21,IXTYPE)*LRCPD2,
     *         ' '
            ENDIF
            CALL SULINE (IOGDB,1)
         ENDIF
C
      IUNITX=0
      IUNITD=0
      IRSIFC=0
      LRSIFC=0
      NPTR=0
      NPTM24=0
      NDAYS=0
      IPTREC=0
      IDTREC=0
      NUMSTA=0
      NPUSDO=0
      NDUSDO=0
      NDMAX=0
C
      IF (IAMORD.EQ.0) THEN
         IUNITX=KPDSIF
         IUNITD=KPDDDF(IDDTDR(4,IXTYPE))
         IRSIFC=INFREC+1
         LRSIFC=LSTSIF
         NPTR=IDDTDR(5,IXTYPE)
         NPTM24=IDDTDR(5,IXTM24)
         NDAYS=IDDTDR(7,IXTYPE)
         IPTREC=IDDTDR(14,IXTYPE)
         IDTREC=IDDTDR(15,IXTYPE)
         NUMSTA=IDDTDR(17,IXTYPE)
         NPUSDO=IDDTDR(18,IXTYPE)
         NDUSDO=IDDTDR(19,IXTYPE)
         NDMAX=IDDTDR(21,IXTYPE)*LRCPD2
         ENDIF
      IF (IAMORD.EQ.1) THEN
         IUNITX=KURSIF
         IUNITD=KURDDF(JDDTDR(4,IXTYPE))
         IRSIFC=ISIFRC+1
         LRSIFC=LTSIFR
         NPTR=JDDTDR(5,IXTYPE)
         NPTM24=JDDTDR(5,IXTM24)
         NDAYS=JDDTDR(7,IXTYPE)
         IPTREC=JDDTDR(14,IXTYPE)
         IDTREC=JDDTDR(15,IXTYPE)
         NUMSTA=JDDTDR(17,IXTYPE)
         NPUSDO=JDDTDR(18,IXTYPE)
         NDUSDO=JDDTDR(19,IXTYPE)
         NDMAX=JDDTDR(21,IXTYPE)*LRCPD2
         ENDIF
C
      IF (NPUSDO.GT.NPNTRO) NPNTRO=NPUSDO
      IF (NDUSDO.GT.NDATAO) NDATAO=NDUSDO
C
      IF (IPDDB.GT.0) THEN
         WRITE (IOGDB,*)
     *      ' IUNITX=',IUNITX,
     *      ' IUNITD=',IUNITD,
     *      ' IRSIFC=',IRSIFC,
     *      ' LRSIFC=',LRSIFC,
     *      ' '
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,*)
     *      ' NPTR=',NPTR,
     *      ' NPTM24=',NPTM24,
     *      ' NDAYS=',NDAYS,
     *      ' '
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,*)
     *      ' IPTREC=',IPTREC,
     *      ' IDTREC=',IDTREC,
     *      ' NUMSTA=',NUMSTA,
     *      ' '
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,*)
     *      ' NPUSDO=',NPUSDO,
     *      ' NDUSDO=',NDUSDO,
     *      ' NDMAX=',NDMAX,
     *      ' '
         CALL SULINE (IOGDB,1)
         ENDIF
C
C  CHECK NUMBER OF STATIONS DEFINED
      IF (NUMSTA.EQ.0) THEN
         WRITE (LP,120) DTYPE
         CALL SULINE (LP,2)
         GO TO 40
         ENDIF
C
C  READ POINTER RECORDS
      IREC=IPTREC
      NREC=IUNRCD(NPUSDO,LRCPD2)
      CALL RVLRCD (IUNITD,IREC,NREC,IPNTRO,LRCPDD,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,130) 'READING',IREC,IUNITD
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 50
         ENDIF
C
C  DUMP POINTER RECORD
      IF (IPDDB.GT.0) THEN
         STRING='IPNTRO'
         CALL URVCM2 (STRING,IPNTRO,NPUSDO)
         ENDIF
C
C  PROCESS EACH DAY
      NDUSDN=0
      INEWDA=1
      NRECPD=IUNRCD(NDMAX,LRCPD2)
      IRECDT=IDTREC
      DO 30 IDAYS=1,NDAYS
C     READ DATA RECORDS
         NREC=IUNRCD(NDUSDO,LRCPD2)
         IREC=IRECDT
         CALL RVLRCD (IUNITD,IREC,NREC,IDATAO,LRCPDD,IERR)
         IF (IERR.NE.0) THEN
            WRITE (LP,130) 'READING',IREC,IUNITD
            CALL SUERRS (LP,2,-1)
            ISTAT=1
            GO TO 50
            ENDIF
C     PRINT ARRAY
         IF (IPDDB.GT.0) THEN
            STRING='IDATAO'
            CALL URVCM2 (STRING,IDATAO,NDUSDO)
            ENDIF
C     COMPRESS DATA RECORDS
         IUPSIF=1
         CALL URVCM3 (DTYPE,NPTR,NPTM24,LRCPD2,INEWDA,
     *      IRSIFC,LRSIFC,IUNITX,IUPSIF,
     *      LPNTRO,IPNTRO,NPUSDO,LPNTRN,IPNTRN,
     *      LDATAO,IDATAO,NDUSDO,NDMAX,LDATAN,IDATAN,NDUSDN,
     *      IERR)
         IF (IERR.NE.0) THEN
            IF (IPPDB.GT.0) THEN
               WRITE (IOGDB,*)
     *            ' URVCM3 CALLED : ',
     *            ' IERR=',IERR,
     *            ' '
               CALL SULINE (LP,1)
               ENDIF
            ISTAT=1
            GO TO 40
            ENDIF
C     CHECK IF NEW DAY
         IF (INEWDA.EQ.0) GO TO 20
            IF (IPDDB.GT.0) THEN
               STRING='IPNTRN'
               CALL URVCM2 (STRING,IPNTRN,NPUSDO)
               ENDIF
C        WRITE POINTER ARRAY
            NPREC=IUNRCD(NPUSDO,LRCPD2)
            IREC=IPTREC
            CALL WVLRCD (IUNITD,IREC,NPREC,IPNTRN,LRCPDD,IERR)
            IF (IERR.NE.0) THEN
               WRITE (LP,130) 'WRITING',IREC,IUNITD
               CALL SUERRS (LP,2,-1)
               ISTAT=1
               GO TO 50
               ENDIF
20       IF (IPDDB.GT.0) THEN
            STRING='IDATAN'
            CALL URVCM2 (STRING,IDATAN,NDUSDN)
            ENDIF
C     WRITE DATA ARRAY
         NDREC=IUNRCD(NDUSDN,LRCPD2)
         IREC=IRECDT
         CALL WVLRCD (IUNITD,IREC,NDREC,IDATAN,LRCPDD,IERR)
         INEWDA=0
         IRECDT=IRECDT+NRECPD
30       CONTINUE
C
C  CHECK IF NUMBER OF DATA WORDS USED CHANGED
      NDUSDO=0
      IF (IAMORD.EQ.0) NDUSDO=IDDTDR(19,IXTYPE)
      IF (IAMORD.EQ.1) NDUSDO=JDDTDR(19,IXTYPE)
      IF (NDUSDN.EQ.NDUSDO) THEN
         WRITE (LP,90) DTYPE,NDUSDO
         CALL SULINE (LP,2)
         GO TO 40
         ELSE
C        UPDATE DIRECTORY FOR CHANGE IN NUMBER OF DATA VALUES
            WRITE (LP,100) DTYPE,NDUSDO,NDUSDN
            CALL SULINE (LP,2)
            IF (IAMORD.EQ.0) IDDTDR(19,IXTYPE)=NDUSDN
            IF (IAMORD.EQ.1) JDDTDR(19,IXTYPE)=NDUSDN
            GO TO 40
         ENDIF
C
40    GO TO 10
C
50    IF (IPDTR.GT.0) THEN
         WRITE (IOGDB,*) 'EXIT URVCMP'
         CALL SULINE (IOGDB,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT ('0*** ERROR - INVALID VALUE OF IAMORD : ',I3)
70    FORMAT ('0*** NOTE - BEGIN TO COMPRESS  << ',A4,
     *   ' DATA >>  RECORDS.')
90    FORMAT ('0*** NOTE - DATA WORDS USED FOR TYPE ',A4,
     *   ' (',I5,') DID NOT CHANGE.')
100   FORMAT ('0*** NOTE - DATA WORDS USED FOR TYPE ',A4,
     *   ' WILL BE CHANGED FROM ',I5,' TO ',I5,'.')
110   FORMAT ('0*** ERROR - TYPE ',A4,' NOT FOUND IN THE ',
     *   'PREPROCESSOR DATA BASE DIRECTORY.')
120   FORMAT ('0*** NOTE - NO STATIONS WITH DATA TYPE ',A,
     *   ' ARE DEFINED.')
130   FORMAT ('0*** ERROR - ',A,' RECORD ',I6,' FROM UNIT ',I2,'.')
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE URVCM2 (STRING,ARRAY,LARRAY)
C
C  ROUTINE TO PRINT I*2 ARRAY
C
      CHARACTER*(*) STRING
C
      INTEGER*2 ARRAY(LARRAY)
C
      INCLUDE 'uiox'
C
C
      IF (LARRAY.EQ.0) GO TO 20
C
      WRITE (LP,30) STRING(1:LENSTR(STRING))
      CALL SULINE (LP,2)
C
      IPOS1=1
      NPER=20
C
10    IPOS2=IPOS1+NPER-1
      IF (IPOS2.GT.LARRAY) IPOS2=LARRAY
C
      WRITE (LP,40) IPOS1,(ARRAY(I),I=IPOS1,IPOS2)
      CALL SULINE (LP,1)
C
      IPOS1=IPOS1+NPER
      IF (IPOS1.GT.LARRAY) GO TO 20
      GO TO 10
C
20    RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT ('0',A,':')
40    FORMAT (' ',I5,': ',20(I5,1X))
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE URVCM3 (DTYPE,NPTR,NPTM24,LRCPD2,INEWDA,
     *   IRSIFC,LRSIFC,IUNITX,IUPSIF,
     *   LPNTRO,IPNTRO,NPUSDO,LPNTRN,IPNTRN,
     *   LDATAO,IDATAO,NDUSDO,NDMAX,LDATAN,IDATAN,NDUSDN,
     *   ISTAT)
C
C  THIS ROUTINE COMPRESSES THE PREPROCESSOR DATA BASE DATA ARRAYS
C  CONTAINING VARIABLE TIME INTERVAL DATA FOR DAILY DATA TYPES.
C
C  THE LAST POINTER POSITION FOR EACH STATION IS CHANGED IF
C  THE DATA ARE MOVED. POINTERS ARE NOT MOVED.
C
C     ARGUMENT LIST:
C
C       ARGUMENT       I/O   TYPE   DIM    DESCRIPTION
C       --------       ----- -----  -----  ---------------------
C       DTYPE            I    A4      1    DATA TYPE
C       NPTR             I   I*4      1    NUMBER OF POINTERS
C       NPTM24           I   I*4      1    NUMBER OF POINTERS FOR DATA
C                                           TYPE TM24
C       LRCPD2           I   I*4      1    NUMBER OF I*2 WORDS IN RECORD
C       INEWDA           I   I*4      1    NEW DAY INDICATOR
C                                           1=INITIAL DAY
C                                           0=SUBSEQUENT DAY
C       IRSIFC           I   I*4      1    INITIAL SIF RECORD NUMBER
C       LRSIFC           I   I*4      1    LAST SIF RECORD NUMBER
C       IUNITX           I   I*4      1    INDEX UNIT NUMBER
C       IUPSIF           I   I*4      1    UPDATE INDICATOR
C                                            1=ONLY UPDATE SIF IF
C                                              DATA POINTER CHANGES
C                                           -1=ALWAYS UPDATE SIF
C       LPNTRO           I   I*4      1    I*2 LENGTH OF IPNTRO
C       IPNTRO           I   I*2    LPNTR  OLD POINTER ARRAY
C       NPUSDO           I   I*4      1    NUMBER OF POINTER WORDS USED
C                                           IN IPNTRO
C       LPNTRN           I   I*4      1    I*2 LENGTH OF IPNTRN
C       IPNTRN           O   I*2    LPNTR  NEW POINTER ARRAY
C       LDATAO           I   I*4      1    I*2 LENGTH OF IDATAO
C       IDATAO           I   I*2    LDATA  OLD DATA ARRAY
C       NDUSDO           I   I*4      1    NUMBER OF DATA WORDS USED
C                                           IN IDATAO
C       NDMAX            I   I*4      1    MAXIMUM NUMBER OF DATA WORDS
C       LDATAN           I   I*4      1    I*2 LENGTH OF IDATAN
C       IDATAN           O   I*2    LDATA  NEW DATA ARRAY
C       NDUSDN           O   I*4      1    I*2 WORDS USED IN IDATAN
C       ISTAT            O   I*4      1    STATUS CODE
C                                           0=NORMAL RETURN
C                                           1=INVALID TYPE
C                                           2=POINTER OR DATA
C                                             ARRAY TOO SMALL
C                                           3=POINTER OR DATA
C                                             ARRAY EMPTY
C                                           4=ALL STATIONS DELETED
C                                           5=ERROR ACCESSING FILE
C                                           6=NUMBER OF POINTERS IS ZERO
C                                           7=NUMBER OF WORDS IN SIF IS
C                                             ZERO
C
C
      CHARACTER*(*) DTYPE
      CHARACTER*4 STYPE
      CHARACTER*8 STAID
C
      INTEGER*2 IPNTRO(LPNTRO),IPNTRN(LPNTRN)
      INTEGER*2 IDATAO(LDATAO),IDATAN(LDATAN)
      INTEGER*2 I2VAL,ISIBUF(32)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdbdta'
C
      EQUIVALENCE (ICVAR1,INOTE)
C
C
      IF (IPDTR.GT.0) THEN
         WRITE (IOGDB,*) 'ENTER URVCM3'
         CALL SULINE (IOGDB,1)
         ENDIF
C
      IF (IPDDB.GT.0) THEN
         WRITE (IOGDB,*)
     *      ' DTYPE=',DTYPE,
     *      ' NPTR=',NPTR,
     *      ' NPTM24=',NPTM24,
     *      ' LRCPD2=',LRCPD2,
     *      ' INEWDA=',INEWDA,
     *      ' '
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,*)
     *      ' IRSIFC=',IRSIFC,
     *      ' LRSIFC=',LRSIFC,
     *      ' IUNITX=',IUNITX,
     *      ' IUPSIF=',IUPSIF,
     *      ' '
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,*)
     *      ' LPNTRO=',LPNTRO,
     *      ' NPUSDO=',NPUSDO,
     *      ' LPNTRN=',LPNTRN,
     *      ' '
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,*)
     *      ' LDATAO=',LDATAO,
     *      ' NDUSDO=',NDUSDO,
     *      ' LDATAN=',LDATAN,
     *      ' NDUSDN=',NDUSDN,
     *      ' NDMAX=',NDMAX,
     *      ' '
         CALL SULINE (IOGDB,1)
         ENDIF
C
      ISTAT=0
C
      INDERR=0
C
      IF (DTYPE.EQ.'PPVR'.OR.DTYPE.EQ.'TAVR') THEN
         ELSE
            WRITE (LP,160) DTYPE
            CALL SUERRS (LP,2,-1)
            ISTAT=1
            INDERR=1
         ENDIF
      IF (NPUSDO.GT.LPNTRO) THEN
         WRITE (LP,170) 'POINTERS',NPUSDO,LPNTRO
         CALL SUERRS (LP,2,-1)
         ISTAT=2
         INDERR=1
         ENDIF
      IF (NDUSDO.GT.LDATAO) THEN
         WRITE (LP,170) 'DATA',NDUSDO,LDATAO
         CALL SUERRS (LP,2,-1)
         ISTAT=2
         INDERR=1
         ENDIF
      IF (NPUSDO.EQ.0) THEN
         WRITE (LP,180) 'POINTER'
         CALL SUERRS (LP,2,-1)
         ISTAT=3
         INDERR=1
         ENDIF
      IF (NDUSDO.EQ.0) THEN
         WRITE (LP,180) 'DATA'
         CALL SUERRS (LP,2,-1)
         ISTAT=3
         INDERR=1
         ENDIF
      IF (NPTR.EQ.0) THEN
         WRITE (LP,190) 'POINTERS'
         CALL SUERRS (LP,2,-1)
         ISTAT=6
         INDERR=1
         ENDIF
C
      IF (INDERR.GT.0) GO TO 150
C
C  CHECK IF FIRST DAY
      IF (INEWDA.EQ.1) THEN
C     CHANGE POINTER ARRAY
         IPOS=1
         DO 30 IPUSD=1,NPUSDO
            IF (IPUSD.EQ.(IPUSD/NPTR)*NPTR) GO TO 20
10             IPNTRN(IPUSD)=IPNTRO(IPUSD)
               GO TO 30
20          IF (IPNTRO(IPUSD).EQ.0) GO TO 10
               IPNTRN(IPUSD)=IPOS
               ITMINT=IPNTRO(IPUSD-1)
               IF (ITMINT.EQ.0) GO TO 30
                  NVAL=24/ITMINT
                  IPOS=IPOS+NVAL
30          CONTINUE
         ENDIF
C
C  COMPRESS DATA ARRAY
      NDUSDN=0
      NUPSIF=0
      NDELET=0
      IPRBLN=0
      DO 140 IPTR=NPTR,NPUSDO,NPTR
         NPOSN1=IPNTRN(IPTR)
         NPOSO1=IPNTRO(IPTR)
         IF (NPOSN1.EQ.0) GO TO 140
         ITMINT=IPNTRO(IPTR-1)
         NVAL=24/ITMINT
         NPOSN2=NPOSN1+NVAL-1
         IPNTR1=IPNTRO(IPTR-NPTR+1)
         IF (IPDDB.GT.1) THEN
            WRITE (IOGDB,*)
     *         ' IPNTR1=',IPNTR1,
     *         ' MISSNG=',MISSNG,
     *         ' '
            CALL SULINE (IOGDB,1)
            ENDIF
C     CHECK IF DELETED SLOT
         IF (IPNTR1.EQ.0) GO TO 60
C     MOVE DATA
         DO 50 N=NPOSN1,NPOSN2
            J=N-NPOSN1
            IF (IPDDB.GT.1) THEN
               WRITE (IOGDB,*)
     *            ' NPOSN1=',NPOSN1,
     *            ' NPOSN2=',NPOSN2,
     *            ' J=',J,
     *            ' NDMAX=',NDMAX,
     *            ' '
               CALL SULINE (IOGDB,1)
               ENDIF
            IF (NPOSO1+J.GT.NDMAX) GO TO 40
               IDATAN(N)=IDATAO(NPOSO1+J)
               GO TO 50
40          IDATAN(N)=MISSNG
50          CONTINUE
60       NDUSDN=NDUSDN+NVAL
C     CHECK IF NEW DAY
         IF (INEWDA.NE.1) GO TO 140
C     CHECK IF DELETED SLOT
         IF (IPNTR1.EQ.0) THEN
            NDELET=NDELET+1
            INOTE=0
            IF (INOTE.EQ.1) THEN
               IF (IPRBLN.EQ.0) THEN
                  WRITE (LP,*)
                  CALL SULINE (LP,1)
                  IPRBLN=1
                  ENDIF
               WRITE (LP,200) IPTR,DTYPE
               CALL SULINE (LP,1)
               ENDIF
            ENDIF
C     CHECK IF SIF TO ALWAYS BE UPDATED
         IF (IUPSIF.EQ.-1) GO TO 80
         IF (NPOSO1.EQ.NPOSN1) GO TO 140
C     CHECK IF DELETED SLOT
         IF (IPNTR1.EQ.0) THEN
            IF (IPDDB.GT.1) THEN
               WRITE (IOGDB,*)
     *            ' IPNTR1=',IPNTR1,
     *            ' NPOSN1=',NPOSN1,
     *            ' NPOSN2=',NPOSN2,
     *            ' '
               CALL SULINE (IOGDB,1)
               ENDIF
            DO 70 N=NPOSN1,NPOSN2
               IDATAN(N)=MISSNG
70             CONTINUE
            GO TO 140
            ENDIF
80       JSIBUF=0
         I2VAL=0
         IF (DTYPE.EQ.'PPVR') THEN
            JSIBUF=7
            I2VAL=IPNTR1
            ENDIF
         IF (DTYPE.EQ.'TAVR') THEN
            JSIBUF=9
            I2VAL=(IPNTR1/NPTM24)*2+1
            ENDIF
         IREC=IRSIFC
         IF (IPDDB.GT.1) THEN
            WRITE (IOGDB,*)
     *         ' IREC=',IREC,
     *         ' JSIBUF=',JSIBUF,
     *         ' I2VAL=',I2VAL,
     *         ' '
            CALL SULINE (IOGDB,1)
            ENDIF
C     READ SIF RECORD
90       CALL UREADT (IUNITX,IREC,ISIBUF,IERR)
         IF (IERR.NE.0) THEN
            WRITE (LP,270) 'READING',IREC,IUNITX
            CALL SUERRS (LP,2,-1)
            ISTAT=5
            GO TO 150
            ENDIF
         IF (ISIBUF(JSIBUF).NE.I2VAL) GO TO 100
C     GET STATION IDENTIFIER
         STAID=' '
         CALL SUBSTR (ISIBUF(2),1,LEN(STAID),STAID,-1)
C     CHECK IF DELETED
         IF (STAID.NE.'DELETED') GO TO 110
100      NWORDS=ISIBUF(1)
         IF (NWORDS.EQ.0) THEN
            WRITE (LP,210) 'WORDS IN',IUNITX,IREC
            CALL SUERRS (LP,2,-1)
            ISTAT=7
            GO TO 150
            ENDIF
         NREC=IUNRCD(NWORDS,LRCPD2)
         IF (NREC.EQ.0) THEN
            WRITE (LP,210) 'RECORDS FOR',IUNITX,IREC
            CALL SUERRS (LP,2,-1)
            ISTAT=7
            GO TO 150
            ENDIF
         IREC=IREC+NREC
         IF (IPDDB.GT.1) THEN
            WRITE (IOGDB,*)
     *         ' NWORDS=',NWORDS,
     *         ' NREC=',NREC,
     *         ' IREC=',IREC,
     *         ' LRSIFC=',LRSIFC,
     *         ' '
            CALL SULINE (IOGDB,1)
            ENDIF
C     CHECK IF PROCESSING LAST SIF RECORD
         IF (IREC.LE.LRSIFC) GO TO 90
C        STATION NOT FOUND
            IPOS=IPTR-NPTR+1
            WRITE (LP,220) DTYPE,IPOS
            CALL SUWRNS (LP,2,-1)
            GO TO 140
C     UPDATE DATA ARRAY POSITION IN SIF
110      NTYPES=ISIBUF(10)
         JSIBUF=11
         DO 120 ITYPES=1,NTYPES
            STYPE=' '
            CALL SUBSTR (ISIBUF(JSIBUF),1,LEN(STYPE),STYPE,-1)
            IF (IPDDB.GT.1) THEN
               WRITE (IOGDB,*)
     *            ' NTYPES=',NTYPES,
     *            ' ITYPES=',ITYPES,
     *            ' JSIBUF=',JSIBUF,
     *            ' STYPE=',STYPE,
     *            ' ITMINT=',ITMINT,
     *            ' '
               CALL SULINE (IOGDB,1)
               ENDIF
            IF (DTYPE.EQ.'PPVR'.AND.
     *         (STYPE.EQ.'PP01'.AND.ITMINT.EQ.1.OR.
     *          STYPE.EQ.'PP03'.AND.ITMINT.EQ.3.OR.
     *          STYPE.EQ.'PP06'.AND.ITMINT.EQ.6)) GO TO 130
            IF (DTYPE.EQ.'TAVR'.AND.
     *         (STYPE.EQ.'TA01'.AND.ITMINT.EQ.1.OR.
     *          STYPE.EQ.'TA03'.AND.ITMINT.EQ.3.OR.
     *          STYPE.EQ.'TA06'.AND.ITMINT.EQ.6)) GO TO 130
            JSIBUF=JSIBUF+3
120         CONTINUE
C     TYPE NOT FOUND
         IF (IPDDB.GT.1) THEN
            WRITE (IOGDB,*)
     *         ' IPNTR1=',IPNTR1,
     *         ' '
            CALL SULINE (IOGDB,1)
            ENDIF
         WRITE (LP,230) STYPE,STAID
         CALL SUWRNS (LP,2,-1)
         GO TO 140
C     UPDATE DATA ARRAY POSITION
130      ISIBUF(JSIBUF+2)=NPOSN1
C     REWRITE SIF RECORD
         CALL UWRITT (IUNITX,IREC,ISIBUF,IERR)
         IF (IERR.NE.0) THEN
            ISTAT=5
            WRITE (LP,270) 'WRITING',IREC,IUNITX
            CALL SUERRS (LP,2,-1)
            GO TO 150
            ENDIF
         IF (IPDDB.GT.1) THEN
            WRITE (IOGDB,*)
     *      ' DTYPE=',DTYPE,
     *      ' STAID=',STAID,
     *      ' NPOSN1=',NPOSN1,
     *      ' IREC=',IREC,
     *      ' '
            CALL SULINE (IOGDB,1)
            ENDIF
         INOTE=0
         IF (INOTE.EQ.1) THEN
            IF (IPRBLN.EQ.0) THEN
               WRITE (LP,*)
               CALL SULINE (LP,1)
               IPRBLN=1
               ENDIF
            WRITE (LP,240) DTYPE,STAID,NPOSN1,IREC
            CALL SULINE (LP,1)
            ENDIF
         NUPSIF=NUPSIF+1
140      CONTINUE
C
      IF (NDELET.GT.0) THEN
         WRITE (LP,250) NDELET,DTYPE
         CALL SULINE (LP,2)
         ENDIF
C
      IF (NUPSIF.GT.0) THEN
         WRITE (LP,260) NUPSIF
         CALL SULINE (LP,2)
         ENDIF
C
      IF (NDUSDN.GT.0) GO TO 150
C
C  NO STATIONS LEFT - ALL WERE DELETED
      ISTAT=4
      WRITE (LP,280)
      CALL SULINE (LP,2)
C
150   IF (IPDTR.GT.0) THEN
         WRITE (IOGDB,*) 'EXIT URVCM3'
         CALL SULINE (IOGDB,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
160   FORMAT ('0*** ERROR - IN URVCM3 - ',A,' IS AN INVALID DATA TYPE.')
170   FORMAT ('0*** ERROR - IN URVCM3 - NUMBER OF ',A,
     *   ' WORDS USED (',I5,
     *   ') EXCEEDS MAXIMUM (',I5,').')
180   FORMAT ('0*** ERROR - IN URVCM3 - NUMBER OF ',A,
     *   ' WORDS USED (',I5,
     *   ') IS ZERO.')
190   FORMAT ('0*** ERROR - IN URVCM3 - NUMBER OF ',A,
     *   ' IS ZERO.')
200   FORMAT (' *** NOTE - POSITION ',I6,' OF THE ',A4,
     *   ' POINTER ARRAY IS MARKED DELETED.')
210   FORMAT ('0*** ERROR - IN URVCM3 - NUMBER OF ',A,
     *   ' SIF RECORD AT RECORD ',I6,' OF UNIT ',I2,
     *   'IS ZERO.')
220   FORMAT ('0*** WARNING - IN URVCM3 - STATION IN ',A,
     *   ' POINTER ARRAY POSITION ',
     *   I6,' NOT FOUND IN THE SIF.')
230   FORMAT ('0*** WARNING - DATA TYPE ',A,
     *   ' NOT FOUND IN THE SIF FOR STATION ',A,'.')
240   FORMAT (' *** NOTE - ',A4,' DATA ARRAY POSITION FOR STATION ',A,
     *   ' SET TO ',I6,' IN SIF RECORD ',I6,'.')
250   FORMAT ('0*** NOTE - ',I4,' DELETED SLOTS FOUND IN ',A4,
     *   ' POINTER ARRAY.')
260   FORMAT ('0*** NOTE - ',I4,' SIF RECORDS UPDATED TO ',
     *   'CHANGE DATA ARRAY POSITION.')
270   FORMAT ('0*** ERROR - ',A,' RECORD ',I6,' FROM UNIT ',I2,'.')
280   FORMAT ('0*** NOTE - ALL STATIONS IN SIF ARE DELETED.')
C
      END
