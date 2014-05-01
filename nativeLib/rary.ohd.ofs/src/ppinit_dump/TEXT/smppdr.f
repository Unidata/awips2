C MODULE SMPPDR
C-----------------------------------------------------------------------
C
C  ROUTINE TO DISPLAY STATISTICS FOR RRS STATIONS.
C
      SUBROUTINE SMPPDR (LARRAY,IARRAY,ISTAT)
C
      CHARACTER*4 DTYPE,UNITS
      CHARACTER*8 STAID,STAIDO,XDATE,RSDATE,MRDATE
      CHARACTER*8 XFORM/'  /  /  '/
      CHARACTER*8 XNONE/'  NONE  '/
      CHARACTER*72 XLINE
C
      PARAMETER (L1IARRAY=5)
      DIMENSION IARRAY(L1IARRAY,1)
      PARAMETER (LSIBUF=128)
      INTEGER*2 ISIBUF(LSIBUF)
      PARAMETER (MRRBUF=3000)
      DIMENSION IRRBUF(MRRBUF)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'pdbcommon/pdrrsc'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdbdta'
      INCLUDE 'pdbcommon/pdtrrx'
      INCLUDE 'hclcommon/hdflts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_dump/RCS/smppdr.f,v $
     . $',                                                             '
     .$Id: smppdr.f,v 1.4 1998/07/06 12:15:46 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,80)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('STAT')
C
      ISTAT=0
C
      NUMERR=0
C
C  OPEN DATA BASE
      CALL SUDOPN (1,'PPD ',IERR)
      IF (IERR.GT.0) THEN
         ISTAT=1
         GO TO 70
         ENDIF
C
C  CHECK IF ANY STATION DEFINED
      IF (NPDSTA.EQ.0) THEN
         WRITE (LP,90)
         CALL SULINE (LP,2)
         GO TO 70
         ENDIF
C
C  READ EACH SIF RECORD
      MAXRR=LARRAY/L1IARRAY
      NUMID=0
      NUMIDR=0
      NUMRR=0
      ISIREC=INFREC+1
10    CALL PDRSIF (ISIREC,NXREC,LSIBUF,ISIBUF,IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,140) 'PDRSIF',IERR
         CALL SUERRS (LP,2,NUMERR)
         GO TO 60
         ENDIF
      CALL SUBSTR (ISIBUF(2),1,LEN(STAID),STAID,1)
      IF (STAID.EQ.'DELETED') GO TO 40
      NUMID=NUMID+1
C
C  CHECK EACH DATA TYPE
      IPTR=11
      NUMTP=ISIBUF(10)
      IFIRST=1
      DO 30 INUMTP=1,NUMTP
C     GET DATA TYPE
         CALL SUBSTR (ISIBUF(IPTR),1,LEN(DTYPE),DTYPE,1)
C     CHECK IF RRS DATA TYPE
         IF (IPDCKR(DTYPE).EQ.0) GO TO 20
         IF (IFIRST.EQ.1) THEN
            NUMIDR=NUMIDR+1
            IFIRST=0
            ENDIF
         NUMRR=NUMRR+1
         IF (NUMRR.GT.MAXRR) THEN
            WRITE (LP,120) MAXRR
            CALL SUERRS (LP,2,NUMERR)
            GO TO 60
            ENDIF
C     STORE STATION IDENTIFIER
         CALL SUBSTR (STAID,1,LEN(STAID),IARRAY(1,NUMRR),1)
C     STORE DATA TYPE
         CALL SUBSTR (DTYPE,1,LEN(DTYPE),IARRAY(3,NUMRR),1)
C     STORE SIF RECORD NUMBER
         IARRAY(4,NUMRR)=ISIREC
C     STORE RRS DATA RECORD NUMBER
         IARRAY(5,NUMRR)=ISIBUF(IPTR+2)
20       IPTR=IPTR+3
30       CONTINUE
C
C  CHECK IF LAST SIF RECORD PROCESSED
40    IF (NXREC.LE.LSTSIF) THEN
         ISIREC=NXREC
         GO TO 10
         ENDIF
C
C  CHECK IF ANY RRS STATIONS FOUND
      IF (NUMID.EQ.0) THEN
         WRITE (LP,100)
         CALL SULINE (LP,2)
         GO TO 60
         ENDIF
C
C  SORT ARRAY
      ISPTR=0
      IWORD1=1
      IWORD2=3
      CALL USORT2 (L1IARRAY,NUMRR,IWORD1,IWORD2,IARRAY,IARRAY,ISPTR,
     *   IERR)
C
C  PRINT ALL STATIONS WITH RRS DATA
      IF (ISLEFT(10).GT.0) CALL SUPAGE
      WRITE (LP,110)
      CALL SULINE (LP,2)
      WRITE (LP,160)
      CALL SULINE (LP,5)
C
C  CHECK SIZE OF ARRAY TO STORE RRS RECORD
      CALL RPDLRS (LWNEED,IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,140) 'RPDLRS',IERR
         CALL SUERRS (LP,2,NUMERR)
         GO TO 60
         ENDIF
      IF (LWNEED.GT.MRRBUF) THEN
         WRITE (LP,130) LWNEED,MRRBUF
         CALL SUERRS (LP,2,NUMERR)
         GO TO 60
         ENDIF
C
C  PROCESS EACH RRS DATA TYPE
      STAIDO=' '
      NNUMID=0
      DO 50 INUMRR=1,NUMRR
C     READ SIF RECORD
         ISIREC=IARRAY(4,INUMRR)
         CALL PDRSIF (ISIREC,NXREC,LSIBUF,ISIBUF,IERR)
         IF (IERR.GT.0) THEN
            WRITE (LP,140) 'PDRSIF',IERR
            CALL SUERRS (LP,2,NUMERR)
            GO TO 60
            ENDIF
C        READ RRS DATA RECORD
            IRRREC=IARRAY(5,INUMRR)
            CALL PDRRRR (IRRREC,LRCPDR,MRRBUF,IRRBUF,IERR)
            IF (IERR.GT.0) THEN
               WRITE (LP,150) IRRREC
               CALL SUERRS (LP,2,NUMERR)
               GO TO 60
               ENDIF
            CALL SUBSTR (IRRBUF(2),1,LEN(STAID),STAID,1)
            ISTBEG=LHDRRS+4
            CALL SUBSTR (IRRBUF(4),1,4,NBRSTA,1)
            CALL SUBSTR (IRRBUF(5),1,LEN(DTYPE),DTYPE,1)
            NUMREP=IRRBUF(LHDRRS+3)
            IX=IPDCKR(DTYPE)
C        SET NUMBER OF CHARACTERS AND DECIMAL PLACES
            NCHARF=8
            NDEC=NUMDEC(IX)
            NCHARI=-2
            IPRERR=1
            XLINE=' '
            IPOS=1
C        DATE RESET
            IF (IRRBUF(LHDRRS+1).EQ.0) THEN
               XDATE=XNONE
               ELSE
                  XDATE=XFORM
                  CALL JLMDYH (IRRBUF(LHDRRS+1),IRMO,IRDAY,IRYR,IRHR)
                  IPOS2=1
                  CALL UFI2A (IRMO,XDATE,IPOS2,NCHARI,IPRERR,LP,IERR)
                  IPOS2=IPOS2+IABS(NCHARI)+1
                  CALL UFI2A (IRDAY,XDATE,IPOS2,NCHARI,IPRERR,LP,IERR)
                  IPOS2=IPOS2+IABS(NCHARI)+1
                  CALL UFI2A (IRYR,XDATE,IPOS2,NCHARI,IPRERR,LP,IERR)
               ENDIF
            RSDATE=XDATE
C        DATE OF MOST RECENT REPORT
            IF (IRRBUF(LHDRRS+2).EQ.0) THEN
               XDATE=XNONE
               ELSE
                  XDATE=XFORM
                  CALL JLMDYH (IRRBUF(LHDRRS+2),MRMO,MRDAY,MRYR,MRHR)
                  IPOS2=1
                  CALL UFI2A (MRMO,XDATE,IPOS2,NCHARI,IPRERR,LP,IERR)
                  IPOS2=IPOS2+IABS(NCHARI)+1
                  CALL UFI2A (MRDAY,XDATE,IPOS2,NCHARI,IPRERR,LP,IERR)
                  IPOS2=IPOS2+IABS(NCHARI)+1
                  CALL UFI2A (MRYR,XDATE,IPOS2,NCHARI,IPRERR,LP,IERR)
               ENDIF
            MRDATE=XDATE
C        LARGEST REPORT
            CALL UFF2A (IRRBUF(ISTBEG),XLINE,IPOS,NCHARF,NDEC,
     *         IPRERR,LP,IERR)
            IPOS=IPOS+NCHARF+1
            IF (IRRBUF(LHDRRS+5).EQ.0) THEN
               XDATE=XNONE
               ELSE
                  XDATE=XFORM
                  CALL JLMDYH (IRRBUF(LHDRRS+5),LGMO,LGDAY,LGYR,LGHR)
                  IPOS2=1
                  CALL UFI2A (LGMO,XDATE,IPOS2,NCHARI,IPRERR,LP,IERR)
                  IPOS2=IPOS2+IABS(NCHARI)+1
                  CALL UFI2A (LGDAY,XDATE,IPOS2,NCHARI,IPRERR,LP,IERR)
                  IPOS2=IPOS2+IABS(NCHARI)+1
                  CALL UFI2A (LGYR,XDATE,IPOS2,NCHARI,IPRERR,LP,IERR)
               ENDIF
            XLINE(IPOS:IPOS+LEN(XDATE))=XDATE
            IPOS=IPOS+LEN(XDATE)+1
C        SECOND LARGEST
            CALL UFF2A (IRRBUF(ISTBEG+2),XLINE,IPOS,NCHARF,NDEC,
     *         IPRERR,LP,IERR)
            IPOS=IPOS+NCHARF+1
            IF (IRRBUF(LHDRRS+7).EQ.0) THEN
               XDATE=XNONE
               ELSE
                  CALL JLMDYH (IRRBUF(LHDRRS+7),L2MO,L2DAY,L2YR,L2HR)
                  IPOS2=1
                  CALL UFI2A (L2MO,XDATE,IPOS2,NCHARI,IPRERR,LP,IERR)
                  IPOS2=IPOS2+IABS(NCHARI)+1
                  CALL UFI2A (L2DAY,XDATE,IPOS2,NCHARI,IPRERR,LP,IERR)
                  IPOS2=IPOS2+IABS(NCHARI)+1
                  CALL UFI2A (L2YR,XDATE,IPOS2,NCHARI,IPRERR,LP,IERR)
               ENDIF
            XLINE(IPOS:IPOS+LEN(XDATE))=XDATE
            IPOS=IPOS+LEN(XDATE)+1
C        SMALLEST
            CALL UFF2A (IRRBUF(ISTBEG+4),XLINE,IPOS,NCHARF,NDEC,
     *         IPRERR,LP,IERR)
            IPOS=IPOS+NCHARF+1
            IF (IRRBUF(LHDRRS+9).EQ.0) THEN
               XDATE=XNONE
               ELSE
                  CALL JLMDYH (IRRBUF(LHDRRS+9),ISMO,ISDAY,ISYR,ISHR)
                  IPOS2=1
                  CALL UFI2A (ISMO,XDATE,IPOS2,NCHARI,IPRERR,LP,IERR)
                  IPOS2=IPOS2+IABS(NCHARI)+1
                  CALL UFI2A (ISDAY,XDATE,IPOS2,NCHARI,IPRERR,LP,IERR)
                  IPOS2=IPOS2+IABS(NCHARI)+1
                  CALL UFI2A (ISYR,XDATE,IPOS2,NCHARI,IPRERR,LP,IERR)
               ENDIF
            XLINE(IPOS:IPOS+LEN(XDATE))=XDATE
            IPOS=IPOS+LEN(XDATE)+1
C        SECOND SMALLEST
           CALL UFF2A (IRRBUF(ISTBEG+6),XLINE,IPOS,NCHARF,NDEC,
     *         IPRERR,LP,IERR)
            IPOS=IPOS+NCHARF+1
            IF (IRRBUF(LHDRRS+11).EQ.0) THEN
               XDATE=XNONE
               ELSE
                  CALL JLMDYH (IRRBUF(LHDRRS+11),
     *               IS2MO,IS2DAY,IS2YR,IS2HR)
                  IPOS2=1
                  CALL UFI2A (IS2MO,XDATE,IPOS2,NCHARI,IPRERR,LP,IERR)
                  IPOS2=IPOS2+IABS(NCHARI)+1
                  CALL UFI2A (IS2DAY,XDATE,IPOS2,NCHARI,IPRERR,LP,IERR)
                  IPOS2=IPOS2+IABS(NCHARI)+1
                  CALL UFI2A (IS2YR,XDATE,IPOS2,NCHARI,IPRERR,LP,IERR)
               ENDIF
            XLINE(IPOS:IPOS+LEN(XDATE))=XDATE
            IPOS=IPOS+LEN(XDATE)+1
C        GET UNITS
            IUNFLG=0
            CALL PFDUNT (DTYPE,IUNFLG,UNITS)
C        CHECK IF NEW STATION
            IF (STAID.NE.STAIDO) THEN
               STAIDO=STAID
               NNUMID=NNUMID+1
               NNUMRR=0
               ENDIF
            NNUMRR=NNUMRR+1
C        PRINT STATISTICS
            IF (ISNWPG(LP).EQ.1) THEN
               WRITE (LP,160)
               CALL SULINE (LP,5)
               ENDIF
            WRITE (LP,170)
     *         NNUMID,NNUMRR,
     *         STAID,
     *         NBRSTA,
     *         DTYPE,
     *         UNITS,
     *         RSDATE,
     *         MRDATE,
     *         NUMREP,
     *         XLINE(1:LENSTR(XLINE))
            CALL SULINE (LP,1)
50       CONTINUE
C
      WRITE (LP,180) NUMIDR,NUMID
      CALL SULINE (LP,2)
      WRITE (LP,190) NUMRR,MAXRR
      CALL SULINE (LP,2)
C
60    IF (NUMERR.GT.0) ISTAT=1
C
70    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,200) ISTAT
         CALL SULINE (LP,2)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    FORMAT (' *** ENTER SMPPDR')
90    FORMAT ('0*** NOTE - NO STATIONS ARE DEFINED.')
100   FORMAT ('0*** NOTE - NO STATIONS WITH RRS DATA ARE DEFINED.')
110   FORMAT ('0- STATISTICS FOR RRS STATIONS -')
120   FORMAT ('0*** ERROR - IN SMPPDR - MAXIMUM NUMBER OF IDENTIFIERS ',
     *   'THAT CAN BE PROCESSED (',I5,') EXCEEDED.')
130   FORMAT ('0*** ERROR - IN SMPPDR - NUMBER OF WORDS NEEDED IN ',
     *   'ARRAY TO READ RRS RECORDS (',I5,') EXCEEDS ARRAY ',
     *   'DIMENSION (',I5,').')
140   FORMAT ('0*** ERROR - IN SMPPDR - CALLING ROUTINE ',A,'. ',
     *    'STATUS CODE=',I3)
150   FORMAT ('0*** ERROR - IN SMPPDR - DAIO READ ERROR - IN PDRRRR ',
     *   'AT RECORD ',I5,'.')
160   FORMAT (
     *   '0',4X,' ',1X,1X,
     *      '        ',1X,
     *      '      ',1X,
     *      '    ',1X,
     *      '     ',1X,
     *      '        ',1X,
     *      'DATE OF ',1X,
     *      'TOTAL  ',1X,
     *      '        ',1X,
     *      '        ',1X,
     *      'SECOND  ',1X,
     *      'DATE OF ',1X,
     *      'SECOND  ',1X,
     *      '        ',1X,
     *      '        ',1X,
     *      'DATE OF ' /
     *   ' ',4X,' ',1X,1X,
     *      '     STA',
     *      'TION   ',1X,
     *      'DATA',1X,
     *      '     ',1X,
     *      'DATE    ',1X,
     *      'MOST    ',1X,
     *      'NUMBER ',1X,
     *      'LARGEST ',1X,
     *      'DATE OF ',1X,
     *      'LARGEST ',1X,
     *      'SECOND  ',1X,
     *      'SMALLEST',1X,
     *      'DATE OF ',1X,
     *      'SMALLEST',1X,
     *      'SECOND  ' /
     *   ' ',4X,' ',1X,1X,
     *      'ID      ',1X,
     *      'NUMBER',1X,
     *      'TYPE',1X,
     *      'UNITS',1X,
     *      'RESET   ',1X,
     *      'RECENT  ',1X,
     *      'REPORTS',1X,
     *      'REPORT  ',1X,
     *      'LARGEST ',1X,
     *      'REPORT  ',1X,
     *      'LARGEST ',1X,
     *      'REPORT  ',1X,
     *      'SMALLEST',1X,
     *      'REPORT  ',1X,
     *      'SMALLEST' /
     *   ' ',4X,' ',1X,1X,
     *      '--------',1X,
     *      '------',1X,
     *      '----',1X,
     *      '-----',1X,
     *      '--------',1X,
     *      '--------',1X,
     *      '-------',1X,
     *      '--------',1X,
     *      '--------',1X,
     *      '--------',1X,
     *      '--------',1X,
     *      '--------',1X,
     *      '--------',1X,
     *      '--------',1X,
     *      '--------'
     *   )
170   FORMAT (
     *   1X,I4,'-',I1,1X,
     *   A,1X,
     *   I6,1X,
     *   A,1X,
     *   A,' ',1X
     *   A,1X,
     *   A,1X,
     *   I7,1X,
     *   A)
180   FORMAT ('0*** NOTE - ',I5,' OF THE ',I5,' ',
     *   'STATIONS DEFINED HAVE RRS DATA TYPES.')
190   FORMAT ('0*** NOTE - ',I5,' RRS TYPES PROCESSED. ',
     *   'A MAXIMUM OF ',I5,' CAN BE PROCESSED.')
200   FORMAT (' *** EXIT SMPPDR : STATUS CODE=',I2)
C
      END
