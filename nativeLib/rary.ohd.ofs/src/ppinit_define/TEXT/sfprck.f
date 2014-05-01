C MODULE SFPRCK
C-----------------------------------------------------------------------
C
C  ROUTINE TO CHECK AVAILABLE SPACE IN PROCESSED DATA BASE.
C
      SUBROUTINE SFPRCK (STAID,NGPSN,GPSN,
     *   NRRSTP,RRSTYP,URMISS,IRTIME,IRSTAT,
     *   NUMERR,ISTAT)
C
      CHARACTER*4 DTYPE
      CHARACTER*4 GPSN(NGPSN)
      CHARACTER*8 STAID,FTSID
C
      DIMENSION IRSTAT(NRRSTP)
      PARAMETER (MRUSD=5)
      DIMENSION NRUSD(MRUSD)
      DIMENSION IHEAD(22)

      INCLUDE 'scommon/dimrrs'
C
      INCLUDE 'uio'
      INCLUDE 'prdcommon/ptsctl'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/pdatas'
      INCLUDE 'prdcommon/punits'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfprck.f,v $
     . $',                                                             '
     .$Id: sfprck.f,v 1.2 1998/07/06 12:21:25 page Exp $
     . $' /
C    ===================================================================
C
C
C
C  SET TRACE LEVEL
      LTRACE=ISTRC('STA ')
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,50)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('STA ')
C
      ISTAT=0
C
      IF (NMPRDF.GT.MRUSD) THEN
         WRITE (LP,60) NMPRDF,MRUSD
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 40
         ENDIF
      CALL UMEMST (0,NRUSD,NMPRDF)
C
C  PROCESS EACH PARAMETER TYPE
      DO 30 I=1,NGPSN
         IF (GPSN(I).EQ.'RRS') THEN
C        PROCESS EACH RRS DATA TYPE
            DO 20 J=1,NRRSTP
C           CHECK IF TIME SERIES NOT TO BE CREATED
               IF (IRTIME(J).EQ.-1) GO TO 20
C           CHECK IF TIME SERIES EXISTS AND HAS SAME TIME INTERVAL
               IF (IRSTAT(J).EQ.1) GO TO 20
               INEW=0
C           CHECK IF TIME SERIES DOES NOT EXIST
               IF (IRSTAT(J).EQ.0) INEW=1
C           CHECK IF TIME SERIES EXISTS AND HAS COMPATIBLE TIME 
C           INTERVAL
               IF (IRSTAT(J).EQ.2) INEW=1
C           CHECK IF TIME SERIES EXISTS BUT HAS INCOMPATIBLE DATA TIME 
C           INTERVAL
               IF (IRSTAT(J).EQ.3) INEW=1
C           SET DATA TYPE         
               DTYPE=RRSTYP(J)
               IF (URMISS(J).NE.'SAME') DTYPE=URMISS(J)
C           SET DATA TIME INTERVAL
               ITSTEP=IRTIME(J)
C           CHECK IF TIME SERIES EXISTS
               LXBUF=1
               CALL RPRDH (STAID,DTYPE,LXBUF,IHEAD,NXBUF,XBUF,FTSID,
     *            IERR)
               IF (IERR.EQ.0) THEN
C              TIME SERIES EXISTS - CHECK DATA TIME INTERVAL
                  IF (IHEAD(2).NE.ITSTEP) THEN
                     INEW=1
                     GO TO 10
                     ENDIF
                  GO TO 20
                  ENDIF
               IF (IERR.NE.1) THEN
                  CALL SRPRST ('RPRDFH  ',STAID,DTYPE,LXBUF,NUMERR,IERR)
                  GO TO 20
                  ENDIF
10             IF (INEW.EQ.0) GO TO 20
C           FIND TYPE IN DIRECTORY
               CALL PFDTYP (DTYPE,INDEXD)
               IF (INDEXD.EQ.0) THEN
                  WRITE (LP,70) DTYPE,STAID
                  CALL SUERRS (LP,2,NUMERR)
                  ISTAT=1
                  GO TO 20
                  ENDIF
C           GET NUMBER OF VALUES PER TIME INTERVAL
               NPERIT=DATFIL(13,INDEXD)
C           GET MAXIMUM NUMBER OF DAYS OF DATA
               MAXDAY=DATFIL(4,INDEXD)
C           COMPUTE MAXIMUM NUMBER OF DATA VALUES
               MAXVAL=MAXDAY*(24/ITSTEP)*NPERIT
C           SET NUMBER OF WORDS IN EXTRA BUFFER
               LXBUF=0
C           COMPUTE NUMBER OF WORDS NEEDED
               NWORDS=LENHDC+LXBUF+MAXVAL
C           COMPUTE NUMBER OF RECORDS NEEDED
               NREC=(NWORDS+LRECLT-1)/LRECLT
C           SET UNIT NUMBER
               LUNIT=DATFIL(2,INDEXD)
C           GET FILE NUMBER
               CALL PRGTIX (LUNIT,IFILE,IERR)
C           SET MAXIMUM NUMBER OF RECORDS IN FILE
               NRMAX=TSCNTR(2,IFILE)
C           SET NUMBER OF RECORDS USED IN FILE
               IF (NRUSD(IFILE).EQ.0) THEN
                  NRUSD(IFILE)=TSCNTR(3,IFILE)+NREC
                  ELSE
                     NRUSD(IFILE)=NRUSD(IFILE)+NREC
                  ENDIF
C           CHECK IF THERE IS ROOM IN FILE
               IF (NRUSD(IFILE).GT.NRMAX) THEN
                  WRITE (LP,80) IFILE,DTYPE,STAID
                  CALL SUERRS (LP,2,NUMERR)
                  ISTAT=1
                  GO TO 20
                  ENDIF
20             CONTINUE
            ENDIF
30       CONTINUE
C
40    IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,90) ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (' *** ENTER SFPRCK')
60    FORMAT ('0*** ERROR - IN SFPPCK - NUMBER OF TIME SERIES FILES (',
     *   I2,') EXCEEDS SIZE OF ARRAY NRUSD (',I2,').')
70    FORMAT ('0*** ERROR - IN SFPRCK - DATA TYPE ',A,
     *   ' NOT FOUND IN PROCESSED DATA BASE DIRECTORY ',
     *   'FOR STATION ',A,'.')
80    FORMAT ('0*** ERROR - IN SFPRCK - NOT ENOUGH SPACE IN ',
     *   'TIME SERIES FILE ',I2,' FOR RRS DATA TYPE ',A,' ',
     *   'FOR STATION ',A,'.')
90    FORMAT (' *** EXIT SFPRCK : ISTAT=',I2)
C
      END
