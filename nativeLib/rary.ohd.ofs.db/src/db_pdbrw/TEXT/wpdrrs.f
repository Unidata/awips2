C MODULE WPDRRS
C-----------------------------------------------------------------------
C
      SUBROUTINE WPDRRS (STAID,IDTYPE,NTYPES,IDTYPS,NVLPOB,UNITS,
     *   NUMOBS,LOBS,OBS,LMIN,MIN,LWBUFF,IWBUFF,IWRITE,IFUT,
     *   LSTHR,IREV,ISTAT)
C
C  THIS ROUTINE WRITES RIVER, RESERVOIR, OR SNOW (RRS) DATA
C  FOR ONE STATION TO THE PREPROCESSOR DATA BASE AND UPDATES
C  THE STATION STATISTICS.  THE ROUTINE ALSO DELETES DATA.
C
C  ARGUMENT LIST:
C
C       NAME      TYPE  I/O   DIM   DESCRIPTION
C       ----      ----  ---   ---   -----------
C       STAID   A8 OR I  I  1 OR 2  STATION ID OR NUMBER
C       IDTYPE     I     I     1    ID TYPE INDICATOR:
C                                     0=ID
C                                     1=NUMBER
C       NTYPES     I     I     1    NUMBER OF DATA TYPES
C       IDTYPS     A4    I  NTYPES  DATA TYPES
C       NVLPOB     I     I  NTYPES  NUMBER OF VALUES PER OBSERVATION
C       UNITS      A4    I  NTYPES  UNITS FOR EACH DATA TYPE
C       NUMOBS     I     I  NTYPES  NUMBER OF OBSERVATIONS
C       LOBS       I     I     1    LENGTH OF OBS
C       OBS        R     I   LOBS   DATA VALUES
C       LMIN       I     I     1    LENGTH OF ARRAY MIN
C       MIN        I     I   LMIN   MINUTES FOR INSTANTANEOUS VALUES
C       LWBUFF     I     I     1    LENGTH OF WORK ARRAY
C       IWBUFF     I    I/O LWBUFF  WORK ARRAY
C       IWRITE     I     O  NTYPES  STATUS INDICATOR FOR EACH TYPE:
C                                     -1=SOME DATA NOT WRITTEN
C                                      0=ALL DATA WRITTEN
C                                      1=DATA TYPE NOT FOUND
C                                      2=NO DATA WRITTEN
C                                      3=WRONG NVLPOB
C                                      4=BAD UNITS CONVERSION
C                                      5=FUTURE DATA BEFORE OBSERVED
C                                      6=DATA EXISTS - NOT WRITTEN
C                                      7=INVALID MINUTES
C                                      8=VALUE OUT OF RANGE
C                                      9=DATA NOT FOUND - NOT DELETED
C       IFUT       I     I     1    FUTURE WRITE FLAG:
C                                      0=NOT FUTURE
C                                      1=FUTURE
C       LSTHR      I     O  NTYPES  ARRAY FOR LAST HOUR OF OBSERVED
C       IREV       I     I     1    REVISION WRITE INDICATOR:
C                                      0=NOT REVISION
C                                      1=REVISION
C       ISTAT      I     O     1    STATUS INDICATOR:
C                                      0=OK
C                                      1=STAID NOT FOUND
C                                      2=SOME DATA TYPES NOT FOUND OR
C                                        WRONG NVLPOB OR UNITS
C                                      3=SOME DATA NOT WRITTEN
C                                      4=SYSTEM ERROR
C                                      5=COMBINATION OF 2 AND 3
C                                      6=WORK ARRAY TOO SMALL
C                                      7=FUTURE DATA BEFORE OBSERVED
C                                      27,37,57,87  COMBINATIONS
C                                      8=DATA NOT WRITTEN, INVALID
C                                          MINUTES, AND BAD VALUES
C                                      28,38,58,78  COMBINATIONS
C                                      9=NOT ENOUGH MINUTE VALUES
C                                      10=INVALID IFUT OR IREV VALUE
C                                      11=DATA TO BE DELETED NOT FOUND
C
      INTEGER STAID(1)
      DIMENSION IDTYPS(NTYPES),NVLPOB(NTYPES),UNITS(NTYPES)
      DIMENSION NUMOBS(NTYPES),LSTHR(NTYPES),IWRITE(NTYPES)
      DIMENSION OBS(LOBS),MIN(LMIN),IWBUFF(LWBUFF)
      PARAMETER (LSIBUF=128)
      INTEGER*2 ISIBUF(LSIBUF)
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdrrsc'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/wpdrrs.f,v $
     . $',                                                             '
     .$Id: wpdrrs.f,v 1.4 2002/02/11 19:54:09 dws Exp $
     . $' /
C    ===================================================================
C      
C
      IF (IPDTR.GT.0) WRITE (IOGDB,310)
C
      ISTAT=0
C
      RMSNG=-999.0
C
      CALL UMEMST (0,IWRITE,NTYPES)
C
C  CHECK FOR VALID FUTURE AND REVISION INDICATORS
      IF ((IFUT.EQ.0.OR.IFUT.EQ.1).AND.(IREV.EQ.1.OR.IREV.EQ.0))
     *    GO TO 10
         ISTAT=10
         GO TO 300
C
10    MAXBUF=LWBUFF-2*IFREEL
C
C  GET STATION INFORMATION RECORD
      IF (IDTYPE.EQ.1) GO TO 20
C
C  STATION IDENTIFIER PASSED
      CALL PDFNDR (STAID,LSIBUF,IFIND,IREC,ISIBUF,IFREE,ISTA)
      GO TO 30
C
C  STATION NUMBER PASSED
20    CALL PDFNDI (STAID,LSIBUF,IFIND,IREC,ISIBUF,IFREE,ISTA)
C
30    IF (ISTA.NE.0) GO TO 290
      IF (IFIND.NE.0) GO TO 40
C
C  STATION NOT FOUND
      ISTAT=1
      IF (IPDDB.GT.1) WRITE (IOGDB,320)
      GO TO 300
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    IMPOS=0
      II=1
C
C   PROCESS EACH DATA TYPE
C
      DO 230 I=1,NTYPES
         ISKIP=NVLPOB(I)
         IEND=II+NUMOBS(I)*ISKIP-1
         IREC=IPDFDT(ISIBUF,IDTYPS(I))
         IF (IREC.NE.0) GO TO 50
            IWRITE(I)=1
            IF (IPDDB.GT.1) WRITE (IOGDB,330) IDTYPS(I)
            GO TO 220
C     READ THE RRS RECORD
50       CALL PDRRRR (IREC,LRCPDR,MAXBUF,IWBUFF,ISTAT)
         IF (ISTAT.EQ.2) THEN
            ISTAT=6
            GO TO 300
            ENDIF
         IF (ISTAT.NE.0) GO TO 290
         IF (IPDDB.GT.1) WRITE (IOGDB,340) (IWBUFF(M),M=1,17)
         IF (IDTYPS(I).NE.IWBUFF(5)) GO TO 290
C     CHECK NUMBER OF VALUES PER OBSERVATION SET
         IF (NVLPOB(I).EQ.IWBUFF(14)) GO TO 60
            IWRITE(I)=3
            IF (IPDDB.GT.1) WRITE (IOGDB,350)
            GO TO 220
C     CHECK FOR UNITS CONVERSION
60       CALL PDCKCV (UNITS(I),IDTYPS(I),1,ICONVT,FACTOR,TFACT,ISTA)
         IF (ISTA.EQ.0) GO TO 70
            IWRITE(I)=4
            GO TO 220
C     DELETE APPROPRIATE FUTURE DATA
70       CALL PDDFUT (IFUT,IREV,IWBUFF,IWBUFF(MAXBUF+1),ISTAT)
         IF (ISTAT.NE.0) GO TO 290
         CALL PDDOFD (IFUT,IREV,NUMOBS(I),OBS(II),IWBUFF,
     *      IWBUFF(MAXBUF+1),ISTAT)
         IF (ISTAT.NE.0) GO TO 290
         IFHOUR=0
C     LOOP ON EACH OBSERVATION SET
         DO 210 IPOS=II,IEND,ISKIP
            CALL UMEMOV (OBS(IPOS),ITIM,1)
            IF (IPDDB.GT.1) WRITE (IOGDB,360) ITIM,IFUT,IREV,NUMOBS
            IF (ITIM.NE.0) GO TO 80
               WRITE (LP,370) II
               GO TO 130
80          VAL=OBS(IPOS+1)
            IPER=0
            IF (NVLPOB(I).EQ.3) CALL UMEMOV (OBS(IPOS+2),IPER,1)
            IF (ICONVT.EQ.1) CALL PDCVT1 (FACTOR,TFACT,RMSNG,VAL)
C        CHECK INDICATORS AND RANGES
            CALL PDCKWR (MIN,LMIN,IMPOS,IMIN,IWBUFF,IFUT,IREV,IFHOUR,
     *         ITIM,VAL,ISTAT)
            IF (ISTAT.EQ.0) GO TO 90
            IF (ISTAT.EQ.9) GO TO 300
               IWRITE(I)=ISTAT
               GO TO 210
90          IF (ITIM.GT.0) GO TO 110
C        DELETE DATA -   -   -   -   -   -   -   -   -   -   -   -   -
            IF (-ITIM.GE.JULMIN(ISKIP,IWBUFF(IWBUFF(9)))) GO TO 100
C        CHECK IN FREE POOL RECORDS
            IF (IWBUFF(13).EQ.0) GO TO 210
            IF (-ITIM.LT.IWBUFF(15)) GO TO 210
            CALL PDFVFR (0,ITIM,VAL,IPER,NVLPOB(I),IMIN,IFUT,IREV,
     *         IWBUFF,IWBUFF(MAXBUF+1),ISTAT)
            IF (ISTAT.EQ.0) GO TO 200
            IF (ISTAT.EQ.1) IWRITE(I)=9
            IF (ISTAT.EQ.2) GO TO 285
            GO TO 200
C        CHECK IN REGULAR RECORD
100         CALL PDFVRR (ITIM,VAL,IPER,NVLPOB(I),IMIN,IFUT,IREV,
     *         IWBUFF,IWBUFF(MAXBUF+1),ISTAT)
            IF (ISTAT.EQ.0) GO TO 200
            IF (ISTAT.EQ.1) IWRITE(I)=9
            IF (ISTAT.EQ.2) GO TO 290
            GO TO 200
C        ADD VALUE TO END OF DATA -   -   -   -   -   -   -   -   -   -
110         IF (IWBUFF(8).EQ.0) GO TO 140
            IF (ITIM.LE.JULMIN(ISKIP,IWBUFF(IWBUFF(11)))) GO TO 150
            GO TO 140
C        CHECK FOR MINDAYS
120         WRITE (LP,380) IWBUFF(6),IWBUFF(5),IWBUFF(2),IWBUFF(3)
            JDAY=(ITIM-13)/24+1
            JHR=(ITIM-LOCAL+NLSTZ)-((JDAY-1)*24)
            IF (IPDDB.GT.0) WRITE (IOGDB,*) 'ITIM=',ITIM
            IF (IPDDB.GT.0) WRITE (IOGDB,385) LOCAL,NLSTZ,JDAY,JHR
            CALL MDYH2 (JDAY,JHR,IMO,IDA,IYR,IHR,NX1,NX2,'Z   ' )
            I1PRIM=JULMIN(ISKIP,IWBUFF(IWBUFF(9)))
            JDAY=(I1PRIM-13)/24+1
            JHR=(I1PRIM-LOCAL+NLSTZ)-((JDAY-1)*24)
            IF (IPDDB.GT.0) WRITE (IOGDB,*) 'I1PRIM=',I1PRIM
            IF (IPDDB.GT.0) WRITE (IOGDB,385) LOCAL,NLSTZ,JDAY,JHR
            CALL MDYH2 (JDAY,JHR,KMO,KDA,KYR,KHR,NX1,NX2,'Z   ' )
            WRITE (LP,390) IMO,IDA,IYR,IHR
            WRITE (LP,400) KMO,KDA,KYR,KHR
            I1FREE=IWBUFF(15)
            IF (I1FREE.EQ.0) GO TO 130
               JDAY=(I1FREE-13)/24+1
               JHR=(I1FREE-LOCAL+NLSTZ)-((JDAY-1)*24)
               IF (IPDDB.GT.0) WRITE (IOGDB,*) ' I1FREE=',I1FREE
               IF (IPDDB.GT.0) WRITE (IOGDB,385) LOCAL,NLSTZ,JDAY,JHR
               CALL MDYH2 (JDAY,JHR,LMO,LDA,LYR,LHR,NX1,NX2,'Z   ' )
               WRITE (LP,410) LMO,LDA,LYR,LHR
130         IF (IWRITE(I).EQ.9) IWRITE(I)=2
            IF (IWRITE(I).EQ.0) IWRITE(I)=-1
            GO TO 210
C        MOVE TO END OF DATA
140         CALL PDMVAF (ITIM,VAL,IPER,ISKIP,IMIN,IFHOUR,IWBUFF,
     *         IWBUFF(MAXBUF+1),ISTAT)
            IF (ISTAT.NE.0) GO TO 290
            CALL PDSTAR (0,ITIM,VAL,0,IMIN,IFUT,IWBUFF,
     *         IWBUFF(MAXBUF+1),ISTAT)
            IF (ISTAT.EQ.1) GO TO 190
            IF (ISTAT.NE.0) GO TO 290
            GO TO 200
150         IF (ITIM.GE.JULMIN(ISKIP,IWBUFF(IWBUFF(9)))) GO TO 170
C        ADD VALUE BEFORE -   -   -   -   -   -   -   -   -   -   -   -
            IF (IWBUFF(13).EQ.0) GO TO 160
C        PUT IT IN FREE POOL RECORD
            IF (ITIM.GE.IWBUFF(15)) GO TO 170
            IF (IWBUFF(15)-ITIM.GE.IWBUFF(6)*24) GO TO 120
            CALL PDRDFR (IWBUFF(13),IWBUFF(MAXBUF+1),ISTAT)
            IF (ISTAT.NE.0) GO TO 285
            CALL PDMVFR (ITIM,VAL,IPER,ISKIP,IMIN,IWBUFF(13),3,
     *         IWBUFF(MAXBUF+1),ISTAT)
            IF (ISTAT.NE.0) GO TO 285
            IWBUFF(15)=ITIM
            CALL PDSTAR (0,ITIM,VAL,0,IMIN,IFUT,IWBUFF,
     *        IWBUFF(MAXBUF+1),ISTAT)
            IF (ISTAT.EQ.1) GO TO 190
            IF (ISTAT.NE.0) GO TO 290
            GO TO 200
C        DATA IS BEFORE WITH NO FREE POOL
160         IF (JULMIN(ISKIP,IWBUFF(IWBUFF(9)))-ITIM.GE.IWBUFF(6)*24)
     *          GO TO 120
            CALL PDMVBF (ITIM,VAL,IPER,ISKIP,IMIN,IWBUFF,
     *         IWBUFF(MAXBUF+1),ISTAT)
            IF (ISTAT.NE.0) GO TO 290
            CALL PDSTAR (0,ITIM,VAL,0,IMIN,IFUT,IWBUFF,
     *         IWBUFF(MAXBUF+1),ISTAT)
            IF (ISTAT.EQ.1) GO TO 190
            IF (ISTAT.NE.0) GO TO 290
            GO TO 200
C        DATA IS IN RANGE OF EXISTING DATA -   -   -   -   -   -   -   -
170         IF (ITIM.LT.JULMIN(ISKIP,IWBUFF(IWBUFF(9)))) GO TO 180
            CALL PDFVRR (ITIM,VAL,IPER,ISKIP,IMIN,IFUT,IREV,IWBUFF,
     *         IWBUFF(MAXBUF+1),ISTAT)
            IF (ISTAT.EQ.1) GO TO 190 
            IF (ISTAT.EQ.2) GO TO 290
            GO TO 200
C        DATA IS IN FREE POOL RECORDS
180         CALL PDFVFR (0,ITIM,VAL,IPER,ISKIP,IMIN,IFUT,IREV,IWBUFF,
     *         IWBUFF(MAXBUF+1),ISTAT)
            IF (ISTAT.EQ.1) GO TO 190 
            IF (ISTAT.EQ.2) GO TO 285
            GO TO 200
C        DATA NOT WRITTEN
190         IWRITE(I)=6
            GO TO 210
C     SET WRITE INDICATOR
200         IF (IWRITE(I).EQ.2) IWRITE(I)=-1
210         CONTINUE
C     CLEAN UP FREE POOL RECORDS
         CALL PDDLFR (IFHOUR,IWBUFF,IWBUFF(MAXBUF+1),ISTAT)
         IF (ISTAT.NE.0) GO TO 285
C     WRITE THE RRS RECORD
         CALL PDWRRR (IREC,LRCPDR,IWBUFF,ISTAT)
         IF (ISTAT.NE.0) GO TO 290
         IF (IPDDB.GT.1) CALL PDPRRR (IWBUFF,NLINE,ISTAT)
220      II=IEND+1
         LSTHR(I)=IWBUFF(16)
230      CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SET THE STATUS CODES
      ISTAT=0
      DO 280 I=1,NTYPES
         IF (IWRITE(I).EQ.0) GO TO 280
         IF (IWRITE(I).EQ.1.OR.IWRITE(I).EQ.3.OR.IWRITE(I).EQ.4) THEN
            IF (ISTAT.EQ.3) ISTAT=5
            IF (ISTAT.EQ.0) ISTAT=2
            GO TO 280
            ENDIF
         IF (IWRITE(I).EQ.-1.OR.IWRITE(I).EQ.2) THEN
            IF (ISTAT.EQ.0) ISTAT=3
            IF (ISTAT.EQ.2) ISTAT=5
            GO TO 280
            ENDIF
         IF (IWRITE(I).EQ.5) THEN
            ISTAT=ISTAT*10+7
            GO TO 280
            ENDIF
         IF (IWRITE(I).EQ.6.OR.IWRITE(I).EQ.7.OR.IWRITE(I).EQ.8) THEN
            ISTAT=ISTAT*10+8
            ENDIF
         IF (IWRITE(I).EQ.9) THEN
            ISTAT=11
            ENDIF
280      CONTINUE
      GO TO 300
C
C  SYSTEM ERROR READING FREEPOOL RECORDS
285   IF (IDTYPE.EQ.0) THEN
         WRITE (LP,420) 'FREEPOOL',IDTYPS(I),(STAID(N),N=1,2),IPUSER
         ENDIF
      IF (IDTYPE.EQ.1) THEN
         WRITE (LP,425) 'FREEPOOL',IDTYPS(I),STAID,IPUSER
         ENDIF
      ISTAT=4
      GO TO 300
C
C  SYSTEM ERROR READING NON-FREEPOOL RECORDS
290   IF (IDTYPE.EQ.0) THEN
         WRITE (LP,420) 'NON-FREEPOOL',IDTYPS(I),(STAID(N),N=1,2),IPUSER
         ENDIF
      IF (IDTYPE.EQ.1) THEN
         WRITE (LP,425) 'NON-FREEPOOL',IDTYPS(I),STAID,IPUSER
         ENDIF
      ISTAT=4
      GO TO 300
C
300   IF (IPDDB.GT.1) WRITE (IOGDB,430) (IWRITE(I),I=1,NTYPES)
C
      IF (IPDTR.GT.0) WRITE (IOGDB,440) ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
310   FORMAT (' *** ENTER WPDRRS')
320   FORMAT (' STATION NOT FOUND')
330   FORMAT (' DATA TYPE ',A4,' NOT FOUND IN SIF RECORD')
340   FORMAT (' RRS RECORD=',I4,1X,2A4,1X,I6,1X,A4,9(1X,I4),1X,
     *   I8,I8,I3)
350   FORMAT (' NUMBER OF VALUES PER OBSERVATION DIFFERENT')
360   FORMAT (' ITIM,IFUT,IREV,NUMOBS=',I11,2I2,I5)
370   FORMAT ('0**WARNING** TIME FOR OBSERVATION ',I4,' IS ZERO.')
380   FORMAT ('0**WARNING** VALUE NOT WRITTEN TO PRESERVE ',
     *   I4,' DAYS OF DATA FOR DATA TYPE ',A4,' FOR STATION ',2A4,'.')
385   FORMAT (' LOCAL=',I2,3X,'NLSTZ=',I2,3X,
     *   'JDAY=',I6,3X,'JHR=',I3)
390   FORMAT (T14,'DATE OF DATA BEING WRITTEN = ',
     *   I2.2,'/',I2.2,'/',I4.4,'-',I2.2,'Z')
400   FORMAT (T14,'DATE OF EARLIEST PRIMARY DATA = ',
     *   I2.2,'/',I2.2,'/',I4.4,'-',I2.2,'Z')
410   FORMAT (T14,'DATE OF EARLIEST FREE POOL DATA = ',
     *   I2.2,'/',I2.2,'/',I4.4,'-',I2.2,'Z')
420   FORMAT ('0**ERROR** IN WPDRRS - READING ',A,' ',A4,
     *   ' RECORDS FOR STATION ',2A4,' FOR USER ',2A4,'.')
425   FORMAT ('0**ERROR** IN WPDRRS - READING ',A,' ',A4,
     *   ' RECORDS FOR STATION ',I6,' FOR USER ',2A4,'.')
430   FORMAT (' IWRITE=',20I6)
440   FORMAT (' *** EXIT WPDRRS : ISTAT=',I2)
C
      END
