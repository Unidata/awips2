C MEMBER WPD24P
C  (from old member PDWPD24P)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 11/01/95.08:50:33 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE WPD24P (IDATE,LDATA,IDATA,LWORK,IWORK,ISTAT)
C
C          ROUTINE:  WPD24P
C
C             VERSION:  1.0.0
C
C                DATE:  2-21-84
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C  THIS ROUTINE WRITES ALL ESTIMATED 24-HOUR PRECIPITION DATA
C  TO THE PREPROCESSOR DATA BASE. THE PP24 STATISTICS ARE NOT UPDATED.
C
C  OBSERVED AND ESTIMATED OBSERVED DATA VALUES ARE GREATER THAN OR
C  EQUAL TO ZERO. VALUES ARE REPLACED IF MISSING OR ESTIMATED.
C
C  ESTIMATED NON-ZERO VALUES ARE LESS THAN ZERO AND GREATER THAN
C  -9998. ESTIMATED ZERO VALUES ARE EQUAL TO -9998. THESE VALUES
C  REPLACE MISSING OR ESTIMATED VALUES.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C        IDATE     I     I     1     JULIAN DAY OF DATA
C        LDATA     I     I     1     LENGTH OF IDATA ARRAY
C        IDATA    I*2    I   LDATA   ARRAY WITH DATA FOR ALL 24-HOUR
C                                    PRECIP STATIONS FOR IDATE
C                                      (NOT ENCODED)
C        LWORK     I     I     1     LENGTH OF ARRAY IWORK
C        IWORK    I*2    O   LWORK   WORK ARRAY
C        ISTAT     I     O     1     STATUS FLAG
C                                      0=NORMAL RETURN
C                                      1=TRIED TO REPLACE NONMISSING
C                                           VALUE ON FILE
C                                      2=IWORK ARRAY TOO SMALL
C
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdbdta'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER*2 IDATA(LDATA),IWORK(LWORK),IPNTRS(1)
C
      DIMENSION ISTAID(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/wpd24p.f,v $
     . $',                                                             '
     .$Id: wpd24p.f,v 1.2 1996/01/16 12:17:29 page Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA LPP24/4HPP24/
      DATA ISTAID/4HESTI,4HMATD/
C
C***********************************************************************
C
C
      ISTAT=0
C
      LRCPD2=LRCPDD*2
C
      IF (IPDTR.GT.0) WRITE (LP,*) '*** ENTER WPD24P'
C
C  READ DATA FOR SPECIFIED DAY
      IRETRN=0
      LPNTRS=1
      LENDAT=1
      CALL RPDDLY (LPP24,IDATE,IRETRN,LPNTRS,IPNTRS,LPFILL,LWORK,IWORK,
     *   LDFILL,NUMSTA,MSNG,LENDAT,IDATES,IERR)
C
C  CHECK SIZE OF WORK SPACE -
C  MUST BE BIG ENOUGH TO HOLD FULL RECORDS OF DATA SO WHEN
C  WVLRCD IS CALLED IT CAN BE FILLED WITH MISSING DATA
C  TO THE END OF THE LAST RECORD
      NREC=IUNRCD(LDFILL,LRCPD2)
      NWORDS=NREC*LRCPD2
      IF (LWORK.LT.NWORDS) GO TO 70
C
      IF (IERR.EQ.0.OR.IERR.EQ.1) GO TO 10
      IERR=IERR-1
      GO TO (70,90,110,130,170),IERR
C
C  FILL END OF IWORK ARRAY WITH MISSING
10    NFILL=NWORDS-LDFILL
      IF (NFILL.GT.0) THEN
         DO 20 I=1,NFILL
            IWORK(LDFILL+I)=MISSPP
20          CONTINUE
         ENDIF
C
C  FIND THE TYPE AND FILE SUBSCRIPT
      IDX=IPDCKD(LPP24)
      IF (IDX.EQ.0) GO TO 150
      IDF=IDDTDR(4,IDX)
C
C  CHECK FOR ESTIMATED VALUES IN DATA ARRAY
      DO 60 I=1,LDATA
         IF (IDATA(I).EQ.MISSNG) GO TO 60
C     CHECK VALUES - REPLACE IF ESTIMATED OR MISSING
         IF (IDATA(I).LT.0) GO TO 30
            CALL PDGTPP (IWORK(I),PP24,IHR,IPP)
            IF (IHR.NE.24.AND.IWORK(I).NE.MISSPP) GO TO 60
            RDATA=IDATA(I)
            GO TO 40
30       IF (IDATA(I).EQ.-9998) THEN
            RDATA=0
            ELSE
               RDATA=-IDATA(I)
            ENDIF
40       RDATA=RDATA/100.0
C     INDEX IS UPDATED IN ROUTINE PDCVPP - SAVE OLD INDEX
         J=I
C     ENCODE DATA VALUE
         FACTOR=1
         IREV=1
         IHOUR=24
         CALL PDCVPP (RDATA,FACTOR,IREV,IHOUR,IWORK(I),J,ISTAID,IERR)
         IF (IERR.NE.20) GO TO 60
            ISTAT=1
            IF (IPDDB.GT.0) WRITE (LP,50) IWORK(I)
50    FORMAT ('0**ERROR** TRIED TO REPLACE NON-MISSING VALUE ',I6,
     *   ' ON FILE.')
60       CONTINUE
C
C  GET DATE OF EARLIEST OF DATA
      CALL UMEMOV (IDDTDR(8,IDX),IEDATE,1)
C
C  CALCULATE WHERE TO BEGIN WRITING NREC RECORDS
      CALL PDCREC (IDATE,IEDATE,IDX,IREC)
C
C  WRITE RECORDS
      CALL WVLRCD (KPDDDF(IDF),IREC,NREC,IWORK,LRCPDD,IERR)
      IF (IERR.NE.0) GO TO 170
C
      GO TO 190
C
C  ERROR CONDITIONS
C
70    ISTAT=2
      IF (IPDDB.GT.0) WRITE (LP,80) LWORK,NWORDS
80    FORMAT ('0**ERROR** IWORK ARRAY TOO SMALL FOR ONE DAY OF PP24 ',
     *   'DATA. ',
     *   'NUMBER OF WORDS IN IWORK IS ',I6,'. ',
     *   'NUMBER OF WORDS NEEDED IS ',I6,'.')
      GO TO 190
C
90    IF (IPDDB.GT.0) WRITE (LP,100)
100   FORMAT ('0**ERROR** POINTERS AND DATA ARRAYS TOO SMALL.')
      GO TO 190
C
110   IF (IPDDB.GT.0) WRITE (LP,120) IDATE
120   FORMAT ('0**ERROR** JULIAN DATE ',I5,' NOT FOUND FOR PP24 DATA.')
      GO TO 190
C
130   IF (IPDDB.GT.0) WRITE (LP,140) LPP24
140   FORMAT ('0**ERROR** ',A4,' IS AN INVALID DATA TYPE.')
      GO TO 190
C
150   IF (IPDDB.GT.0) WRITE (LP,160) LPP24
160   FORMAT ('0**ERROR** DATA TYPE ',A4,' NOT FOUND ON ',
     *   'PREPROCESSOR DATA BASE.')
      GO TO 190
C
170   IF (IPDDB.GT.0) WRITE (LP,180) IERR
180   FORMAT ('0**ERROR** SYSTEM ERROR ACCESSING FILE. ISTAT=',I2)
C
190   IF (IPDTR.GT.0) WRITE (LP,*) '*** EXIT WPD24P'
C
      RETURN
C
      END
