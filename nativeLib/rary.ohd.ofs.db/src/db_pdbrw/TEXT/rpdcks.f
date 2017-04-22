C MEMBER RPDCKS
C  (from old member PDBUTILS)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 10/20/94.16:16:57 BY $WC21DT
C
C @PROCESS LVL(77)
C
      SUBROUTINE RPDCKS (NDLYTP,IDLYTP,NRRSTP,IRRSTP,ISTAT)
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE CHECKS TO DETERMINE IF THERE IS ROOM ON THE
C    PREPROCESSOR DATA BASE TO ADD A STATION TO THE STATION
C    INFORMATION RECORD PART OF THE INDEX FILE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       NDLYTP     I     I     1     NUMBER OF DLY DATA TYPES
C       IDLYTP     A4    I   NDLYTP  DLY DATA TYPES
C       NRRSTP     I     I     1     NUMBER OF RRS DATA TYPES
C       IRRSTP     A4    I   NRRSTP  RRS DATA TYPES
C       ISTAT     I     O     1      STATUS INDICATOR
C                                      0=OK
C                                      1=SPACE NOT AVAILABLE
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdbdta'
      INCLUDE 'pdbcommon/pdsifc'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION IDLYTP(1),IRRSTP(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/rpdcks.f,v $
     . $',                                                             '
     .$Id: rpdcks.f,v 1.1 1995/09/17 18:44:35 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA LPP24/4HPP24/
      DATA LTM24/4HTM24/
C
C***********************************************************************
C
C
      ISTAT=0
C
      IF (IPDTR.GT.0) WRITE (IOGDB,60)
C
      NWRDS=10
C
      IF (NDLYTP.EQ.0.AND.NRRSTP.EQ.0) GO TO 50
C
      IF (NDLYTP.EQ.0) GO TO 20
C
C  PROCESS EACH DLY TYPE
      DO 10 I=1,NDLYTP
C     CHECK IF TYPE IS VALID
         IX=IPDCKD(IDLYTP(I))
         IF (IX.EQ.0) THEN
            IF (IPDDB.GT.0) WRITE (LPE,70) IDLYTP(I)
            GO TO 10
            ENDIF
         IF (IDLYTP(I).EQ.LPP24.OR.IDLYTP(I).EQ.LTM24) GO TO 10
         NWRDS=NWRDS+2
         IF (IPDDB.GT.0) WRITE (IOGDB,80) IDLYTP(I),NWRDS
10       CONTINUE
C
20    IF (NRRSTP.EQ.0) GO TO 40
C
C  PROCESS EACH RRS TYPE
      DO 30 I=1,NRRSTP
C     CHECK IF TYPE IS VALID
         IX=IPDCKR(IRRSTP(I))
         IF (IX.EQ.0) THEN
            IF (IPDDB.GT.0) WRITE (LPE,70) IRRSTP(I)
            GO TO 30
            ENDIF
         NWRDS=NWRDS+3
         IF (IPDDB.GT.0) WRITE (IOGDB,90) IRRSTP(I),NWRDS
30       CONTINUE
C
C  ADD WORDS FOR STATISTICS
40    NWRDS=NWRDS+NDSTAT
C
C  GET NUMBER OF RECORDS
      LRCPD2=LRCPDI*2
      NREC=IUNRCD(NWRDS,LRCPD2)
C
C  SET NEXT AVAILABLE RECORD
      IREC=LSTSIF+1
C
      IF (IPDDB.GT.0) WRITE (IOGDB,100) IREC,NREC,MXSIFR
C
C  CHECK IF ENOUGH ROOM IN FILE
      IF (IREC+NREC.GT.MXSIFR) ISTAT=1
C
50    IF (IPDTR.GT.0) WRITE (IOGDB,110) ISTAT
C
      RETURN
C
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT (' *** ENTER RPDCKS')
70    FORMAT (' **ERROR** INVALID DATA TYPE : ',A4)
80    FORMAT (' IDLYTP(I)=',A4,3X,'NWRDS=',I3)
90    FORMAT (' IRRSTP(I)=',A4,3X,'NWRDS=',I3)
100   FORMAT (' IREC=',I5,3X,'NREC=',I3,3X,'MXSIFR=',I5)
110   FORMAT (' *** EXIT RPDCKS - ISTAT=',I2)
C
      END
