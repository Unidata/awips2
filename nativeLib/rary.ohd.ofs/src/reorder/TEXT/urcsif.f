C MODULE URCSIF
C-----------------------------------------------------------------------
C
      SUBROUTINE URCSIF (ISTAT)
C
C   THIS ROUTINE READS THE STATION INDEX RECORDS TO OBTAIN THE
C   RECORD NUMBER OF THE SIF RECORDS. THE INDEX ARRAYS WILL BE RESET
C   TO THE NEW RECORD NUMBER OF THE SIF RECORDS THAT WILL BE WRITTEN
C   TO THE NEW SET OF FILES.
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdhshc'
      INCLUDE 'pdbcommon/pdhshi'
      INCLUDE 'urcommon/ursifc'
      INCLUDE 'urcommon/urrrsc'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'urcommon/urcdta'
      INCLUDE 'urcommon/urhshc'
      INCLUDE 'urcommon/urhshi'
C
      PARAMETER (LSIBUF=128)
      INTEGER*2 ISIBUF(LSIBUF),ISIDUM(LSIBUF)
      CHARACTER*8 STAID     
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urcsif.f,v $
     . $',                                                             '
     .$Id: urcsif.f,v 1.6 2000/12/18 21:33:07 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.0) THEN
         WRITE (IOGDB,*) 'ENTER URCISF'
         CALL SULINE (IOGDB,1)
         ENDIF
C
      ISTAT=0
C
      LIDX=LRLURI*2
C
C  INITIALIZE HASH ARRAYS
      NURHSC=MURHSC/2
      CALL UMEMST (0,IURHSC,NURHSC)
      NURHSI=MURHSI/2
      CALL UMEMST (0,IURHSI,NURHSI)
C
      WRITE (LP,90)
      CALL SULINE (LP,2)
C
      IREC=INFREC+1
      JREC=ISIFRC+1
C
C  READ SIF RECORD
10    IAMORD=0
      CALL PDRSIF (IREC,NXREC,LSIBUF,ISIBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 50
C
C  SET STATION ID
      CALL UMEMOV (ISIBUF(2),STAID,2)
C
C  CHECK IF DELETED STATION
      IF (STAID.EQ.'DELETED') GO TO 45
C
      ICHKID=0
      IF (ICHKID.EQ.1) THEN
         IF (STAID.EQ.'ACGA4') THEN
            WRITE (LP,*) 'STAID=',STAID
            ENDIF
         ENDIF
C
C  SET STATION NUMBER
      NUMID=ISIBUF(6)
C
      IAMORD=1
C
C  GET LOCATION IN CHARACTER INDEX
      CALL PDFNDR (STAID,LSIBUF,IFINDC,ISIREC,ISIDUM,IFREEC,ISTAT)
      IF (ISTAT.NE.0) GO TO 50
      IF (IFINDC.EQ.0) GO TO 20
         WRITE (LP,100) STAID
         CALL SUERRS (LP,2,-1)
         GO TO 45
C
20    IF (NUMID.EQ.0) GO TO 30
C
C  GET LOCATION IN INTEGER INDEX
      CALL PDFNDI (NUMID,LSIBUF,IFINDI,ISIREC,ISIDUM,IFREEI,ISTAT)
      IF (ISTAT.NE.0) GO TO 50
      IF (IFINDI.EQ.0) GO TO 30
         WRITE (LP,110) NUMID
         CALL SUERRS (LP,2,-1)
         GO TO 45
C
30    IAMORD=0
C
C  COMPUTE NUMBER OF RECORDS FOR SIF IN NEW FILE
      NWORD=ISIBUF(1)
      NREC=IUNRCD(NWORD,LIDX)
C
      IF (IPDDB.GT.0) THEN
         WRITE (IOGDB,*)
     *      ' JREC=',JREC,
     *      ' NREC=',NREC,
     *      ' '
         CALL SULINE (IOGDB,1)
         ENDIF

C  RESET NUMBER OF WORDS IN OLD RECORD
      IPOS=NWORD+1
      IF (ISIBUF(IPOS).GT.0) ISIBUF(IPOS)=0
C
C  WRITE SIF TO NEW FILE
      CALL WVLRCD (KURSIF,JREC,NREC,ISIBUF,LRLURI,ISTAT)
      IF (ISTAT.NE.0) GO TO 50
C
C  UPDATE NUMBER OF STATIONS
      NUMSTA=NUMSTA+1
      IF (IPDDB.GT.0) THEN
         WRITE (IOGDB,*)
     *      ' NUMSTA=',NUMSTA,
     *      ' '
         CALL SULINE (IOGDB,1)
         ENDIF
C
C  SET HASH POINTERS TO NEW RECORD NUMBERS IN NEW HASH ARRAYS
      IURHSC(IFREEC)=JREC
      IF (NUMID.EQ.0) GO TO 40
      IURHSI(IFREEI)=JREC
      IF (IPDDB.GT.0) THEN
         WRITE (IOGDB,*)
     *      ' IFREEC=',IFREEC,
     *      ' IFREEI=',IFREEI,
     *      ' '
         CALL SULINE (IOGDB,1)
         ENDIF
C
C  INCREMENT LAST USED SIF RECORD IN NEW FILE
40    LTSIFR=LTSIFR+NREC
      JREC=LTSIFR+1
      IF (IPDDB.GT.0) THEN
         WRITE (IOGDB,*)
     *      ' LTSIFR=',LTSIFR,
     *      ' '
         CALL SULINE (IOGDB,1)
         ENDIF
C
C  CHECK IF LAST SIF RECORD
45    IF (IPDDB.GT.0) THEN
         WRITE (IOGDB,*)
     *      ' NXREC=',NXREC,
     *      ' LSTSIF=',LSTSIF,
     *      ' '
         CALL SULINE (IOGDB,1)
         ENDIF
      IF (NXREC.LE.LSTSIF) THEN
         IREC=NXREC
         GO TO 10
         ENDIF
C
      WRITE (LP,120) NUMSTA
      GO TO 70
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    WRITE (LP,60)
      CALL SUERRS (LP,2,-1)
60    FORMAT ('0*** ERROR - IN URCSIF - SYSTEM WRITE ERROR.')
C
70    IF (IPDTR.GT.0) THEN
        WRITE (IOGDB,*) 'EXIT URCISF'
        CALL SULINE (IOGDB,1)
        ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
90    FORMAT ('0*** NOTE - BEGIN TO COPY  << PPDB INDEX AND ',
     *   'STATION INFORMATION >>  RECORDS.')
100   FORMAT ('0*** ERROR - IN URCSIF - SIF RECORD FOR STATION ',2A4,
     *   ' NOT FOUND.')
110   FORMAT ('0*** ERROR - IN URCSIF - SIF RECORD FOR STATION ',I5,
     *   ' NOT FOUND.')
120   FORMAT ('0*** NOTE - PPDB INDEX AND STATION INFORMATION ',
     *   'RECORDS FOR ',I4,' STATIONS HAVE BEEN SUCCESSFULLY COPIED.')
C
      END
