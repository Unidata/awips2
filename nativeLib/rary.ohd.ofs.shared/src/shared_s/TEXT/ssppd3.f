C MODULE SSPPD3
C-----------------------------------------------------------------------
C
C   ROUTINE TO PRINT LEVEL 3 STATUS FOR PREPROCESSOR DATA BASE.
C
      SUBROUTINE SSPPD3 (LARRAY,IARRAY,ISTAT)
C
      CHARACTER*8 STAID
      PARAMETER (LSIBUF=128)
      INTEGER*2 ISIBUF(LSIBUF)
C
      DIMENSION IARRAY(3,1)
      DIMENSION IWORK(1)
      PARAMETER (MRRBF=2500)
      DIMENSION IRRBF1(MRRBF),IRRBF2(MRRBF)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sworkx'
      INCLUDE 'pdbcommon/pdrrsc'
      INCLUDE 'pdbcommon/pdsifc'
C
      EQUIVALENCE (IWORK(1),SWORK(1))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/ssppd3.f,v $
     . $',                                                             '
     .$Id: ssppd3.f,v 1.4 1998/07/06 12:16:22 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,120)
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
      IF (IERR.NE.0) THEN
         ISTAT=1
         GO TO 110
         ENDIF
C
C  CHECK IF ANY STATION DEFINED
      IF (NPDSTA.EQ.0) THEN
         WRITE (LP,130)
         CALL SULINE (LP,2)
         GO TO 110
         ENDIF
C
C  GET LIST OF PPDB IDENTIFIERS AND RECORD LOCATIONS
      NWORDS=3
      MAXID=LARRAY/NWORDS
      NUMID=0
      IREC=INFREC+1
10    CALL PDRSIF (IREC,NXREC,LSIBUF,ISIBUF,IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,170) 'PDRSIF',IERR
         CALL SUERRS (LP,2,NUMERR)
         GO TO 100
         ENDIF
      CALL SUBSTR (ISIBUF(2),1,8,STAID,1)
      IF (STAID.EQ.'DELETED') GO TO 50
      IPTR=11
      NUM=ISIBUF(10)
      DO 40 I=1,NUM
C     CHECK FOR RRS TYPES
         CALL SUBSTR (ISIBUF(IPTR),1,4,ITYPE,1)
         IF (IPDCKR(ITYPE).EQ.0) GO TO 30
         NUMID=NUMID+1
         IF (NUMID.LE.MAXID.AND.NUMID.LE.LSWORK) GO TO 20
            WRITE (LP,150) MAXID
            CALL SUERRS (LP,2,NUMERR)
            GO TO 100
20       CALL SUBSTR (STAID,1,8,IARRAY(1,NUMID),1)
         NREC=ISIBUF(IPTR+2)
         IARRAY(3,NUMID)=NREC
         IWORK(NUMID)=NREC
30       IPTR=IPTR+3
40       CONTINUE
50    IF (NXREC.LE.LSTSIF) THEN
         IREC=NXREC
         GO TO 10
         ENDIF
C
C  CHECK IF ANY RRS STATIONS FOUND
      IF (NUMID.EQ.0) THEN
         WRITE (LP,160)
         CALL SULINE (LP,2)
         GO TO 100
         ENDIF
C
C  SORT IDENTIFIERS
      ISPTR=0
      CALL SUSORT (NWORDS,NUMID,IARRAY,IARRAY,ISPTR,IERR)
C
C  PRINT ALL STATIONS WITH RRS DATA
      IF (ISLEFT(5).GT.0) CALL SUPAGE
      WRITE (LP,140)
      CALL SULINE (LP,2)
      WRITE (LP,200)
      CALL SULINE (LP,6)
C
C  CHECK FOR SUFFICIENT SPACE IN ARRAY TO STORE RRS RECORD
      CALL RPDLRS (LWNEED,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,170) 'RPDLRS',IERR
         CALL SUERRS (LP,2,NUMERR)
         GO TO 100
         ENDIF
      IF (MRRBF.GE.LWNEED) GO TO 80
         WRITE (LP,180) LWNEED,MRRBF
         CALL SUERRS (LP,2,NUMERR)
         GO TO 100
C
C  PROCESS EACH RRS RECORD
80    NUMTYP=0
      DO 90 I=1,NUMID
         NREC1=IWORK(I)
         CALL PDRRRR (NREC1,LRCPDR,MRRBF,IRRBF1,IERR)
         IF (IERR.GT.0) THEN
            WRITE (LP,190) NREC1
            CALL SUERRS (LP,2,NUMERR)
            GO TO 100
            ENDIF
         NREC2=IARRAY(3,I)
         CALL PDRRRR (NREC2,LRCPDR,MRRBF,IRRBF2,IERR)
         IF (IERR.GT.0) THEN
            WRITE (LP,190) NREC2
            CALL SUERRS (LP,2,NUMERR)
            GO TO 100
            ENDIF
C        PRINT STATUS
            NUMTYP=NUMTYP+1
            WRITE (LP,210) NUMTYP,
     *         (IRRBF1(N),N=2,3),IRRBF1(5),NREC1,IRRBF1(13),
     *         (IRRBF2(N),N=2,3),IRRBF2(5),NREC2,IRRBF2(13)
            CALL SULINE (LP,1)
            IF (ISNWPG(LP).EQ.1) THEN
               WRITE (LP,200)
               CALL SULINE (LP,6)
               ENDIF
90       CONTINUE
C
      WRITE (LP,220) NUMID,MAXID
      CALL SULINE (LP,2)
C
100   IF (NUMERR.GT.0) ISTAT=1
C
110   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,240) ISTAT
         CALL SULINE (LP,2)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
120   FORMAT (' *** ENTER SSPPD3')
130   FORMAT ('0*** NOTE - NO STATIONS ARE DEFINED.')
140   FORMAT ('0- STATUS FOR RRS STATIONS -')
150   FORMAT ('0*** ERROR - IN SMPPD3 - MAXIMUM NUMBER OF IDENTIFIERS ',
     *   'THAT CAN BE PROCESSED (',I5,') EXCEEDED.')
160   FORMAT ('0*** NOTE - NO STATIONS WITH RRS DATA ARE DEFINED.')
170   FORMAT ('0*** ERROR - IN SSPPD3 - CALLING ROUTINE ',A,'. ',
     *   'STATUS CODE=',I3)
180   FORMAT ('0*** ERROR - IN SSPPD3 - NUMBER OF WORDS NEEDED IN ',
     *   'ARRAY TO READ RRS RECORDS (',I5,') EXCEEDS ARRAY ',
     *   'DIMENSION (',I5,').')
190   FORMAT ('0*** ERROR - IN SSPPD3 - DAIO READ ERROR - IN PDRRRR ',
     *   'AT RECORD ',I5,'.')
200   FORMAT ('0',T24,'- NOT SORTED - ',
     *     T68,'- SORTED BY IDENTIFIER -' /
     *   '0',2(6X,18X,'PRIMARY ',3X,'FREE POOL',10X)/
     *   ' ',2(6X,'STA ID  ',3X,'TYPE',3X,'RECORD #',3X,'RECORD # ',
     *         10X) /
     *   ' ',2(6X,8('-'),3X,4('-'),3X,8('-'),3X,9('-'),10X))
210   FORMAT (1X,I4,2X,2(2A4,3X,A4,5X,I5,7X,I5,17X))
220   FORMAT ('0*** NOTE - ',I5,' STATIONS PROCESSED. ',
     *   'A MAXIMUM OF ',I5,' STATIONS CAN BE PROCESSED.')
240   FORMAT (' *** EXIT SSPPD3 : ISTAT=',I2)
C
      END
