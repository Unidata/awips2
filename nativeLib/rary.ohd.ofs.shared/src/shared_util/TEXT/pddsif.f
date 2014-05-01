C MODULE PDDSIF
C-----------------------------------------------------------------------
C
      SUBROUTINE PDDSIF (LSIBUF,ISIBUF,IRESET,LARRAY,ARRAY)
C
C  ROUTINE TO DUMP SIF RECORDS.
C
      CHARACTER*4 PMTYPE,PDTYPE,PRTYPE
      CHARACTER*8 STAID,PRSTID
C
      INTEGER*2 ISIBUF(LSIBUF)
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION IRRBUF(16),IFPBUF(16)
C
      INCLUDE 'uiox'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdrrsc'
      INCLUDE 'pdbcommon/pdbdta'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdi2max'
      INCLUDE 'urcommon/ursifc'
      INCLUDE 'urcommon/urrrsc'
      INCLUDE 'urcommon/urunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/pddsif.f,v $
     . $',                                                             '
     .$Id: pddsif.f,v 1.7 2002/10/10 15:55:45 dws Exp $
     . $' /
C    ===================================================================
C
C
      IBUG=0
C
      NUMSIF=0
      ISIREC=INFREC+1
C
      CALL ULINE (LP,2)
      WRITE (LP,5)
5     FORMAT ('0- DUMP OF STATION INFORMATION FILE (SIF) RECORDS -')
C
C  SET RECORD NUMBER OF LAST SIF RECORD
      IF (IAMORD.EQ.0) LSTREC=LSTSIF
      IF (IAMORD.EQ.1) LSTREC=LTSIFR
C
C  READ SIF RECORD
10    CALL PDRSIF (ISIREC,NXSIREC,LSIBUF,ISIBUF,IERR)
      CALL SUBSTR (ISIBUF(2),1,8,STAID,1)
      IF (STAID.EQ.'DELETED') GO TO 40
      CALL ULINE (LP,1)
      WRITE (LP,'(A,I5,1X,A,A,3(1X,A,I5))')
     *   ' NUMSIF=',NUMSIF+1,
     *   ' STAID=',STAID,
     *   ' ISIREC=',ISIREC,
     *   ' NXSIREC=',NXSIREC,
     *   ' IERR=',IERR
      IF (IERR.NE.0) GO TO 40
      NUMSIF=NUMSIF+1
      NWRDS=ISIBUF(1)
      NUMID=ISIBUF(6)
      IGENL=ISIBUF(7)
      IPP24=ISIBUF(8)
      ITM24=ISIBUF(9)
      NADDTP=ISIBUF(10)
      CALL ULINE (LP,1)
      WRITE (LP,'(9(1X,A,I5))')
     *   ' NWRDS=',NWRDS,
     *   ' NUMID=',NUMID,
     *   ' IGENL=',IGENL,
     *   ' IPP24=',IPP24,
     *   ' ITM24=',ITM24,
     *   ' NADDTP=',NADDTP
      JSIBUF=11
      IF (NADDTP.GT.0) THEN
         NSIBUF=JSIBUF+NADDTP*3-1
         CALL ULINE (LP,1)
         WRITE (LP,15)
     *      ' (ISIBUF(I),I=',JSIBUF,',',NSIBUF,')=',
     *        (ISIBUF(I),I=JSIBUF,NSIBUF)
15    FORMAT (' ',A,I2,A,I2,A,10(2A2,1x,I6,1X)) 
         ENDIF
C  CHECK IF NUMBER OF WORDS IN SIF RECORD IS LESS THAN EXPECTED
      IBGADD=11
      NTPADD=ISIBUF(10)
      NWORDS=IBGADD+NTPADD*3-1
      IF (IPP24.GT.0) NWORDS=NWORDS+NDSTAT
      IF (NWORDS.GT.ISIBUF(1)) THEN
         WRITE (LP,80) STAID,ISIBUF(1),NWORDS
         CALL UWARN (LP,0,-1)
         ENDIF
      IF (IGENL.GT.0) THEN
C     CHECK IF GENL PARAMETER RECORD FOUND
         PMTYPE='GENL'
         IPMREC=0
         CALL RPPREC (STAID,PMTYPE,IPMREC,LARRAY,ARRAY,NFILL,IPTRNX,
     *      IERR)
         IF (IERR.NE.0) THEN
            CALL UWARN (LP,0,-1)
            WRITE (LP,50) PMTYPE,STAID,IERR
            ENDIF
         ELSE
            CALL UWARN (LP,0,-1)
            WRITE (LP,60) PMTYPE,STAID
         ENDIF
      IF (IPP24.GT.0) THEN
C     CHECK IF PCPN PARAMETER RECORD FOUND
         PMTYPE='PCPN'
         IPMREC=0
         CALL RPPREC (STAID,PMTYPE,IPMREC,LARRAY,ARRAY,NFILL,IPTRNX,
     *      IERR)
         IF (IERR.NE.0) THEN
            IF (IERR.EQ.2.AND.IRESET.EQ.1) THEN
               CALL UWARN (LP,0,-1)
               WRITE (LP,70) PMTYPE,STAID,'PP24'
               ISIBUF(8)=0
               CALL PDWSIF (LSIBUF,ISIBUF,ISIREC,IERR)
               ELSE
                  CALL UWARN (LP,0,-1)
                  WRITE (LP,50) PMTYPE,STAID,IERR
               ENDIF
            ENDIF
         ENDIF
      IF (ITM24.GT.0) THEN
C     CHECK IF TEMP PARAMETER RECORD FOUND
         PMTYPE='TEMP'
         IPMREC=0
         CALL RPPREC (STAID,PMTYPE,IPMREC,LARRAY,ARRAY,NFILL,IPTRNX,
     *      IERR)
         IF (IERR.NE.0) THEN
            IF (IERR.EQ.2.AND.IRESET.EQ.1) THEN
               CALL UWARN (LP,0,-1)
               WRITE (LP,70) PMTYPE,STAID,'TM24'
               ISIBUF(9)=0
               CALL PDWSIF (LSIBUF,ISIBUF,ISIREC,IERR)
               ELSE
                  CALL UWARN (LP,0,-1)
                  WRITE (LP,50) PMTYPE,STAID,IERR
               ENDIF
            ENDIF
         ENDIF
      IF (NADDTP.GT.0) THEN
         IRRS=0
         DO 30 I=1,NADDTP
            IPOS=JSIBUF+(I-1)*3
            CALL UMEMOV (ISIBUF(IPOS),PDTYPE,1)
C        CHECK IF RRS DATA TYPE
            IX=IPDCKR(PDTYPE)
            IF (IBUG.EQ.1) WRITE (LP,*) 'PDTYPE=',PDTYPE,' IX=',IX
            IF (IX.GT.0) THEN
               IRRS=1
               IRREC=ISIBUF(IPOS+2)
CMGM 4/2002 THE RRS RECORD NUMBER (IRREC) MAY BE STORED IN THE ISIBUF
C    ARRAY AS A NEGATIVE NUMBER. THIS ALLOWS TWICE AS MANY RECORD 
C    NUMBERS IN THE I2 ARRAY. CONVERT IRREC TO A POSITIVE RECORD NUMBER
C    IRREC2 TO READ/WRITE (UREADT/UWRITT) THE KPDRRS FILE.
CMGM            
               IRREC2=IRREC
               IF (IRREC2.LT.0) IRREC2=IRREC2+2*I2MAX
               IF (IBUG.EQ.1) WRITE (LP,*) 'STAID=',STAID,
     *            ' PDTYPE=',PDTYPE,
     *            ' IRREC=',IRREC,
     *            ' IRREC2=',IRREC2
C           READ THE RRS PRIMARY DATA RECORD
               IF (IAMORD.EQ.0) IUNIT=KPDRRS
               IF (IAMORD.EQ.1) IUNIT=KURRRS
               CALL UREADT (IUNIT,IRREC2,IRRBUF,ISTAT)
               IF (ISTAT.NE.0) THEN
                  CALL UEROR (LP,0,-1)
                  WRITE (LP,90) 'PRIMARY',IRREC2,PDTYPE,STAID
                  GO TO 30
                  ENDIF
               CALL UMEMOV (IRRBUF(2),PRSTID,2)
               CALL UMEMOV (IRRBUF(5),PRTYPE,1)
               IFPREC=IRRBUF(13)
               IF (IBUG.EQ.1) WRITE (LP,*) 'PRSTID=',PRSTID,
     *            ' PRTYPE=',PRTYPE,
     *            ' IFPREC=',IFPREC
               IF (PRSTID.NE.STAID) THEN
                  CALL UEROR (LP,0,-1)
                  WRITE (LP,95) PRSTID,IRREC2,PDTYPE,STAID
                  GO TO 30
                  ENDIF
C           CHECK IF ANY FREEPOOL DATA RECORDS
               IF (IFPREC.LE.0) GO TO 30
               IF (IAMORD.EQ.0) IUNIT=KPDDDF(LUFREE)
               IF (IAMORD.EQ.1) IUNIT=KURDDF(IUFREE)
C           READ FREEPOOL RECORD
20             CALL UREADT (IUNIT,IFPREC,IFPBUF,ISTAT)
               IF (ISTAT.NE.0) THEN
                  IF (IRESET.EQ.0) THEN
                     CALL UEROR (LP,0,-1)
                     WRITE (LP,90) 'FREEPOOL',IFPREC,PDTYPE,STAID
                     ELSE
                        CALL UEROR (LP,0,-1)
                        WRITE (LP,100) IFPREC,PDTYPE,STAID
                        IRRBUF(13)=0
                        CALL UWRITT (KPDRRS,IRREC,IRRBUF,ISTAT)
                     ENDIF
                  GO TO 30
                  ENDIF
               IFPRECC=IFPBUF(1)
C           CHECK IF ANY MORE FREEPOOL RECORDS
               IF (IFPRECC.GT.0) THEN
                  IFPREC=IFPRECC
                  GO TO 20
                  ENDIF
               ENDIF
30          CONTINUE
         IF (IRRS.EQ.1) THEN
C        CHECK IF RRS PARAMETER RECORD FOUND
            PMTYPE='RRS'
            IPMREC=0
            CALL RPPREC (STAID,PMTYPE,IPMREC,LARRAY,ARRAY,NFILL,IPTRNX,
     *         IERR)
            IF (IERR.NE.0) THEN
               IF (IERR.EQ.2) THEN
                  CALL UWARN (LP,0,-1)
                  WRITE (LP,75) PMTYPE,STAID
                  ELSE
                     CALL UWARN (LP,0,-1)
                     WRITE (LP,50) PMTYPE,STAID,IERR
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
C
C  CHECK IF MORE RECORDS
      IF (NXSIREC.LE.LSTREC) THEN
         ISIREC=NXSIREC
         GO TO 10
         ENDIF
C
40    CALL ULINE (LP,2)
      WRITE (LP,110) NUMSIF
C
      IF (NUMSIF.NE.NPDSTA) THEN
         CALL UWARN (LP,0,-1)
         WRITE (LP,120) NUMSIF,NPDSTA
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT ('0**WARNING** ',A,' PARAMETER RECORD FOR STATION ',A,
     *   ' NOT SUCCESSFULLY READ. RPPREC STATUS CODE IS ',I2,'.')
60    FORMAT ('0**WARNING** RECORD NUMBER OF ',A,' PARAMETER RECORD ',
     *    'IN PREPROCESSOR PARAMETRIC DATA BASE FOR STATION ',A,
     *    ' IS ZERO.')
70    FORMAT ('0**WARNING** ',A,' PARAMETER RECORD FOR STATION ',A,
     *   ' NOT FOUND. ',A,
     *   ' POINTER LOCATION IN SIF RECORD WILL BE SET TO ZERO.')
75    FORMAT ('0**WARNING** ',A,' PARAMETER RECORD FOR STATION ',A,
     *   ' NOT FOUND. ',A,'.')
80    FORMAT ('0**WARNING** STATION ',A,' HAS PP24 DATA ',
     *   'BUT NUMBER OF WORDS IN SIF RECORD (',I2,
     *   ') IS LESS THAN EXPECTED (',I2,').')
90    FORMAT ('0**ERROR** READING RRS ',A,' RECORD ',I10,
     *   ' FOR DATA TYPE ',A,' FOR STATION ',A,'.')
95    FORMAT ('0**ERROR** STATION ID (',A,
     *   ') IN RRS PRIMARY RECORD ',I6,' IS INCORRECT ',
     *   'FOR DATA TYPE ',A,' FOR STATION ',A,'.')
100   FORMAT ('0**WARNING** ERROR ENCOUNTERED READING RRS FREEPOOL ',
     *       'RECORD ',I10,' FOR DATA TYPE ',A,' FOR STATION ',A,'.' /
     *   T14,'RECORD NUMBER OF FIRST FREEPOOL RECORD IN ',
     *       'RRS PRIMARY RECORD WILL BE SET TO ZERO.')
110   FORMAT ('0**NOTE** ',I4,' SIF RECORDS PROCESSED.')
120   FORMAT ('0**WARNING** NUMBER OF SIF RECORDS CHECKED (',I4,
     *   ') DOES NOT EQUAL NUMBER OF STATIONS DEFINED (',I4,').')
C
      END
