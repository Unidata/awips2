C MODULE PRCNTS
C-----------------------------------------------------------------------
C
C  ROUTINE TO CHECK THE NUMBER OF TIME SERIES.
C
      SUBROUTINE PRCNTS
C
      PARAMETER (MTSTYPA=100)
      CHARACTER*4 TSTYP,TSTYPA(MTSTYPA)
      DIMENSION NTSTYPA(MTSTYPA)
      DIMENSION IXBUF(4)
C
      INCLUDE 'uio'
      INCLUDE 'prdcommon/punits'
      INCLUDE 'prdcommon/pdatas'
      INCLUDE 'prdcommon/pmaxdm'
      INCLUDE 'prdcommon/pdftbl'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/prdutil/RCS/prcnts.f,v $
     . $',                                                             '
     .$Id: prcnts.f,v 1.1 1999/04/26 11:55:29 page Exp $
     . $' /
C    ===================================================================
C
C
C
C  CHECK IF ANY TIME SERIES DEFINED
      IF (NUMTMS.EQ.0) THEN
         WRITE (LP,10)
10    FORMAT ('0**NOTE** - NO TIME SERIES ARE DEFINED.')
         GO TO 110
         ENDIF
C
      NUMTS=0
      NUMDEL=0
      NTSTYP=0
      CALL UMEMST ('    ',TSTYPA,MTSTYPA)
      CALL UMEMST (0,NTSTYPA,MTSTYPA)
C
C  COMPUTE NUMBER OF INDEX RECORDS
      NUMREC=MAXTMS*2
C
C  PROCESS EACH INDEX RECORD
      DO 40 I=1,NUMREC
         CALL UREADT (KINDEX,I,IXBUF,IERR)
C     CHECK IF UNUSED
         IF (IXBUF(1).EQ.0) GO TO 40
C     CHECK IF DELETED
         IF (IXBUF(1).EQ.-1) THEN
            NUMDEL=NUMDEL+1
            GO TO 40
            ENDIF
         NUMTS=NUMTS+1
         CALL UMEMOV (IXBUF(3),TSTYP,1)
         DO 20 N=1,MTSTYPA
            IF (TSTYP.EQ.TSTYPA(N)) THEN
               NTSTYPA(N)=NTSTYPA(N)+1
               GO TO 40
               ENDIF
            IF (TSTYPA(N).EQ.' ') THEN
               TSTYPA(N)=TSTYP
               NTSTYPA(N)=NTSTYPA(N)+1
               NTSTYP=NTSTYP+1
               GO TO 40
               ENDIF
20          CONTINUE
         WRITE (LP,30) MTSTYPA
30    FORMAT ('0**ERROR** MAXIMUM NUMBER OF TIME SERIES TYPES ',
     *   'THAT CAN BE PROCESSED (',I4,') EXCEEDED.')
40       CONTINUE
C
      WRITE (LP,50) NUMTS,'TIME SERIES'
50    FORMAT ('0**NOTE** ',I5,' ',A,' FOUND IN INDEX FILE.')
      WRITE (LP,50) NUMDEL,'DELETED TIME SERIES'
C
      IF (NUMTS.NE.NUMTMS) THEN
         WRITE (LP,60) NUMTS,NUMTMS
60    FORMAT ('0**ERROR** NUMBER OF TIME SERIES FOUND IN ',
     *   'INDEX FILE (',I4,') DOES NOT EQUAL THAT FOUND IN ',
     *   'PARAMEER FILE (',I4,').')
         ENDIF
C
      DO 100 N=1,NTSTYP
         DO 80 I=1,NUMDTP
         CALL UMEMOV (DATFIL(1,I),TSTYP,1)
            IF (TSTYP.EQ.TSTYPA(N)) THEN
               IF (NTSTYPA(N).NE.DATFIL(15,I)) THEN
                  WRITE (LP,70) TSTYPA(N),NTSTYPA(N),DATFIL(15,I)
70    FORMAT ('0**ERROR** NUMBER OF ',A,' TIME SERIES FOUND IN ',
     *   'INDEX FILE (',I4,') DOES NOT EQUAL THAT FOUND IN ',
     *   'DATA TYPE DIRECTORY (',I4,').')
                  ENDIF
               GO TO 100
               ENDIF
80          CONTINUE
         WRITE (LP,90) TSTYPA(N)
90    FORMAT ('0**ERROR** TIME SERIES TYPE ',A,
     *   ' NOT FOUND IN DATA TYPE DIRECTORY.')
100      CONTINUE
C
C
110   RETURN
C
      END
