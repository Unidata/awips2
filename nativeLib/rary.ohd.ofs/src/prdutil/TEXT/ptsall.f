C MEMBER PTSALL
C  (from old member PRDPRINT)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 05/12/95.10:54:24 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PTSALL (IKEY,LWKBUF,IWKBUF,ISTAT)
C
C          ROUTINE:  PTSALL
C
C             VERSION:  1.0.0
C
C                DATE:  5-5-82
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE READS THE INPUT CARDS FOR THE TSHDRS AND
C    TSDATA COMMANDS AND THEN CALLS THE GENERAL PRINT ROUTINE
C    TO PRINT THE APPROPRIATE TIME SERIES.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IKEY       I     I     1    KEY TO PRINT FORMAT
C                                     0=HEADER
C                                     1=HEADER AND DATA
C       LWKBUF     I     I     1    LENGTH OF ARRAY IWKBUF
C       IWKBUF     I     I  LWKBUF  LENGTH OF ARRAY IWKBUF
C       ISTAT      I     O     1    STATUS INDICATOR
C                                     0=NORMAL RETURN
C                                     1=EOF FOUND
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      CHARACTER*4 XUNITS
C
      DIMENSION IWKBUF(LWKBUF)
      DIMENSION ID(2)
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'udatas'
      INCLUDE 'ufreei'
      INCLUDE 'ufstcd'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/pmaxdm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/prdutil/RCS/ptsall.f,v $
     . $',                                                             '
     .$Id: ptsall.f,v 1.2 1996/12/09 21:26:03 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA LEND/4HEND /
      DATA LALL/4HALL /
      DATA LREG/4HREG /
      DATA LFUT/4HFUT /
      DATA LBOTH/4HBOTH /
      DATA LAMPER/4H&   /
C
C***********************************************************************
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,210)
C
      ISTAT=0
C
10    IUNITS=IBLNK
      IFORM=2
      NDCNOW=-1
C
C  READ CARD
      IFSTCD=1
      CALL RCOMND (LEND,1,INDX)
      CALL WPCARD (IBUF)
      IF (INDX) 190,20,200
C
C  GET FIRST FIELD
20    NFLD=1
      NUM=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NUM.GT.4) THEN
         CALL ULINE (LP,2)
         WRITE (LP,220)
         GO TO 10
         ENDIF
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),ITYPE,NUM)
C
C  CHECK IF 'ALL' SPECIFIED
      IF (ITYPE.EQ.LALL) GO TO 160
C
C  CHECK FOR VALID DATA TYPE
      CALL PFDTYP (ITYPE,IXTYPE)
      IF (IXTYPE.EQ.0) THEN
30       CALL ULINE (LP,2)
         WRITE (LP,230)
         GO TO 10
         ENDIF
C
C  CHECK PRINT FORMAT
      IF (NFIELD.EQ.1) GO TO 40
         NFLD=NFLD+1
         NUM=IFSTOP(NFLD)-IFSTRT(NFLD)+1
         IF (NUM.LE.4) GO TO 50
40       CALL ULINE (LP,2)
         WRITE (LP,240) NFLD
         GO TO 10
50    CALL UPACK1 (IBUF(IFSTRT(NFLD)),IWHAT,NUM)
      IALL=0
      IF (IWHAT.EQ.LALL) THEN
         IALL=1
         GO TO 60
         ENDIF
C
C  CHECK FOR 'REG', 'FUT' OR 'BOTH'
      IFORM=-1
      IF (IWHAT.EQ.LBOTH) IFORM=2
      IF (IWHAT.EQ.LREG) IFORM=3
      IF (IWHAT.EQ.LFUT) IFORM=4
C
      IF (IFORM.LT.0) GO TO 40
C
      IF (NFIELD.EQ.2) GO TO 120
C
C  CHECK NEXT FIELD FOR ALL
      NFLD=NFLD+1
      NUM=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NUM.GT.4) GO TO 40
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),IWHAT,NUM)
      IALL=0
      IF (IWHAT.EQ.LALL) THEN
         IALL=1
         ELSE
            NFLD=NFLD-1
         ENDIF
C
C  CHECK IF UNITS SPECIFIED
60    IF (NFLD+1.GT.NFIELD) GO TO 70
      NFLD=NFLD+1
      IF (IFTYPE(NFLD).EQ.0) GO TO 70
      NUM=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NUM.GT.4) GO TO 40
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),IUNITS,NUM)
C
C  CHECK FOR VALID UNITS CODE
      IDUCNV=2
      NDUCNV=1
      CALL UDUCNV (IUNITS,IUNITS,IDUCNV,NDUCNV,UDUVL1,UDUVL2,IERR)
      IF (IERR.NE.0) THEN
         CALL ULINE (LP,2)
         WRITE (LP,250) NFLD
         GO TO 10
         ENDIF
C
C  SET DEFAULT DECIMAL PLACES
70    CALL UMEMOV (IUNITS,XUNITS,1)
      CALL UDUNDC (XUNITS,NDCNOW,IERR)
      IF (IERR.NE.0) NDCNOW=2
C
C  CHECK IF NUMBER OF DECIMAL PLACES SPECIFIED
      IF (NFLD+1.GT.NFIELD) GO TO 80
      NFLD=NFLD+1
      IF (IFTYPE(NFLD).NE.1) THEN
         CALL ULINE (LP,2)
         WRITE (LP,260) NFLD,NDCNOW
         GO TO 80
         ENDIF
C
C  SET NUMBER OF DECIMAL PLACES
      CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),INTEGR)
      IF (INTEGR.LT.0.OR.INTEGR.GT.7) THEN
         CALL ULINE (LP,2)
         WRITE (LP,270) NFLD,NDCNOW
         ELSE
            NDCNOW=INTEGR
         ENDIF
C
80    IF (IALL.EQ.0) GO TO 120
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT ALL TIME SEREIS FOR ONE DATA TYPES
C
      IF (IPRDB.GT.0) WRITE (IOGDB,280)
C
C  CHECK NUMBER OF TIME SERIES DEFINED
90    NUMTS=DATFIL(15,IXTYPE)
      IF (NUMTS.EQ.0) THEN
         CALL ULINE (LP,2)
         WRITE (LP,290) ITYPE
         GO TO 10
         ENDIF
C
      IPAGE=0
      CALL PTSHCS (IPAGE,DATFIL(7,IXTYPE),IKEY)
      IF (IFORM.NE.4.OR.DATFIL(7,IXTYPE).EQ.0) THEN
         ELSE
            IXTYPE=DATFIL(7,IXTYPE)
            IF (IXTYPE.LE.0) GO TO 30
         ENDIF
C
      ISTRT=DATFIL(8,IXTYPE)
C
100   NLINEL=5
      CALL ULINEL (LP,NLINEL,IRETRN)
      IF (IRETRN.EQ.1) THEN
         IPAGE=1
         CALL PTSHCS (IPAGE,DATFIL(7,IXTYPE),IKEY)
         ENDIF
      IUNIT=DATFIL(2,IXTYPE)
      CALL PTSRDG (IUNIT,ISTRT,NXTREC,IUNITS,IFREC,
     *   IPAGE,IFORM,IKEY,
     *   LWKBUF,IWKBUF,NDCNOW,IERR)
      IF (IERR.NE.0) GO TO 10
      IF (NXTREC.EQ.0) GO TO 110
         ISTRT=NXTREC
         GO TO 100
C
C  PROCESS FUTURE FOR TSHDRS
110   IF (IKEY.EQ.1.OR.IFORM.NE.2.OR.DATFIL(7,IXTYPE).LE.0) GO TO 10
      IXTYPE=DATFIL(7,IXTYPE)
      IF (IXTYPE.LE.0) GO TO 30
      GO TO 90
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT SPECIFIED TIME SERIES
C
120   IF (IPRDB.GT.0) WRITE (IOGDB,300)
C
      IPRHDR=0
C
C  READ CARD
130   CALL RCOMND (LEND,1,INDX)
      IFSTCD=1
      CALL WPCARD (IBUF)
      IF (INDX.NE.0) THEN
         CALL ULINE (LP,2)
         WRITE (LP,310)
         IF (INDX.EQ.-1) GO TO 190
         GO TO 10
         ENDIF
C
C  GET FIELD
      NFLD=1
140   IF (NFLD.GT.NFIELD) GO TO 10
      NUM=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NUM.GT.8) THEN
         CALL ULINE (LP,2)
         WRITE (LP,320) NFLD
         GO TO 150
         ENDIF
      ID(2)=IBLNK
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),ID,NUM)
C
C  CHECK FOR CONTINUATION INDICATOR
      IF (ID(1).EQ.LAMPER) GO TO 130
C
C  CHECK LINES LEFT ON PAGE
      NLINEL=5
      CALL ULINEL (LP,NLINEL,IRETRN)
      IF (IRETRN.EQ.1) THEN
         IPAGE=1
         CALL PTSHCS (IPAGE,DATFIL(7,IXTYPE),IKEY)
         ELSE
            IF (IPRHDR.EQ.0) THEN
               IPAGE=0
               CALL PTSHCS (IPAGE,DATFIL(7,IXTYPE),IKEY)
               IPRHDR=1
               ENDIF
         ENDIF
C
C  PRINT USING FORMAT FOR REGULAR DATA
      IPAGE=0
      IF (IFORM.EQ.4.AND.DATFIL(7,IXTYPE).NE.0) THEN
         IPOS=IABS(DATFIL(7,IXTYPE))
         ITYPE=DATFIL(1,IPOS)
         ENDIF
      CALL PTSREC (ID,ITYPE,IUNITS,IFREC,
     *   IPAGE,IFORM,IKEY,
     *   LWKBUF,IWKBUF,NDCNOW,IERR)
C
      IF (IKEY.EQ.1.OR.IFORM.NE.2.OR.DATFIL(7,IXTYPE).EQ.0) GO TO 150
C
C  PRINT USING FORMAT FOR FUTURE DATA
      LUNIT=DATFIL(2,DATFIL(7,IXTYPE))
      IPAGE=0
      CALL PTSRDG (LUNIT,IFREC,NXTREC,IUNITS,IFREC,
     *   IPAGE,DATFIL(7,IXTYPE),IKEY,
     *   LWKBUF,IWKBUF,NDCNOW,IERR)
C
150   NFLD=NFLD+1
      GO TO 140
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT ALL TIME SERIES FOR ALL DATA TYPES
C
160   IF (IPRDB.GT.0) WRITE (IOGDB,330)
C
C  CHECK IF ANY DATA TYPES DEFINED
      IF (NUMDTP.EQ.0) THEN
         CALL ULINE (LP,2)
         WRITE (LP,350)
         GO TO 10
         ENDIF
C
      DO 180 I=1,NUMDTP
         IF (IKEY.EQ.1.AND.DATFIL(7,I).LT.0) GO TO 180
         ITYPE=DATFIL(1,I)
         NUMTS=DATFIL(15,I)
         CALL ULINE (LP,2)
         WRITE (LP,340)
         IF (NUMTS.EQ.0) THEN
            CALL ULINE (LP,2)
            WRITE (LP,290) ITYPE
            GO TO 180
            ENDIF
         NDCNOW=-1
         IPAGE=0
         CALL PTSHCS (IPAGE,DATFIL(7,I),IKEY)
         IUNIT=DATFIL(2,I)
         ISTRT=DATFIL(8,I)
170      IPAGE=0
         CALL PTSRDG (IUNIT,ISTRT,NXTREC,IUNITS,IFREC,
     *      IPAGE,IFORM,IKEY,
     *      LWKBUF,IWKBUF,NDCNOW,IERR)
         IF (IERR.NE.0) GO TO 10
         IF (NXTREC.GT.0) THEN
            ISTRT=NXTREC
            GO TO 170
            ENDIF
180      CONTINUE
C
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  END OF FILE ENCOUNTERED
190   CALL ULINE (LP,2)
      WRITE (LP,360)
      ISTAT=1
C
200   IF (IPRTR.GT.0) WRITE (IOGDB,370)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
210   FORMAT (' *** ENTER PTSALL')
220   FORMAT ('0**ERROR** DATA TYPE OR ''ALL'' MUST BE IN FIELD ONE.')
230   FORMAT ('0**ERROR** VALUE IN FIELD ONE IS NOT A VALID DATA TYPE.')
240   FORMAT ('0**ERROR** UNRECOGNIZED VALUE IN FIELD ',I2,'.')
250   FORMAT ('0**ERROR** THE VALUE IN FIELD ',I2,' IS NOT A VALID ',
     *   'UNITS CODE.')
260   FORMAT ('0**WARNING** INTEGER VALUE EXPECTED IN FIELD ',
     *   I2,'. DEFAULT OF ',I4,' WILL BE USED.')
270   FORMAT ('0**WARNING** INVALID NUMBER OF DECIMAL PLACES IN FIELD ',
     *   I2,'. DEFAULT OF ',I4,' WILL BE USED.')
280   FORMAT (' PRINTING ALL OF A TYPE')
290   FORMAT ('0**NOTE** NO ',A4,' TIME SERIES DEFINED.')
300   FORMAT (' PRINTING FROM LIST')
310   FORMAT ('0**ERROR** EXPECTING LIST OF TIME SERIES IDENTIFIERS.')
320   FORMAT ('0**ERROR** INVALID TIME SERIES ID IN FIELD ',I2,'.')
330   FORMAT (' PRINTING ALL TIME SERIES')
340   FORMAT ('0',132('#'))
350   FORMAT ('0**NOTE** NO DATA TYPES ARE DEFINED.')
360   FORMAT ('0**WARNING** NO END CARD FOUND.')
370   FORMAT (' *** EXIT PTSALL')
C
      END
