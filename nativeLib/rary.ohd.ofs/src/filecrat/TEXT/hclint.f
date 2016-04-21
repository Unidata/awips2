C MEMBER HCLINT
C  (from old member UXHCLINT)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 01/27/95.15:32:16 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HCLINT (USER,DCBDDN,DCBMBR,DSKUNT,DISP,LDEBUG)
C
C             ROUTINE:  HCLINT
C
C             VERSION:  1.0.0
C
C                DATE: 7-16-81
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C  ROUTINE TO INITIALIZE THE HYDROLOGIC COMMAND LANGUAGE LOCAL OR
C  GLOBAL DATASETS.
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'udatas'
      INCLUDE 'ufreei'
      INCLUDE 'hclcommon/hdatas'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hwords'
      INCLUDE 'hclcommon/hindx'
      INCLUDE 'hclcommon/hdflts'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      CHARACTER*4 DISP
      CHARACTER*(*) USER,DCBDDN,DCBMBR
      CHARACTER*8 XOPTN,XTYPE
      CHARACTER*8 FILES(3)/'HCLINDEX','HCLDEFIN','HCLLDFLT'/
C
C
      DIMENSION IARR(4),IJARR(16),IHCNTL(16)
      DIMENSION MAXREC(3),LBUF(300)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/filecrat/RCS/hclint.f,v $
     . $',                                                             '
     .$Id: hclint.f,v 1.1 1995/09/17 19:08:45 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA IQUES/4H?   /
C
C***********************************************************************
C
C
      CALL ULINE (LP,2)
      WRITE (LP,270)
C
      MLBUF=300
C
      INDERR=0
      IPWORD=IBLNK
      IGLFLG=0
      IDELDR=0
      MAXFLD=4
      MALREC=5341
      CALL UMEMST (0,MAXREC,3)
C
C
C  PRINT CARD
      CALL ULINE (LP,1)
      WRITE (LP,*) ' '
      CALL WPCARD (IBUF)
C
      INDERR=0
C
C  READ CARD
10    CALL RPCARD (IBUF,IERR)
      CALL ULINE (LP,1)
      WRITE (LP,*) ' '
      CALL WPCARD (IBUF)
      IF (IERR.GT.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,280)
         GO TO 260
         ENDIF
C  FIND FIELDS ON CARD
      CALL UFREE (1,72)
C
C  CHECK FOR BLANK CARD
      IF (NFIELD.EQ.0) GO TO 10
C
      NFLD=1
      NUM=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NUM.GT.8) NUM=8
      XOPTN=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),XOPTN,NUM)
C
C  CHECK FOR COMME|T
      IF (XOPTN.EQ.'$') GO TO 10
      IF (XOPTN.EQ.'END') GO TO 90
C
C  CHECK FOR OPTION
      IF (XOPTN.EQ.'DELLDR') GO TO 20
      IF (XOPTN.EQ.'GLOBAL') GO TO 30
      IF (XOPTN.EQ.'LOCAL') GO TO 30
      IF (XOPTN.EQ.'PASSWORD') GO TO 60
      IF (XOPTN.EQ.'TRACKS') GO TO 70
      CALL UEROR (LP,1,-1)
      WRITE (LP,290)
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SET OPTION TO DELETE LOCAL DEFINTION REFERENCE RECORDS
C
20    IDELDR=1
      CALL ULINE (LP,2)
      WRITE (LP,330)
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  GET FILE TYPE
C
30    IF (NFIELD.GT.2) GO TO 40
      NFLD=1
      NUM=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NUM.GT.6) GO TO 40
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),XTYPE,NUM)
      IF (XTYPE.EQ.'LOCAL') THEN
         CALL ULINE (LP,2)
         WRITE (LP,300) 'LOCAL'
         IGLFLG=1
         GO TO 50
         ENDIF
      IF (XTYPE.EQ.'GLOBAL') THEN
         CALL ULINE (LP,2)
         WRITE (LP,300) 'GLOBAL'
         IGLFLG=-1
         GO TO 50
         ENDIF
40    CALL UEROR (LP,1,-1)
      WRITE (LP,340)
      GO TO 260
C
50    IF (NFIELD.EQ.1) GO TO 10
C
C  SET DISPOSITON
      NFLD=2
      NUM=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),DISP,NUM)
      CALL ULINE (LP,2)
      WRITE (LP,310) DISP
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PROCESS PASSWORD
C
60    IF (NFIELD.NE.2) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,350) 'PASSWORD'
         INDERR=1
         GO TO 10
         ENDIF
      IF (NUM.NE.8) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,350) 'PASSWORD'
         INDERR=1
         GO TO 10
         ENDIF
C
      NFLD=2
      NUM=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NUM.GT.4) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,350) 'PASSWORD'
         INDERR=1
         GO TO 10
         ENDIF
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),IPWORD,NUM)
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CALCULATE MAXIMUM NUMBER OF RECORDS FOR EACH FILE
C
70    IF (NFIELD.NE.MAXFLD) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,350) 'TRACKS'
         INDERR=1
         GO TO 10
         ENDIF
      IF (NUM.NE.6) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,350) 'TRACKS'
         INDERR=1
         GO TO 10
         ENDIF
      IPRINT=2
      DO 80 I=2,MAXFLD
C     GET DATASET LOGICAL RECORD LENGTH AND BLOCKSIZE
         CALL UFLDCB (DCBDDN,DCBMBR,FILES(I-1),LRECL,LBLOCK,IERR)
         IF (IERR.GT.0) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,320) 'UFLDCB',IERR
            GO TO 260
            ENDIF
C     CALCULATE NUMBER OF RECORDS PER TRACK
         CALL UDKBLK (' ',0,DSKUNT,LBLOCK,IPRINT,NBLKS,IPCT,IERR)
         IF (IERR.GT.0) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,320) 'UDKBLK',IERR
            GO TO 260
            ENDIF
         NPRBLK=LBLOCK/LRECL
         NPRTRK=NPRBLK*NBLKS
         IF (IFTYPE(I).NE.1) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,350) 'TRACKS'
            INDERR=1
            GO TO 10
            ENDIF
         CALL UNUMIC (IBUF,IFSTRT(I),IFSTOP(I),NTRKS)
         MAXREC(I-1)=NTRKS*NPRTRK
80       CONTINUE
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF LOCAL OR GLOBAL SPECIFIED
90    IF (IGLFLG.EQ.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,360)
         INDERR=1
         ENDIF
C
C  CHECK IF PASSWORD SPECIFIED
      IF (IPWORD.EQ.IBLNK.OR.IPWORD.EQ.IQUES) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,370)
         INDERR=1
         ENDIF
C
      IF (INDERR.GT.0) GO TO 260
C
C  CHECK DISPOSITION
      IF (DISP.EQ.'NEW') GO TO 100
C
C  CHECK PASSWORD
      NUNIT=KDEFNL
      IF (IGLFLG.EQ.-1) NUNIT=KDEFNG
      CALL UREADT (NUNIT,1,IJARR,IERR)
      CALL ULINE (LP,2)
      WRITE (LP,380)
      IF (IJARR(3).EQ.DEFN) THEN
         IF (IJARR(5).NE.IPWORD) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,390) IJARR(3),IJARR(5)
            GO TO 260
            ENDIF
         ENDIF
C
C  SET UP CONTROL RECORDS
100   CALL UMEMST (0,IHCNTL,16)
      IHCNTL(1)=0
      IHCNTL(2)=IGLFLG
      IHCNTL(3)=DEFN
      IHCNTL(4)=LRECLH
      IHCNTL(5)=IPWORD
      IHCNTL(6)=MAXREC(2)
      IHCNTL(7)=1
      IHCNTL(8)=0
      IHCNTL(9)=0
      IHCNTL(10)=0
      IHCNTL(11)=0
      IF (IGLFLG.EQ.-1) IHCNTL(11)=499
      IHCNTL(12)=MAXREC(3)
      IHCNTL(13)=1
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK NUMBER OF RECORDS ALLOCATE TO INDEX FILE
      IF (MAXREC(1).LT.MALREC) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,410) MAXREC(1),MALREC
         INDERR=1
         GO TO 170
         ENDIF
C
C  INITIALIZE INDEX FILE
C
      NUNIT=KINDXL
      IF (IGLFLG.EQ.-1) NUNIT=KINDXG
C
C  PRINT DATASET ATTRIBUTES
      IPRERR=1
      CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,LP,IERR)
      IF (IERR.GT.0) THEN
         INDERR=1
         GO TO 170
         ENDIF
C
      IARR(1)=IBLNK
      IARR(2)=IBLNK
      IARR(3)=0
      IARR(4)=0
      K=4
      DO 120 I=1,4
         DO 110 J=1,1000
            K=K+1
            CALL UWRITT (NUNIT,K,IARR,IERR)
            IF (IERR.NE.0) GO TO 240
110         CONTINUE
         IF (IGLFLG.EQ.-1.AND.I.EQ.3) GO TO 130
120      CONTINUE
C
130   CALL ULINE (LP,2)
      WRITE (LP,420) NUNIT,K
C
C  INITIALIZE INDEX CONTROLS
      J=0
      IF (IGLFLG.EQ.-1) J=4
      DO 140 I=1,4
         K=I+J
         HINDEX(1,K)=4+1000*I
         HINDEX(2,K)=5+1000*(I-1)
         HINDEX(3,K)=HINDEX(2,K)-1
         HINDEX(4,K)=I*IGLFLG
140      CONTINUE
C
      HINDEX(1,8)=MALREC
      HINDEX(2,8)=3271
      HINDEX(3,8)=3270
      HINDEX(4,8)=3005
      J=J+1
      CALL WVLRCD (NUNIT,1,4,HINDEX(1,J),LRECLI,IERR)
      IF (IERR.NE.0) GO TO 240
      IF (IGLFLG.GT.0) GO TO 170
C
C  INITIALIZE GLOBAL HCL INDEX FOR FUNCTION NUMBERS
      CALL UMEMST (0,IARR,4)
      IARR(1)=1000
      CALL UWRITT (NUNIT,3005,IARR,IERR)
      IF (IERR.NE.0) GO TO 240
      IARR(1)=0
      DO 150 I=3006,3255
         CALL UWRITT (NUNIT,I,IARR,IERR)
         IF (IERR.NE.0) GO TO 240
150      CONTINUE
C
C  INITIALIZE LOCAL DEFINITION REFERENCE RECORDS
      DO 160 I=3271,MALREC
         CALL UWRITT (NUNIT,I,IARR,IERR)
         IF (IERR.NE.0) GO TO 240
160      CONTINUE
C
      CALL ULINE (LP,2)
      WRITE (LP,400) NUNIT
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  INITIALIZE DEFINITION FILE
C
170   NUNIT=KDEFNL
      IF (IGLFLG.EQ.-1) NUNIT=KDEFNG
C
C  PRINT DATASET ATTRIBUTES
      IPRERR=1
      CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,LP,IERR)
      IF (IERR.GT.0) THEN
         INDERR=1
         GO TO 190
         ENDIF
C
      CALL UMEMST (0,IJARR,LRECLH)
      CALL UMEMOV (IHCNTL,IJARR,16)
      CALL UWRITT (NUNIT,1,IJARR,IERR)
      IF (IERR.NE.0) GO TO 240
      CALL UMEMST (0,IJARR,13)
      NUM=MAXREC(2)
      DO 180 I=2,NUM
         CALL UWRITT (NUNIT,I,IJARR,IERR)
         IF (IERR.NE.0) GO TO 240
180      CONTINUE
C
      CALL ULINE (LP,2)
      WRITE (LP,420) NUNIT,MAXREC(2)
C
      IF (IGLFLG.EQ.-1) GO TO 250
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  INITIALIZE LOCAL DEFAULT FILE
C
190   IF (INDERR.EQ.1) GO TO 260
C
      NUNIT=KLDFGD
C
C  PRINT DATASET ATTRIBUTES
      IPRERR=1
      CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,LP,IERR)
      IF (IERR.GT.0) GO TO 260
      CALL UMEMST (0,IJARR,16)
      CALL UMEMOV (IHCNTL,IJARR,4)
      IJARR(2)=1
      IJARR(3)=DFLT
      CALL UWRITT (NUNIT,1,IJARR,IERR)
      IF (IERR.NE.0) GO TO 240
      CALL UMEMST (0,IJARR,16)
      NUM=MAXREC(3)
      DO 200 I=2,NUM
         CALL UWRITT (NUNIT,I,IJARR,IERR)
         IF (IERR.NE.0) GO TO 240
200      CONTINUE
C
      CALL ULINE (LP,2)
      WRITE (LP,420) NUNIT,MAXREC(3)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (DISP.EQ.'NEW') GO TO 250
      IF (IDELDR.EQ.0) GO TO 250
C
C  DELETE ALL LDR RECORDS IN GLOBAL INDEX
      CALL HGETPM (IERR)
      IF (IERR.NE.0) GO TO 240
      CALL HRIDXC (IERR)
      IF (IERR.NE.0) GO TO 240
      ISTRT=HINDEX(2,8)
      IEND=HINDEX(3,8)+1
C
C  READ AN LDR RECORD
210   IF (ISTRT.EQ.IEND) GO TO 230
      CALL HRLDRR (ISTRT,LBUF,MLBUF,IERR)
      IF (IERR.GT.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,430)
         GO TO 260
         ENDIF
C
C  SEE IF USER IS SAME - DELETE IT
      CALL UNAMCP (LBUF(5),HNAMRF,IMATCH)
      IF (IMATCH.NE.0) GO TO 220
         CALL UMEMOV ('DELETED ',LBUF(3),2)
         LBUF(2)=0
         CALL UWRITT (KINDXG,ISTRT,LBUF,IERR)
         IF (IERR.NE.0) GO TO 240
220   ISTRT=ISTRT+LBUF(1)
      GO TO 210
C
230   CALL ULINE (LP,2)
      WRITE (LP,440) HNAMRF
      GO TO 250
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DAIO ERROR
240   CALL UEROR (LP,1,-1)
      WRITE (LP,450)
      GO TO 260
C
250   CALL ULINE (LP,2)
      WRITE (LP,460)
C
260   RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
270   FORMAT ('0*** NOTE - BEGIN HYDROLOGIC COMMAND LANGUAGE FILE ',
     *   'INITIALIZATION.')
280   FORMAT ('+*** ERROR - READING INPUT CARD.')
290   FORMAT ('+*** ERROR - INVALID OPTION.')
300   FORMAT ('0*** NOTE - ',A,' DATASETS WILL BE INITIALIZED.')
310   FORMAT ('0*** NOTE - DISPOSITION SET TO ',A,'.')
320   FORMAT ('+*** ERROR - ROUTINE ',A,' NOT SUCCESSFULLY CALLED. ',
     *   'STATUS CODE=',I2)
330   FORMAT ('0*** NOTE - LOCAL DEFINITION REFERENCES IN GLOBAL ',
     *   'INDEX WILL BE DELETED.')
340   FORMAT ('+*** ERROR - INVALID INPUT ON LOCAL OR GLOBAL CARD.')
350   FORMAT ('+*** ERROR - INVALID INPUT ON ',A,' CARD.')
360   FORMAT ('+*** ERROR - GLOBAL OR LOCAL NOT SPECIFIED.')
370   FORMAT ('+*** ERROR - PASSWORD NOT SPECIFIED.')
380   FORMAT ('0*** NOTE - CHECKING PASSWORD.')
390   FORMAT ('+*** ERROR - PASSWORD IS INCORRECT. ',
     *  ' DATASET TYPE=',A4,3X,'PASSWORD=',A4)
400   FORMAT ('0*** NOTE - UNIT NUMBER ',I2.2,' INITIALIZED FOR ',
     *  'FUNCTION NUMBERS AND LOCAL DEFINITION REFERENCE RECORDS.')
410   FORMAT ('+*** ERROR - IN HCLINT - NUMBER OF RECORD ALLOCATE FOR ',
     *'INDEX FILE ',I4,' IS LESS THAN MINIMUM RECORD REQUIRED ',I4,' .')
420   FORMAT ('0*** NOTE - UNIT ',I2.2,' SUCCESSFULLY INITIALIZED ',
     *   'WITH ',I5,' RECORDS.')
430   FORMAT ('+*** ERROR - CALLING HRLDRR - LOCAL DEFINITION RECORDS ',
     *   'CANNOT BE DELETED.')
440   FORMAT ('0*** NOTE - ALL LOCAL DEFINITION REFERENCES IN ',
     *   'GLOBAL INDEX FOR USER ',2A4,' SUCCESSFULLY DELETED.')
450   FORMAT ('+*** ERROR - DAIO WRITE ERROR.')
460   FORMAT ('0*** NOTE - HYDROLOGIC COMMAND LANGUAGE FILES ',
     *   'SUCCESSFULLY INITIALIZED.')
C
      END
