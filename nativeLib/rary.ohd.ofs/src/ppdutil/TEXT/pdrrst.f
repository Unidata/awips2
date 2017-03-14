C MODULE PDRRST
C-----------------------------------------------------------------------
C
       SUBROUTINE PDRRST (IRSTYP,ISTAFL,IUNFLG)
C
C             ROUTINE:  PDRRST
C             VERSION:  1.0.0
C                DATE:  5-26-83
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE DISPLAYS THE STATISTICS FOR RRS STATION DATA.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C        IRSTYP    A4    I     1     RRS DATA TYPE OR 'ALL'
C        ISTAFL    I     I     1     STATION ID FLAG
C                                      0=8-CHAR ID
C                                      1=NUMERIC ID
C        IUNFLG    I      I     1     UNITS FLAG
C                                      0=ENGLISH
C                                      1=METRIC
C
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'pdbcommon/pdrrsc'
      INCLUDE 'pdbcommon/pdtrrx'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdbdta'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
      PARAMETER (LSIBUF=128)
C
C
      CHARACTER*72 XLINE
C
      DIMENSION IDRAY(2,40)
      DIMENSION IRRBUF(1000)
C
      INTEGER*2 ISIBUF(LSIBUF)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil/RCS/pdrrst.f,v $
     . $',                                                             '
     .$Id: pdrrst.f,v 1.3 1998/04/07 14:38:31 page Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA LALL/4HALL /
C
C***********************************************************************
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,160)
C
      IRSFL=0
      IREC=INFREC+1
      NXREC=0
      IPRHDR=1
C
      CALL UPAGE (LP)
C
C  READ STATION ID CARD
10    CALL RIDCRD (ISTAFL,IDRAY,NUMID,LASTCD,ISTAT)
      IF (ISTAT.NE.0) GO TO 140
      I=NUMID
      IF (IDRAY(1,1).EQ.LALL) GO TO 40
C
C  FIND SIF RECORD FOR STATION ID
20    IF (ISTAFL.EQ.1) GO TO 30
      CALL PDFNDR (IDRAY(1,I),LSIBUF,IFIND,ISIREC,ISIBUF,IFREE,ISTAT)
      IF (ISTAT.NE.0) GO TO 140
      GO TO 50
C
C  FIND SIF RECORD FOR STATION NUMBER
30    CALL PDFNDI (IDRAY(1,I),LSIBUF,IFIND,ISIREC,ISIBUF,IFREE,ISTAT)
      IF (ISTAT.NE.0) GO TO 140
      GO TO 50
C
C  PRINT ALL STATIONS WITH RRS DATA
40    CALL PDRSIF (IREC,NXREC,LSIBUF,ISIBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 140
      GO TO 60
C
50    IF (IFIND.EQ.0) GO TO 110
C
C  FIND RRS DATA TYPE IN RECORD
60    JSIBUF=11
      NUM=ISIBUF(10)
      DO 100 K=1,NUM
         CALL UMEMOV (ISIBUF(JSIBUF),ITYPE,1)
C     CHECK IF DATA TYPE IS RRS
         IX=IPDCKR(ITYPE)
         IF (IX.EQ.0) GO TO 90
         IF (IRSTYP.EQ.LALL) GO TO 70
         IF (IRSTYP.NE.ITYPE) GO TO 90
70       NREC=ISIBUF(JSIBUF+2)
C     READ THE RRS RECORD
         CALL RPDLRS (LWNEED,ISTAT)
         IF (ISTAT.NE.0) GO TO 140
         MAXBUF=LWNEED
         CALL PDRRRR (NREC,LRCPDR,MAXBUF,IRRBUF,ISTA)
         IF (ISTA.NE.0) GO TO 140
         NLINEL=1
         CALL ULINEL (LP,NLINEL,IRETRN)
         IF (IPDDB.GT.0) WRITE (IOGDB,*) 'IPRHDR=',IPRHDR,
     *      ' IRETRN=',IRETRN
         IF (IPRHDR.EQ.0.AND.IRETRN.EQ.0) GO TO 80
C        WRITE HEADER FOR REPORT
            IPRHDR=0
            CALL ULINE (LP,2)
            WRITE (LP,170)
         CALL ULINE (LP,5)
         WRITE (LP,180)
C     CONVERT DATES FOR REPORT
80       KT=LHDRRS
         IF (IPDDB.GT.0) WRITE (IOGDB,*) ' KT=',KT
         CALL JLMDYH (IRRBUF(KT+1),JMO,JDAY,JYR,JHR)
         CALL JLMDYH (IRRBUF(KT+2),MRMO,MRDAY,MRYR,MRHR)
         CALL JLMDYH (IRRBUF(KT+5),LGMO,LGDAY,LGYR,LGHR)
         CALL JLMDYH (IRRBUF(KT+7),L2MO,L2DAY,L2YR,L2HR)
         CALL JLMDYH (IRRBUF(KT+9),ISMO,ISDAY,ISYR,ISHR)
         CALL JLMDYH (IRRBUF(KT+11),IS2MO,IS2DAY,IS2YR,IS2HR)
C     ENCODE THE DATE FOR REPORTS
         CALL UREPET (' ',XLINE,LEN(XLINE))
         IPOS=1
         IST=LHDRRS+4
C     SET NUMBER OF CHARACTERS AND DECIMAL PLACES
         NCHARF=8
         NDEC=NUMDEC(IX)
         NCHARI=-2
         IPRERR=1
C    ENCODE THE LARGEST VALUE AND DATE OF LARGEST REPORT
         IF (IPDDB.GT.0) WRITE (IOGDB,190) IST,IRRBUF(IST)
         CALL UFF2A (IRRBUF(IST),XLINE,IPOS,NCHARF,NDEC,IPRERR,LP,IERR)
         CALL UFI2A (LGMO,XLINE,IPOS+9,NCHARI,IPRERR,LP,IERR)
         CALL UMOVEX ('/',1,XLINE,IPOS+11,1)
         CALL UFI2A (LGDAY,XLINE,IPOS+12,NCHARI,IPRERR,LP,IERR)
         CALL UMOVEX ('/',1,XLINE,IPOS+14,1)
         CALL UFI2A (LGYR,XLINE,IPOS+15,NCHARI,IPRERR,LP,IERR)
C     SECOND LARGEST
         IF (IPDDB.GT.0) WRITE (IOGDB,190) IST+2,IRRBUF(IST+2)
         CALL UFF2A (IRRBUF(IST+2),XLINE,IPOS+18,NCHARF,NDEC,IPRERR,LP,
     *      IERR)
         CALL UFI2A (L2MO,XLINE,IPOS+27,NCHARI,IPRERR,LP,IERR)
         CALL UMOVEX ('/',1,XLINE,IPOS+29,1)
         CALL UFI2A (L2DAY,XLINE,IPOS+30,NCHARI,IPRERR,LP,IERR)
         CALL UMOVEX ('/',1,XLINE,IPOS+32,1)
         CALL UFI2A (L2YR,XLINE,IPOS+33,NCHARI,IPRERR,LP,IERR)
C     SMALLEST
         IF (IPDDB.GT.0) WRITE (IOGDB,190) IST+4,IRRBUF(IST+4)
         CALL UFF2A (IRRBUF(IST+4),XLINE,IPOS+36,NCHARF,NDEC,IPRERR,LP,
     *      IERR)
         CALL UFI2A (ISMO,XLINE,IPOS+45,NCHARI,IPRERR,LP,IERR)
         CALL UMOVEX ('/',1,XLINE,IPOS+47,1)
         CALL UFI2A (ISDAY,XLINE,IPOS+48,NCHARI,IPRERR,LP,IERR)
         CALL UMOVEX ('/',1,XLINE,IPOS+50,1)
         CALL UFI2A (ISYR,XLINE,IPOS+51,NCHARI,IPRERR,LP,IERR)
C     SECOND SMALLEST
         IF (IPDDB.GT.0) WRITE (IOGDB,190) IST+6,IRRBUF(IST+6)
         CALL UFF2A (IRRBUF(IST+6),XLINE,IPOS+54,NCHARF,NDEC,IPRERR,LP,
     *      IERR)
         CALL UFI2A (IS2MO,XLINE,IPOS+63,NCHARI,IPRERR,LP,IERR)
         CALL UMOVEX ('/',1,XLINE,IPOS+65,1)
         CALL UFI2A (IS2DAY,XLINE,IPOS+66,NCHARI,IPRERR,LP,IERR)
         CALL UMOVEX ('/',1,XLINE,IPOS+68,1)
         CALL UFI2A (IS2YR,XLINE,IPOS+69,NCHARI,IPRERR,LP,IERR)
C     GET UNITS
         CALL PFDUNT (ITYPE,IUNFLG,IUNIT)
         KTP3=KT+3
         KTP12=KT+12
C     PRINT STATISTICS
         CALL ULINE (LP,1)
         WRITE (LP,200) (IRRBUF(N),N=2,4),JMO,JDAY,JYR,IRRBUF(5),IUNIT,
     *     MRMO,MRDAY,MRYR,IRRBUF(KTP3),IRRBUF(KTP12),XLINE
         IRSFL=1
90       JSIBUF=JSIBUF+3
100      CONTINUE
C
      IF (IRSFL.EQ.1.OR.IDRAY(1,1).EQ.LALL) GO TO 120
      WRITE (LP,210) IDRAY(1,I),IDRAY(2,I)
      GO TO 120
C
C  STATION NOT DEFINED
110   WRITE (LP,220)
C
C  ALL STATIONS ARE PRINTED
120   IF (IDRAY(1,1).NE.LALL) GO TO 130
      IF (NXREC.GT.LSTSIF) GO TO 150
      IREC=NXREC
      GO TO 40
C
C  CHECK FOR NEXT STATION
130   I=I-1
      IF (I.NE.0) GO TO 20
C
      IF (IRSFL.EQ.0.AND.LASTCD.EQ.0) GO TO 150
C
C  CHECK IF MORE CARDS TO READ
      IF (LASTCD.NE.0) GO TO 10
      GO TO 150
C
C  SYSTEM ERROR
140   WRITE (LP,230)
C
150   IF (IPDTR.GT.0) WRITE (IOGDB,240)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
160   FORMAT (' *** ENTER PDRRST')
170   FORMAT ('0*** STATISTICS FOR RRS STATIONS ***')
180   FORMAT ('0',36X,'DATE OF',2X,'TOTAL',3X,'FREE',20X,'SECOND',2X,
     *      'DATE OF',21X,'SECOND',2X,'DATE OF' /
     *   5X,'STATION',6X,'DATE',4X,'DATA',8X,'MOST',2X,'NUMBER OF POOL',
     *       2X,'LARGEST',2X,'DATE OF',2X,'LARGEST',1X,'SECOND',3X,
     *      'SMALLEST',1X,'DATE OF',2X,'SMALLEST',2X,'SECOND' /
     *   4X,'ID',4X,'NUMBER',2X,'RESET',3X,'TYPE UNITS',1X,'RECENT',2X,
     *      'REPORTS WRITES',2X,'REPORT',2X,'LARGEST',2X,'REPORT',2X,
     *      'LARGEST',3X,'REPORT',2X,'SMALLEST',2X,'REPORT',2X,
     *      'SMALLEST' /
     *   1X,8('-'),1X,6('-'),1X,8('-'),1X,
     *      4('-'),1X,5('-'),1X,8('-'),1X,6('-'),1X,6('-'),1X,
     *      4(7('-'),1X,8('-'),2X))
190   FORMAT (' IST=',I4,3X,'IRRBUF(IST)=',F10.3)
200   FORMAT (1X,2A4,1X,I5,2X,
     *   I2.2,'/',I2.2,'/',I2.2,1X,A4,2X,A4,1X,
     *   I2.2,'/',I2.2,'/',I2.2,2X,I4,2X,I4,2X,A)
210   FORMAT ('0**NOTE** RRS TYPE NOT FOUND FOR STATION ',2A4,'.')
220   FORMAT ('0**ERROR** STATION NOT DEFINED.')
230   FORMAT ('0**ERROR** IN PDRRST - DAIO ERROR')
240   FORMAT (' *** EXIT PDRRST')
C
      END
