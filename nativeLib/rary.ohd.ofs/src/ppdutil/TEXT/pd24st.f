C MODULE PD24ST
C-----------------------------------------------------------------------
C
       SUBROUTINE PD24ST (ISTAFL,IUNFLG)
C
C
C             ROUTINE:  PD24ST
C             VERSION:  1.0.0
C                DATE:  5-20-83
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE DISPLAYS THE 24 HOUR PRECIPITATION STATISTICS FOR
C    THE 8-CHAR OR NUMERIC STATION ID SPECIFIED.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C        ISTAFL    I     I     1     STATION FLAG
C                                     0=STATION IS 8 CHAR ID
C                                     1=STATION NUMBER
C        IUNFLG    I     I     1     PRECIPITATION UNITS FLAG
C                                     0=ENGLISH
C                                     1=METRIC
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdsifc'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
      PARAMETER (LSIBUF=128)
C
C
      CHARACTER*4 DUNITS
C
      DIMENSION IDRAY(2,40)
C
      INTEGER*2 ISIBUF(LSIBUF)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil/RCS/pd24st.f,v $
     . $',                                                             '
     .$Id: pd24st.f,v 1.3 1998/04/07 14:36:49 page Exp $
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
      IF (IPDTR.GT.0) WRITE (IOGDB,150)
C
      DUNITS='IN'
      IPRINT=0
      IREC=INFREC+1
      NXREC=0
      IPRHDR=1
C
      CALL UPAGE (LP)
C
C  READ STATION ID CARD
10    CALL RIDCRD (ISTAFL,IDRAY,NUMID,LASTCD,ISTAT)
      IF (ISTAT.NE.0) GO TO 120
C
C  CHECK UNITS FLAG
      IF (IUNFLG.EQ.1) DUNITS='MM'
      I=NUMID
      IF (IDRAY(1,1).EQ.LALL) GO TO 40
C
20    IF (ISTAFL.EQ.1) GO TO 30
C
C  FIND SIF RECORD FOR STATION ID
      CALL PDFNDR (IDRAY(1,I),LSIBUF,IFIND,ISIREC,ISIBUF,IFREE,ISTAT)
      IF (ISTAT.NE.0) GO TO 120
      GO TO 50
C
C  FIND SIF RECORD FOR STATION NUMBER
30    CALL PDFNDI (IDRAY(1,I),LSIBUF,IFIND,ISIREC,ISIBUF,IFREE,ISTAT)
      IF (ISTAT.NE.0) GO TO 120
      GO TO 50
C
C  PRINT ALL PP24 STATISTICS
40    CALL PDRSIF (IREC,NXREC,LSIBUF,ISIBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 120
      IF (ISIBUF(8).EQ.0) GO TO 80
      GO TO 60
C
C  CALCULATE WHERE STATS BEGIN IN FILE
50    IF (IFIND.EQ.0) GO TO 100
C
60    IST=ISIBUF(10)*3+10
C
C  WRITE HEADER FOR REPORT
      NLINEL=1
      CALL ULINEL (LP,NLINEL,IRETRN)
      IF (IPDDB.GT.0) WRITE (IOGDB,*) 'IPRHDR=',IPRHDR,
     *   ' IRETRN=',IRETRN
      IF (IPRHDR.EQ.0.AND.IRETRN.EQ.0) GO TO 70
         IPRHDR=0
         CALL ULINE (LP,2)
         WRITE (LP,160) DUNITS
C
      CALL ULINE (LP,5)
      WRITE (LP,170)
C
C  CONVERT DATES FOR REPORT
70    CALL UMEMOV (ISIBUF(IST+1),JDAY,1)
      CALL MDYH2 (JDAY,0,IMO,IDAY,IYR,IHR,ITZ,IDSAV,TIME(3))
      IYR=MOD(IYR,100)
      CALL UMEMOV (ISIBUF(IST+3),MDAY,1)
      CALL MDYH2 (MDAY,0,MRMO,MRDAY,MRYR,MRHR,ITZ,IDSAV,TIME(3))
      MRYR=MOD(MRYR,100)
      CALL UMEMOV (ISIBUF(IST+11),LDAY,1)
      CALL MDYH2 (LDAY,0,LGMO,LGDAY,LGYR,LGHR,ITZ,IDSAV,TIME(3))
      LGYR=MOD(LGYR,100)
      CALL UMEMOV (ISIBUF(IST+14),L2DAY,1)
      CALL MDYH2 (L2DAY,0,L2MO,LG2DAY,L2YR,L2HR,ITZ,IDSAV,TIME(3))
      L2YR=MOD(L2YR,100)
C
C  CONVERT INTEGER VALUES TO REAL
      RPTLG=ISIBUF(IST+10)
      RPTLG=RPTLG/100
      RPT2LG=ISIBUF(IST+13)
      RPT2LG=RPT2LG/100
      SMNOZO=ISIBUF(IST+16)
      SMNOZO=SMNOZO/100
C
C  MOVE PRECIPITATION VALUES FOR PRINTING
      CALL UMEMOV (ISIBUF(IST+8),ACPRP,1)
      CALL UMEMOV (ISIBUF(IST+17),ACPSQ,1)
C
      CALL ULINE (LP,1)
      WRITE (LP,180) (ISIBUF(J),J=2,6),IMO,IDAY,IYR,MRMO,MRDAY,
     *            MRYR,(ISIBUF(IST+J),J=5,7),ACPRP,RPTLG,LGMO,
     *            LGDAY,LGYR,RPT2LG,L2MO,LG2DAY,L2YR,SMNOZO,ACPSQ
      IPRINT=1
C
80    IF (IDRAY(1,1).NE.LALL) GO TO 90
      IF (NXREC.GT.LSTSIF) GO TO 130
      IREC=NXREC
      GO TO 40
C
C  CHECK FOR NEXT STATION
90    I=I-1
      IF (I.NE.0) GO TO 20
      GO TO 110
C
C  STATION NOT DEFINED
100   WRITE (LP,190)
C
C  CHECK IF MORE ID CARDS TO READ
110   IF (LASTCD.EQ.0) GO TO 130
      GO TO 10
C
C  SYSTEM ERROR
120   WRITE (LP,200)
      GO TO 140
C
130   IF (IPRINT.EQ.0.AND.IDRAY(1,1).EQ.LALL) WRITE (LP,190)
C
140   IF (IPDTR.GT.0) WRITE (IOGDB,210)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
150   FORMAT (' *** ENTER PD24ST')
160   FORMAT ('0*** STATISTICS FOR PCPN STATIONS 24-HR TOTAL ***',3X,
     *   'DATA UNITS = ',A)
170   FORMAT ('0',29X,'DATE OF',2X,'NUMBER OF',3X,'TOTAL',3X,'NUMBER',
     *      ' OF',29X,'SECOND',3X,'DATE OF SMALLEST',3X,'ACCUM' /
     *   6X,'STATION',8X,'DATE',6X,'MOST',4X,'DAYS WITH NUMBER',1X,
     *      'OF DAYS WITH',3X,'ACCUM',3X,'LARGEST',2X,'DATE OF',2X,
     *      'LARGEST',3X,'SECOND',2X,'NONZERO',3X,'PRECIP' /
     *   5X,'ID',4X,'NUMBER',4X,'RESET',4X,'RECENT',4X,'REPORTS',3X,
     *      'REPORTS',2X,'ZERO PCPN',2X,'PRECIP',3X,'REPORT',3X,
     *      'LARGEST',2X,'REPORT',3X,'LARGEST',2X,'REPORT',3X,
     *      'SQUARED' /
     *   2X,8('-'),1X,7('-'),1X,8('-'),2X,8('-'),1X,3(1X,9('-')),
     *      5(2X,7('-')),1X,8('-'),1X,9('-'))
180   FORMAT (2X,4A2,1X,I6,2X,2(I2.2,'/',I2.2,'/',I2.2,2X),1X,
     *   I4,7X,I4,6X,I4,2X,2(F8.2,1X),2X,I2.2,'/',I2.2,'/',I2.2,1X,
     *   F6.2,3X,I2.2,'/',I2.2,'/',I2.2,2X,F5.2,2X,F9.4)
190   FORMAT ('0**ERROR** STATION NOT DEFINED.')
200   FORMAT ('0**ERROR** IN PD24ST - DAIO ERROR.')
210   FORMAT (' *** EXIT PD24ST')
C
      END
