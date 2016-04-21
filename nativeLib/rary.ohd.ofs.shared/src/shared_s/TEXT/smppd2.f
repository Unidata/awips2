C MEMBER SMPPD2
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 08/19/94.08:25:31 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE SMPPD2 (NSTA,STATE,DESCRP,ISUMRY,ISIBUF,ISTAT)
C
C  PRINT STATISTICS FOR PCPN STATIONS
C
      CHARACTER*8 RSDATE,MRDATE,LGDATE,L2DATE
      CHARACTER*8 XFORM/'  /  /  '/
      CHARACTER*8 XNONE/'  NONE  '/
C
      INTEGER*2 ISIBUF(128)
C
      DIMENSION DESCRP(5)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'hclcommon/hdflts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/smppd2.f,v $
     . $',                                                             '
     .$Id: smppd2.f,v 1.1 1995/09/17 19:20:30 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) '*** ENTER SMPPD2'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DUMP')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NSTA=',NSTA,
     *      ' ISUMRY=',ISUMRY
         CALL SULINE (IOSDBG,1)
         ENDIF
      ISTAT=0
C
      IF (NSTA.EQ.1) THEN
         IF (ISLEFT(10).GT.0) CALL SUPAGE
         WRITE (LP,50)
         CALL SULINE (LP,2)
         IF (ISUMRY.EQ.0) THEN
            WRITE (LP,60)
            CALL SULINE (LP,5)
            ENDIF
         IF (ISUMRY.EQ.1) THEN
            WRITE (LP,70)
            CALL SULINE (LP,5)
            ENDIF
         ENDIF
C
C  CALCULATE WHERE STATISTICS BEGIN IN FILE
      ISTRT=ISIBUF(10)*3+10
C
C  GET DATE STATISTICS RESET
      CALL UMEMOV (ISIBUF(ISTRT+1),JDAY,1)
      IF (JDAY.EQ.0) THEN
         RSDATE=XNONE
         GO TO 10
         ENDIF
      CALL MDYH2 (JDAY,0,IRMO,IRDAY,IRYR,IHR,ITZ,IDSAV,TIME(3))
      IRYR=MOD(IRYR,100)
      RSDATE=XFORM
      IPRERR=1
      CALL UFI2A (IRMO,RSDATE,1,-2,IPRERR,LP,IERR)
      CALL UFI2A (IRDAY,RSDATE,4,-2,IPRERR,LP,IERR)
      CALL UFI2A (IRYR,RSDATE,7,-2,IPRERR,LP,IERR)
C
C  GET DATE OF MOST RECENT REPORT
10    CALL UMEMOV (ISIBUF(ISTRT+3),MDAY,1)
      IF (MDAY.EQ.0) THEN
         MRDATE=XNONE
         GO TO 20
         ENDIF
      CALL MDYH2 (MDAY,0,MRMO,MRDAY,MRYR,MRHR,ITZ,IDSAV,TIME(3))
      MRYR=MOD(MRYR,100)
      MRDATE=XFORM
      IPRERR=1
      CALL UFI2A (MRMO,MRDATE,1,-2,IPRERR,LP,IERR)
      CALL UFI2A (MRDAY,MRDATE,4,-2,IPRERR,LP,IERR)
      CALL UFI2A (MRYR,MRDATE,7,-2,IPRERR,LP,IERR)
C
C  GET DATE OF LARGEST REPORT
20    CALL UMEMOV (ISIBUF(ISTRT+11),LDAY,1)
      IF (LDAY.EQ.0) THEN
         LGDATE=XNONE
         GO TO 30
         ENDIF
      CALL MDYH2 (LDAY,0,LGMO,LGDAY,LGYR,LGHR,ITZ,IDSAV,TIME(3))
      LGYR=MOD(LGYR,100)
      LGDATE=XFORM
      IPRERR=1
      CALL UFI2A (LGMO,LGDATE,1,-2,IPRERR,LP,IERR)
      CALL UFI2A (LGDAY,LGDATE,4,-2,IPRERR,LP,IERR)
      CALL UFI2A (LGYR,LGDATE,7,-2,IPRERR,LP,IERR)
C
C  GET DATE OF SECOND LARGEST REPORT
30    CALL UMEMOV (ISIBUF(ISTRT+14),L2DAY,1)
      IF (L2DAY.EQ.0) THEN
         L2DATE=XNONE
         GO TO 40
         ENDIF
      CALL MDYH2 (L2DAY,0,LG2MO,LG2DAY,LG2YR,L2HR,ITZ,IDSAV,TIME(3))
      LG2YR=MOD(LG2YR,100)
      L2DATE=XFORM
      IPRERR=1
      CALL UFI2A (LG2MO,L2DATE,1,-2,IPRERR,LP,IERR)
      CALL UFI2A (LG2DAY,L2DATE,4,-2,IPRERR,LP,IERR)
      CALL UFI2A (LG2YR,L2DATE,7,-2,IPRERR,LP,IERR)
C
C  CONVERT INTEGER VALUES TO REAL
40    RPTLG=ISIBUF(ISTRT+10)
      RPTLG=RPTLG/100
      RPT2LG=ISIBUF(ISTRT+13)
      RPT2LG=RPT2LG/100
      SMNOZO=ISIBUF(ISTRT+16)
      SMNOZO=SMNOZO/100
C
C  GET REPORTING VALUES
      NDYRPT=ISIBUF(ISTRT+5)
      NTLRPT=ISIBUF(ISTRT+6)
      NDZRPT=ISIBUF(ISTRT+7)
      NNZRPT=NDYRPT-NDZRPT
C
C  GET PRECIPITATION VALUES
      CALL UMEMOV (ISIBUF(ISTRT+8),ACPRP,1)
      CALL UMEMOV (ISIBUF(ISTRT+17),ACPSQ,1)
C
C  PRINT STATISTICS
      IF (ISNWPG(LP).EQ.1) THEN
         IF (ISUMRY.EQ.0) THEN
            WRITE (LP,60)
            CALL SULINE (LP,5)
            ENDIF
         IF (ISUMRY.EQ.1) THEN
             WRITE (LP,70)
             CALL SULINE (LP,5)
             ENDIF
         ENDIF
      IF (ISUMRY.EQ.0)
     *   WRITE (LP,80) NSTA,(ISIBUF(J),J=2,6),
     *      RSDATE,
     *      MRDATE,
     *      NDYRPT,NTLRPT,NDZRPT,
     *      ACPRP,
     *      RPTLG,LGDATE,
     *      RPT2LG,L2DATE,
     *      SMNOZO,ACPSQ
      IF (ISUMRY.EQ.1)
     *   WRITE (LP,90) NSTA,(ISIBUF(J),J=2,5),DESCRP,STATE,
     *      NTLRPT,NDYRPT,NNZRPT,
     *      ACPRP,
     *      RPTLG,LGDATE,
     *      MRDATE,
     *      RSDATE
      CALL SULINE (LP,1)
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) '*** EXIT SMPPD2'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT ('0- STATISTICS FOR PCPN STATIONS 24-HR TOTAL ',
     *   '(UNITS=IN) -')
60    FORMAT (1H0,31X,'DATE OF',2X,'NUMBER OF',1X,'TOTAL',5X,'NUMBER',
     *      ' OF',27X,'SECOND',2X,'DATE OF',2X,'SMALLEST',1X,'ACCUM' /
     *   9X,'STATION',6X,'DATE',6X,'MOST',5X,'DAYS WITH NUMBER',
     *      ' OF DAYS WITH',1X,'ACCUM',3X,'LARGEST',2X,'DATE OF',2X,
     *      'LARGEST',1X,'SECOND',3X,'NONZERO',2X,'PRECIP' /
     *   6X,'ID',7X,'NUM',4X,'RESET',5X,'RECENT',3X,'REPORTS',3X,
     *      'REPORTS  ',1X,'ZERO PCPN',1X,'PRECIP',2X,'REPORT',2X,
     *      'LARGEST',3X,'REPORT',2X,'LARGEST',2X,'REPORT',3X,
     *      'SQUARED' /
     *   6X,8('-'),1X,5('-'),2X,8('-'),2X,8('-'),3(1X,9('-')),1X,6('-'),
     *      2(2X,7('-'),1X,8('-')),1X,8('-'),1X,8('-'))
70    FORMAT (1H0,43X,'TOTAL',6X,'NUMBER OF',2X,'NUMBER OF',37X,
     *       'MOST',6X,'STARTING' /
     *   10X,'STATION',27X,'NUMBER OF',2X,'DAYS WITH',2X,'NON-ZERO',3X,
     *      'ACCUM',3X,'LARGEST',2X,'DATE OF',11X,'RECENT',4X,
     *      'DATE OF' /
     *   6X,'ID',7X,'DESCRIPTION',11X,'STATE',2X,'REPORTS',4X,
     *      'REPORTS',4X,'DAYS',7X,
     *      'PRECIP',2X,'REPORT',3X,'LARGEST',11X,
     *      'REPORT',4X,'STATS' /
     *   6X,8('-'),1X,20('-'),2X,5('-'),2X,
     *      9('-'),2X,9('-'),2X,9('-'),2X,
     *      6('-'),2X,
     *      7('-'),2X,8('-'),10X,
     *      8('-'),2X,8('-'))
80    FORMAT (1H ,I4,1X,4A2,1X,I5,2X,
     *   A8,2X,
     *   A8,3X,
     *   I5,5X,I5,5X,I5,3X,
     *   F6.2,2X,
     *   F7.2,1X,A8,2X,
     *   F7.2,1X,A8,1X,
     *   F8.2,1X,F8.3)
90    FORMAT (1H ,I4,1X,4A2,1X,5A4,2X,A2,7X,
     *   I5,6X,I5,6X,I5,4X,
     *   F6.2,2X,
     *   F7.2,2X,A8,10X,
     *   A8,2X,
     *   A8)
C
      END
