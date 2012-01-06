C MODULE SSPRD
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT THE STATUS OF THE PROCESSED DATA BASE.
C
      SUBROUTINE SSPRD (LARRAY,ARRAY,LEVEL,NFLD,IFLCHK,ISTAT)
C
      CHARACTER*7 XCLNGC(4)/'PP','FC','ANY','N/A'/
      CHARACTER*8 XPRCOD(3)/'INST','ACCM','MEAN'/
      CHARACTER*8 XFILES(6)
     *   /'PRDTS1  ','PRDTS2  ','PRDTS3  ',
     *    'PRDTS4  ','PRDTS5  ','????????'/
      CHARACTER*8 CNTTYP,TSIDX
      CHARACTER*9 XDIMNS(13)
     *   /'L   ','L2  ','L3  ','L/T ','L3/T','E/L2',
     *    'PRES','TEMP','DLES','TIME','DIR ','E   ','E/T '/
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION IBUF(4)
C
      INCLUDE 'uiox'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suoptx'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/pmaxdm'
      INCLUDE 'prdcommon/punits'
      INCLUDE 'prdcommon/ptsctl'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'urcommon/urtscl'
      INCLUDE 'urcommon/urftbl'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/ssprd.f,v $
     . $',                                                             '
     .$Id: ssprd.f,v 1.6 2004/01/23 16:30:09 scv Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SSPRD'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('STAT')
C
      ISTAT=0
C
      MFILES=5
C
C  PRINT HEADER
      IF (ISLEFT(5).GT.0) CALL SUPAGE
      WRITE (LP,120)
      CALL SULINE (LP,2)
      WRITE (LP,140)
      IF (IOPOVP.EQ.1) THEN
         WRITE (LP,140)
         CALL SULINE (LP,0)
         WRITE (LP,140)
         CALL SULINE (LP,0)
         ENDIF
C
C  OPEN DATA BASE
      CALL SUDOPN (1,'PRD ',IERR)
      IF (IERR.GT.0) GO TO 100
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT INDEX FILE STATUS
C
      IF (LEVEL.GT.0) THEN
         WRITE (LP,160) USERPR
         CALL SULINE (LP,4)
         MAXIDX=MAXTMS*2
         IOVFL=MAXIDX*.87225
         WRITE (LP,170) MAXIDX,IOVFL
         CALL SULINE (LP,2)
         NDEF=0
         NDEL=0
         LSTREC=IOVFL-1
         DO 10 IREC=1,LSTREC
            CALL UREADT (KINDEX,IREC,IBUF,IERR)
            IF (IERR.NE.0) THEN
               WRITE (LP,180) IREC,KINDEX
               CALL SUERRS (LP,2,NUMERR)
               GO TO 10
               ENDIF
            IF (IBUF(1).EQ.0) GO TO 10
            IF (IBUF(1).EQ.-1) NDEL=NDEL+1
            IF (IBUF(1).NE.-1) NDEF=NDEF+1
10          CONTINUE
         MREC=IOVFL-1
         WRITE (LP,190) MREC,NDEF,NDEL
         CALL SULINE (LP,2)
         NDEF=0
         NDEL=0
         DO 20 IREC=IOVFL,MAXIDX
            CALL UREADT (KINDEX,IREC,IBUF,IERR)
            IF (IERR.NE.0) THEN
               WRITE (LP,180) IREC,KINDEX
               CALL SUERRS (LP,2,NUMERR)
               GO TO 20
               ENDIF
            IF (IBUF(1).EQ.0) GO TO 20
            IF (IBUF(1).EQ.-1) NDEL=NDEL+1
            IF (IBUF(1).NE.-1) NDEF=NDEF+1
20          CONTINUE
         MREC=MAXIDX-IOVFL+1
         WRITE (LP,200) MREC,NDEF,NDEL
         CALL SULINE (LP,2)
         WRITE (LP,210) MAXTMS,NUMTMS
         CALL SULINE (LP,2)
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT TIME SERIES FILE STATUS
C
C  SET UNIT NUMBER OFFSET
      IF (IAMORD.EQ.0) IOFSET=0
      IF (IAMORD.EQ.1) IOFSET=KUPRDO
C
      IF (ISLEFT(5).GT.0) CALL SUPAGE
      WRITE (LP,220)
      CALL SULINE (LP,2)
      IF (ISLEFT(5).GT.0) CALL SUPAGE
      WRITE (LP,230)
      CALL SULINE (LP,4)
      DO 40 I=1,NMPRDF
         IF (IAMORD.EQ.0) IUNIT=TSCNTR(1,I)
         IF (IAMORD.EQ.1) IUNIT=ITSCNT(1,I)
         NUNIT=IUNIT-IOFSET
         NFILE=IUNIT-KPRDTU(1)-IOFSET+1
         IF (NFILE.GT.0.AND.NFILE.LE.MFILES.AND.NFILE.LE.NMPRDF) THEN
            ELSE
               WRITE (LP,240) NFILE
               CALL SUWRNS (LP,2,-1)
               NFILE=6
            ENDIF
         IF (IAMORD.EQ.0) THEN
            MAXREC=TSCNTR(2,I)
            LSTREC=TSCNTR(3,I)-1
            NUMTYP=TSCNTR(4,I)
            ENDIF
         IF (IAMORD.EQ.1) THEN
            MAXREC=ITSCNT(2,I)
            LSTREC=ITSCNT(3,I)-1
            NUMTYP=ITSCNT(4,I)
            ENDIF
         NFREE=MAXREC-LSTREC
         IPCT=(FLOAT(LSTREC)/FLOAT(MAXREC))*100.+.5
         WRITE (LP,250) I,MAXREC,LSTREC,NFREE,IPCT,NUMTYP,NUNIT,
     *      XFILES(NFILE)
         CALL SULINE (LP,1)
         IF (ISNWPG(LP).EQ.1) THEN
            WRITE (LP,230)
            CALL SULINE (LP,4)
            ENDIF
         IF (IFLCHK.NE.0) THEN
            CALL SSCHK (NUNIT,MAXREC,IPCT,IFLCHK,'PRD ',IERR)
            ENDIF
40       CONTINUE
C
      IF (NUMDTP.EQ.0) THEN
         WRITE (LP,260)
         CALL SULINE (LP,2)
         GO TO 100
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT DATA TYPE DIRECTORY STATUS
C
      IF (LEVEL.GT.0) THEN
         IF (ISLEFT(4).GT.0) CALL SUPAGE
         WRITE (LP,270)
         CALL SULINE (LP,2)
         WRITE (LP,295) MINDAY
         CALL SULINE (LP,2)
         WRITE (LP,290) MAXDTP,NUMDTP
         CALL SULINE (LP,2)
         WRITE (LP,300)
         CALL SULINE (LP,4)
         NCOUNT=0
         MCOUNT=5
         DO 90 I=1,NUMDTP
            IF (IAMORD.EQ.0) THEN
               IDIMNS=DATFIL(12,I)
               IPRCOD=DATFIL(6,I)
               ICLNGC=DATFIL(11,I)+1
               ITYPE=DATFIL(1,I)
               IFUT=DATFIL(7,I)
               ENDIF
            IF (IAMORD.EQ.1) THEN
               IDIMNS=IDATFL(12,I)
               IPRCOD=IDATFL(6,I)
               ICLNGC=IDATFL(11,I)+1
               ITYPE=IDATFL(1,I)
               IFUT=IDATFL(7,I)
               ENDIF
            CNTTYP='REGULAR'
            IF (IFUT.LT.0) THEN
               IF (IAMORD.EQ.0) THEN
                  II=-DATFIL(7,I)
                  ITYPE=DATFIL(1,II)
                  ENDIF
               IF (IAMORD.EQ.1) THEN
                  II=-IDATFL(7,I)
                  ITYPE=IDATFL(1,II)
                  ENDIF
               CNTTYP='FUTURE'
               ENDIF
            IF (IFUT.EQ.0) CNTTYP='MIXED'
            IF (IAMORD.EQ.0) IUNIT=DATFIL(2,I)
            IF (IAMORD.EQ.1) IUNIT=IDATFL(2,I)
            NUNIT=IUNIT-IOFSET
            NFILE=IUNIT-KPRDTU(1)-IOFSET+1
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,*) 'IN SSPRD - IUNIT=',IUNIT,
     *            ' KPRDTU(1)=',KPRDTU(1),' IOFSET=',IOFSET,
     *            ' NFILE=',NFILE
               CALL SULINE (IOSDBG,1)
               ENDIF
            IF (NFILE.GT.0.AND.NFILE.LE.MFILES.AND.NFILE.LE.NMPRDF) THEN
               ELSE
                  WRITE (LP,240) NFILE
                  CALL SUWRNS (LP,2,-1)
                  NFILE=6
               ENDIF
            IF (ISNWPG(LP).EQ.1.OR.NCOUNT.LT.MCOUNT) GO TO 80
               IF (ISLEFT(2).EQ.1) THEN
                  CALL SUPAGE
                  WRITE (LP,300)
                  CALL SULINE (LP,4)
                  NCOUNT=0
                  GO TO 80
                  ENDIF
               WRITE (LP,130)
               CALL SULINE (LP,1)
               NCOUNT=0
80          INEWPG=ISNWPG(LP)
            IF (INEWPG.EQ.1) THEN
               WRITE (LP,300)
               CALL SULINE (LP,4)
               NCOUNT=0
               ENDIF
            IF (IAMORD.EQ.0) THEN
               WRITE (LP,310) I,ITYPE,CNTTYP,NUNIT,XFILES(NFILE),
     *            (DATFIL(J,I),J=4,5),XPRCOD(IPRCOD),DATFIL(7,I),
     *            XCLNGC(ICLNGC),XDIMNS(IDIMNS),DATFIL(15,I)
               CALL SULINE (LP,1)
               ENDIF
            IF (IAMORD.EQ.1) THEN
               WRITE (LP,310) I,ITYPE,CNTTYP,NUNIT,XFILES(NFILE),
     *            (IDATFL(J,I),J=4,5),XPRCOD(IPRCOD),IDATFL(7,I),
     *            XCLNGC(ICLNGC),XDIMNS(IDIMNS),IDATFL(15,I)
               CALL SULINE (LP,1)
               ENDIF
            NCOUNT=NCOUNT+1
90          CONTINUE
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (LEVEL.GT.1) THEN
         TSIDX=' '
         CALL SSPRD1 (LARRAY,ARRAY,TSIDX,NFLD,IERR)
         ENDIF
C
100   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SSPRD : ISTAT=',ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
120   FORMAT ('0')
130   FORMAT (' ')
140   FORMAT ('+*--> PROCESSED DATA BASE STATUS')
160   FORMAT ('0- INDEX FILE STATUS -' /
     *   '0',2X,'USER NAME = ',2A4)
170   FORMAT ('0',2X,'RECORDS IN FILE = ',I5,5X,
     *   'FIRST INDEX OVERFLOW RECORD = ',I5)
180   FORMAT ('0*** ERROR - IN SSPRD - DAIO ERROR AT RECORD ',I5,
     *    ' OF UNIT ',I2,'.')
190   FORMAT ('0',2X,'PRIMARY  AREA:',5X,
     *   'MAXIMUM ENTRIES = ',I5,3X,
     *   'ENTRIES DEFINED = ',I5,3X,
     *   'ENTRIES DELETED = ',I5)
200   FORMAT ('0',2X,'OVERFLOW AREA:',5X,
     *   'MAXIMUM ENTRIES = ',I5,3X,
     *   'ENTRIES DEFINED = ',I5,3X,
     *   'ENTRIES DELETED = ',I5)
210   FORMAT ('0',2X,'MAXIMUM TIME SERIES = ',I5,5X,
     *   'TIME SERIES DEFINED = ',I5)
220   FORMAT ('0- DATA FILE STATUS -')
230   FORMAT (
     *   '0',2X,
     *       4(' '),3X,
     *       'MAXIMUM',3X,
     *       'LAST USED',3X,
     *       'UNUSED ',3X,
     *       'PERCENT',3X,
     *       'NUMBER OF',3X,
     *       'UNIT  ',3X,
     *       'FILE' /
     *   ' ',2X,
     *       'FILE',3X,
     *       'RECORDS',3X,
     *       'RECORD   ',3X,
     *       'RECORDS',3X,
     *       'USED   ',3X,
     *       'TYPES    ',3X,
     *       'NUMBER',3X,
     *       'NAME' /
     *   ' ',2X,
     *       4('-'),3X,
     *       7('-'),3X,
     *       9('-'),3X,
     *       7('-'),3X,
     *       7('-'),3X,
     *       9('-'),3X,
     *       6('-'),3X,
     *       8('-'))
240   FORMAT ('0*** WARNING - ',I5,' IS AN INVALID PROCESSED ',
     *   'DATA BASE ORDINAL FILE NUMBER.')
250   FORMAT (
     *   ' ',2X,
     *   I4,3X,
     *   I7,3X,
     *   I9,3X,
     *   I7,3X,
     *   I7,3X,
     *   I9,3X,
     *   I6,3X,
     *   A8)
260   FORMAT ('0*** NOTE - NO DATA TYPES ARE DEFINED.')
270   FORMAT ('0- DATA TYPE DIRECTORY STATUS -')
290   FORMAT ('0',2X,'MAXIMUM DATA TYPES = ',I3,5X,
     *   'DATA TYPES DEFINED = ',I3)
295   FORMAT ('0',2X,'MINIMUM DAYS OF REGULAR DATA TO BE RETAINED ',
     *   'WHEN UPDATING MIXED TIME SERIES = ',I3)
300   FORMAT (
     *   '0',2X,1X,
     *       4(' '),3X,
     *       8(' '),3X,
     *       6(' '),3X,
     *       8(' '),3X,
     *       4(' '),3X,
     *       'SMALLEST',3X,
     *       'TIME INTERVAL',3X,
     *       'FUTURE-',3X,
     *       'CALLING',3X,
     *       9(' '),3X,
     *       'NUMBER OF' /
     *   ' ',2X,1X,
     *       'DATA',3X,
     *       8(' '),3X,
     *       'UNIT  ',3X,
     *       'FILE    ',3X,
     *       'MAX ',3X,
     *       'TIME    ',3X,
     *       'PROCESSING   ',3X,
     *       'REGULAR',3X,
     *       'ROUTINE',3X,
     *       'UNITS    ',3X,
     *       'TIME SERIES' /
     *   ' ',2X,1X,
     *       'TYPE',3X,
     *       'CONTENTS',3X,
     *       'NUMBER',3X,
     *       'NAME    ',3X,
     *       'DAYS',3X,
     *       'INTERVAL',3X,
     *       'CODE         ',3X,
     *       'POINTER',3X,
     *       'CODE   ',3X,
     *       'DIMENSION',3X,
     *       'DEFINED' /
     *   ' ',2X,1X,
     *       4('-'),3X,
     *       8('-'),3X,
     *       6('-'),3X,
     *       8('-'),3X,
     *       4('-'),3X,
     *       8('-'),3X,
     *       13('-'),3X,
     *       7('-'),3X,
     *       7('-'),3X,
     *       9('-'),3X,
     *       11('-'))
310   FORMAT (
     *   ' ',I2,1X,
     *   A4,3X,
     *   A8,3X,
     *   I6,3X,
     *   A8,3X,
     *   I4,3X,
     *   I8,3X,
     *   A13,3X,
     *   I7,3X,
     *   A7,3X,
     *   A9,3X,
     *   I11)
C
      END
