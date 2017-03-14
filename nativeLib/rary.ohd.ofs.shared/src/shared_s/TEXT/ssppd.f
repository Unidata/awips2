C MODULE SSPPD
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT STATUS OF PREPROCESSOR DATA BASE.
C
      SUBROUTINE SSPPD (LARRAY,ARRAY,LEVEL,IFLCHK,ISTAT)
C
      CHARACTER*4 DLYTYP
      CHARACTER*5 CPCTEN,CPCTPT,CPCTDA
      CHARACTER*7 CPCENT
      CHARACTER*8 XSTAID
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION IBUF(16)
      DIMENSION IFDATS(32)
      PARAMETER (LSIBUF=128)
      INTEGER*2 ISIBUF(LSIBUF)
C
      INCLUDE 'uiox'
      INCLUDE 'udatas'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suoptx'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdrrsc'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdhshi'
      INCLUDE 'pdbcommon/pdhshc'
      INCLUDE 'pdbcommon/pdddfc'
      COMMON /FCTIME/ IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD,NOW(5),
     *   LOCALF,NOUTZ,NOUTDS,NLSTZF,IDA,IHR,LDA,LHR,IDADAT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/ssppd.f,v $
     . $',                                                             '
     .$Id: ssppd.f,v 1.5 2002/02/11 21:04:34 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SSPPD'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('STAT')
C
      ISTAT=0
C
      NUMWRN=0
      LFDATS=32
C
C  PRINT HEADER
      IF (ISLEFT(5).GT.0) CALL SUPAGE
      WRITE (LP,130)
      CALL SULINE (LP,2)
      WRITE (LP,160)
      CALL SULINE (LP,0)
      IF (IOPOVP.EQ.1) THEN
         WRITE (LP,160)
         CALL SULINE (LP,0)
         WRITE (LP,160)
         CALL SULINE (LP,0)
         ENDIF
C
C  OPEN PREPROCESSOR DATA BASE
      CALL SUDOPN (1,'PPD ',IERR)
C
C  READ DEFAULTS TO GET TIMING PARAMETERS
      CALL SUDOPN (1,'UPRM',IERR2)
C
      IF (IERR.EQ.0.AND.IERR2.EQ.0) GO TO 10
         ISTAT=1
         GO TO 110
C
10    LOCALF=LOCAL
      NLSTZF=NLSTZ
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT INDEX FILE STATUS
C
      IF (LEVEL.GT.0) THEN
         IF (ISLEFT(5).GT.0) CALL SUPAGE
         WRITE (LP,180) 'INDEX FILE'
         CALL SULINE (LP,2)
         WRITE (LP,170) IPUSER
         CALL SULINE (LP,2)
         WRITE (LP,190) IPTTYP
         CALL SULINE (LP,2)
         WRITE (LP,200) IH8CHR
         CALL SULINE (LP,2)
         WRITE (LP,210) IHINRC
         CALL SULINE (LP,2)
         IOVFL=NHASHR*.87225
         WRITE (LP,220) NHASHR,IOVFL
         CALL SULINE (LP,2)
         NDEFC=0
         NDELC=0
         NDEFI=0
         NDELI=0
         LSTPOS=IOVFL-1
         DO 20 I=1,LSTPOS
            IF (IPDHSC(I).GT.0) NDEFC=NDEFC+1
            IF (IPDHSC(I).EQ.-1) NDELC=NDELC+1
            IF (IPDHSI(I).GT.0) NDEFI=NDEFI+1
            IF (IPDHSI(I).EQ.-1) NDELI=NDELI+1
20          CONTINUE
         WRITE (LP,240) NDEFC,NDELC
         CALL SULINE (LP,2)
         WRITE (LP,250) NDEFI,NDELI
         CALL SULINE (LP,2)
         NDEFC=0
         NDELC=0
         NDEFI=0
         NDELI=0
         DO 30 I=IOVFL,NHASHR
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,260) IOVFL,NHASHR,I,IPDHSC(I),IPDHSI(I)
               CALL SULINE (IOSDBG,1)
               ENDIF
            IF (IPDHSC(I).GT.0) NDEFC=NDEFC+1
            IF (IPDHSC(I).EQ.-1) NDELC=NDELC+1
            IF (IPDHSI(I).GT.0) NDEFI=NDEFI+1
            IF (IPDHSI(I).EQ.-1) NDELI=NDELI+1
30          CONTINUE
         WRITE (LP,270) NDEFC,NDELC
         CALL SULINE (LP,2)
         WRITE (LP,280) NDEFI,NDELI
         CALL SULINE (LP,2)
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT SIF RECORD STATUS
C
      NUMSIF=LSTSIF-INFREC
      MAXSIF=MXSIFR-INFREC
      NFREE=MAXSIF-NUMSIF
      IPCTDX=0
      IF (NUMSIF.GT.0) THEN
         IPCTDX=(FLOAT(NUMSIF)/FLOAT(MAXSIF))*100.+.5
         ENDIF
C
      IF (LEVEL.GT.0) THEN
         IF (ISLEFT(5).GT.0) CALL SUPAGE
         WRITE (LP,180) 'STATION INFORMATION FILE RECORD'
         CALL SULINE (LP,2)
         WRITE (LP,290) INFREC,MXSIFR,LSTSIF
         CALL SULINE (LP,2)
         WRITE (LP,300) MAXSIF,NUMSIF,NFREE,IPCTDX
         CALL SULINE (LP,2)
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT DAILY DATA FILE STATUS
C
      IF (ISLEFT(5).GT.0) CALL SUPAGE
      WRITE (LP,180) 'DATA FILE'
      CALL SULINE (LP,2)
      WRITE (LP,310) MAXDDF,NUMDDF
      CALL SULINE (LP,2)
C
      IF (NUMDDF.EQ.0) THEN
         WRITE (LP,150)
         CALL SUWRNS (LP,2,NUMWRN)
         GO TO 110
         ENDIF
C
C  CALCULATE PERCENTAGE OF SPACE USED ON RRS FILE
      IPCTRR=0
      IF (MXRRSF.GT.0) THEN
         IPCTRR=(FLOAT(LXRRSR)/FLOAT(MXRRSF))*100.+.5
         ENDIF
C
      IF (ISLEFT(5).GT.0) CALL SUPAGE
      NCOUNT=0
C
      WRITE (LP,320)
      CALL SULINE (LP,4)
      NCOUNT=NCOUNT+1
      CPCENT='N/A'
      CALL URGHTC (CPCENT,LEN(CPCENT),LBEGIN)
      WRITE (LP,340) NCOUNT,MXSIFR,LSTSIF,CPCENT,KPDSIF
      CALL SULINE (LP,1)
      IF (IFLCHK.NE.0) THEN
         CALL SSCHK (KPDSIF,MXSIFR,IPCTDX,IFLCHK,'PPD ',IERR)
         ENDIF
C
      NCOUNT=NCOUNT+1
      WRITE (LP,350) NCOUNT,MXRRSF,LXRRSR,IPCTRR,KPDRRS
      CALL SULINE (LP,1)
      IF (IFLCHK.NE.0) THEN
         CALL SSCHK (KPDRRS,MXRRSF,IPCTRR,IFLCHK,'PPD ',IERR)
         ENDIF
C
      IF (ISLEFT(5).GT.0) CALL SUPAGE
      DO 40 I=1,NUMDDF
         MAXREC=IPDDFC(1,I)
         LSTREC=IPDDFC(2,I)
         IPCTFL=0
         IF (MAXREC.GT.0) THEN
            IPCTFL=(FLOAT(LSTREC)/FLOAT(MAXREC))*100.+.5
            ENDIF
         IF (LUFREE.NE.I) THEN
            NCOUNT=NCOUNT+1
            WRITE (LP,360) NCOUNT,MAXREC,LSTREC,CPCENT,KPDDDF(I),I
            CALL SULINE (LP,1)
            ELSE
               NCOUNT=NCOUNT+1
               WRITE (LP,370) NCOUNT,MAXREC,LSTREC,CPCENT,KPDDDF(I),I
               CALL SULINE (LP,1)
            ENDIF
         IF (IFLCHK.NE.0) THEN
CCC         CALL SSCHK (KPDDDF(I),IPDDFC(1,I),IPCTFL,IFLCHK,'PPD ',IERR)
            ENDIF
40       CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DAILY DATA TYPE INFORMATION
C
      IF (ISLEFT(5).GT.0) CALL SUPAGE
      WRITE (LP,180) 'DAILY DATA TYPE'
      CALL SULINE (LP,2)
      WRITE (LP,380) MXDTYP,NMDTYP
      CALL SULINE (LP,2)
      WRITE (LP,390) TIME(3)
      CALL SULINE (LP,2)
C
      IF (LDEBUG.GT.0) THEN
         DO 50 I=1,NMDTYP
            WRITE (IOSDBG,410) I,(IDDTDR(J,I),J=1,24)
            CALL SULINE (IOSDBG,1)
50          CONTINUE
         ENDIF
C
      IF (ISLEFT(5).GT.0) CALL SUPAGE
      WRITE (LP,400)
      CALL SULINE (LP,4)
      NCOUNT=0
      MCOUNT=5
      MINPCT=95
C
C  PROCESS EACH DATA TYPE
      DO 70 I=1,NMDTYP
C     CHECK IF WRITE ONLY TYPE
         IF (IDDTDR(4,I).LT.0) GO TO 70
            CALL SUBSTR (IDDTDR(2,I),1,4,DLYTYP,1)
            CALL SUBSTR (IDDTDR(8,I),1,4,IEDAY,1)
            CALL SUBSTR (IDDTDR(11,I),1,4,ILDAY,1)
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,420) TIME(3),IEDAY,ILDAY
               CALL SULINE (IOSDBG,1)
               ENDIF
            CALL MDYH2 (IEDAY,0,IEMO,IEDA,IEYR,IEHR,ITZ,IDSAV,TIME(3))
            IEYR=MOD(IEYR,100)
            CALL MDYH2 (ILDAY,0,ILMO,ILDA,ILYR,ILHR,ITZ,IDSAV,TIME(3))
            ILYR=MOD(ILYR,100)
C        SET MAXIMUM AND NUMBER OF ENTRIES USED
            MXENTR=IDDTDR(16,I)
            IF (MXENTR.GT.0.AND.
     *          (DLYTYP.EQ.'PPST'.OR.
     *           DLYTYP.EQ.'PG24'.OR.
     *           DLYTYP.EQ.'APIG'))
     *         MXENTR=MXENTR-1
            NMENTR=IDDTDR(17,I)
C        SET MAXIMUM AND NUMBER OF POINTER WORDS USED
            NDELPT=0
            NDELDT=0
            IF (DLYTYP.NE.'MDR6'.AND.
     *          DLYTYP.NE.'APIG'.AND.
     *          DLYTYP.NE.'PG24'.AND.
     *          DLYTYP.NE.'PPSR') THEN
                IF (NMENTR.GT.0) THEN
C              CHECK FOR DELETED POINTER SLOTS
                  NFLDVL=0
                  CALL SMPPDP (LARRAY*2,ARRAY,NFLDVL,DLYTYP,
     *               NDELPT,NDELDT,IERR)
                  ENDIF
               ENDIF
            MXPNTR=IDDTDR(5,I)*IDDTDR(16,I)
            IF (DLYTYP.EQ.'PPSR') MXPNTR=IDDTDR(5,I)
            NMPNTR=IDDTDR(18,I)
            NMPNTR=NMPNTR-NDELPT
C        SET MAXIMUM AND NUMBER OF DATA WORDS USED            
            MXDATA=IDDTDR(21,I)*LRCPDD*2
            NMDATA=IDDTDR(19,I)
            NMDATA=NMDATA-NDELDT
C        CHECK IF PAGE HEADER NEEDS TO BE PRINTED
            IF (ISNWPG(LP).EQ.1.OR.NCOUNT.LT.MCOUNT) GO TO 60
               IF (ISLEFT(2).EQ.1) THEN
                  CALL SUPAGE
                  NCOUNT=0
                  GO TO 60
                  ENDIF
               WRITE (LP,140)
               CALL SULINE (LP,1)
               NCOUNT=0
60          IF (ISNWPG(LP).EQ.1) THEN
               WRITE (LP,400)
               CALL SULINE (LP,4)
               NCOUNT=0
               ENDIF
C        CALCULATE PERCENT USED
            CPCTEN='N/A'
            CPCTPT='N/A'
            CPCTDA='N/A'
            IF (DLYTYP.NE.'MDR6'.AND.
     *          DLYTYP.NE.'APIG'.AND.
     *          DLYTYP.NE.'PG24') THEN
C           COMPUTE PERCENT OF ENTRIES USED     
               IF (NMENTR.GT.0) THEN
                  IPCTEN=(FLOAT(NMENTR)/FLOAT(MXENTR))*100.+.5
                  ELSE
                     IPCTEN=0
                  ENDIF
               IBEG=1
               NCHAR=LEN(CPCTEN)
               IPRERR=1
               CALL UFI2A (IPCTEN,CPCTEN,IBEG,NCHAR,IPRERR,LP,IERR)
C           COMPUTE PERCENT OF POINTERS WORDS USED
               IF (DLYTYP.NE.'PPSR'.AND.NMPNTR.GT.0) THEN
                  IPCTPT=(FLOAT(NMPNTR)/FLOAT(MXPNTR))*100.+.5
                  ELSE
                     IPCTPT=0
                  ENDIF
               IBEG=1
               NCHAR=LEN(CPCTPT)
               IPRERR=1
               CALL UFI2A (IPCTPT,CPCTPT,IBEG,NCHAR,IPRERR,LP,IERR)
C           COMPUTE PERCENT OF DATA WORDS USED
               IF (NMDATA.GT.0) THEN
                  IPCTDA=(FLOAT(NMDATA)/FLOAT(MXDATA))*100.+.5
                  ELSE
                     IPCTDA=0
                  ENDIF
               IBEG=1
               NCHAR=LEN(CPCTDA)
               IPRERR=1
               CALL UFI2A (IPCTDA,CPCTDA,IBEG,NCHAR,IPRERR,LP,IERR)
               ENDIF
            CALL URGHTC (CPCTEN,LEN(CPCTEN),LBEGIN)
            CALL URGHTC (CPCTPT,LEN(CPCTPT),LBEGIN)
            CALL URGHTC (CPCTDA,LEN(CPCTDA),LBEGIN)
C        PRINT DATA TYPE INFORMATION
            WRITE (LP,430) I,DLYTYP,IDDTDR(7,I),
     *         IEMO,IEDA,IEYR,IEHR,
     *         ILMO,ILDA,ILYR,ILHR,
     *         MXENTR,NMENTR,CPCTEN,
     *         MXPNTR,NMPNTR,CPCTPT,
     *         MXDATA,NMDATA,CPCTDA,
     *         IDDTDR(4,I)
            CALL SULINE (LP,1)
            NCOUNT=NCOUNT+1
C        CHECK IF MAXIMUM VALUES EXCEEDED
            IF (NMENTR.GT.0) THEN
               IF (IPCTEN.GT.MINPCT) THEN
                  WRITE (LP,440) 'ENTRIES',IPCTEN,MINPCT,DLYTYP
                  CALL SUWRNS (LP,2,NUMWRN)
                  ENDIF
               IF (DLYTYP.NE.'PPSR'.AND.NMPNTR.GT.0) THEN
                  IF (IPCTPT.GT.MINPCT) THEN
                     WRITE (LP,440) 'POINTERS',IPCTPT,MINPCT,DLYTYP
                     CALL SUWRNS (LP,2,NUMWRN)
                     ENDIF
                  ENDIF
               IF (NMDATA.GT.0) THEN
                  IF (IPCTDA.GT.MINPCT) THEN
                     WRITE (LP,440) 'DATA WORDS',IPCTDA,MINPCT,DLYTYP
                     CALL SUWRNS (LP,2,NUMWRN)
                     ENDIF
                  ENDIF
               ENDIF
70       CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT MAXIMUM ENTRIES AND ENTRIES DEFINED
      MAXSTA=NHASHR/2
      IPCTST=0
      IF (MAXSTA.GT.0) THEN
         IPCTST=(FLOAT(NPDSTA)/FLOAT(MAXSTA))*100.+.5
         ENDIF
      WRITE (LP,450) MAXSTA,NPDSTA,IPCTST
      CALL SULINE (LP,2)
C
      WRITE (LP,460) MXDDOD
      CALL SULINE (LP,2)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK DAILY DATA POINTERS FOR DELETED SLOTS
C
      CALL SULINE (LP,2)
      WRITE (LP,180) 'DAILY DATA TYPE POINTER RECORD'
      NFLDVL=0
      DLYTYP=' '
      CALL SMPPDP (LARRAY*2,ARRAY,NFLDVL,DLYTYP,NDELPT,NDELDT,IERR)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT STATUS OF FUTURE TEMPERATURE DATA
C
      IF (ISLEFT(5).GT.0) CALL SUPAGE
      CALL SULINE (LP,2)
      WRITE (LP,180) 'FUTURE TEMPERATURE DATA'
C
C  CHECK IF DATA TYPE DEFINED
      IDX=IPDCKD('TF24')
      IF (IDX.EQ.0) THEN
         CALL SULINE (LP,2)
         WRITE (LP,470) 'TF24'
         GO TO 90
         ENDIF
C
C  READ SPECIAL DATA RECORD WITH DATES AND RECORD NUMBERS
      CALL PDGFUD (IFDATS,LFDATS,IDX,IFDREC,IERR)
      IF (IERR.GT.0) GO TO 90
C
C  CHECK IF ANY DATES
      NDATES=IFDATS(1)
      IF (NDATES.EQ.0) THEN
         CALL SULINE (LP,2)
         WRITE (LP,480) 'TF24'
         GO TO 90
         ENDIF
C
C  PRINT STATUS INFORMATION
      IF (ISLEFT(7+NDATES).GT.0) CALL SUPAGE
      CALL SULINE (LP,2)
      WRITE (LP,490) IFDREC
      CALL SULINE (LP,2)
      WRITE (LP,500) IFDATS(1)
      CALL SULINE (LP,3)
      WRITE (LP,510)
      NLAST=IFDATS(1)*2+1
      NSLOT=0
      DO 80 ISLOT=2,NLAST,2
         IF (IFDATS(ISLOT).EQ.0) GO TO 80
            NSLOT=NSLOT+1
            CALL MDYH2 (IFDATS(ISLOT),0,IMO,IDA,IYR,IHR,ITZ,IDSAV,
     *         TIME(3))
            IYR=MOD(IYR,100)
            CALL SULINE (LP,1)
            WRITE (LP,520) NSLOT,IMO,IDA,IYR,IHR,IFDATS(ISLOT+1)
80       CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT STATUS OF RRS DATA RECORDS
C
90    WRITE (LP,180) 'RRS DATA RECORD'
      CALL SULINE (LP,2)
C
C  PRINT STATUS OF PRIMARY DATA RECORDS
      CALL SULINE (LP,2)
      WRITE (LP,540) 'PRIMARY ',MXRRSF,LXRRSR,IPCTRR
C
C  CALCULATE PERCENTAGE OF RRS FREEPOOL SPACE USED
      NUMUSD=0
      NUNIT=KPDDDF(LUFREE)
      DO 100 IREC=IFREE1,MXFRER
         CALL UREADT (NUNIT,IREC,IBUF,IERR)
         IF (IERR.NE.0) THEN
            WRITE (LP,230) IREC,LUFREE
            CALL SUERRS (LP,2,-1)
            GO TO 100
            ENDIF
         IF (IBUF(1).GT.-1) NUMUSD=NUMUSD+1
100      CONTINUE
      NFREMX=MXFRER-IFREE1+1
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,530) MXFRER,IFREEN,IFREE1,NFREMX,NUMUSD
         CALL SULINE (IOSDBG,1)
         ENDIF
      IPCTFP=0
      IF (NFREMX.GT.0) THEN
         IPCTFP=(FLOAT(NUMUSD)/FLOAT(NFREMX))*100.+.5
         ENDIF
C
C  PRINT STATUS OF FREEPOOL DATA RECORDS
      WRITE (LP,540) 'FREEPOOL',NFREMX,NUMUSD,IPCTFP
      CALL SULINE (LP,2)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK LEVEL OPTION
C
      IF (LEVEL.GT.1) THEN
         XSTAID=' '
         CALL SSPPD2 (LARRAY,ARRAY,XSTAID,LSIBUF,ISIBUF,IERR)
         IF (IERR.GT.0) ISTAT=1
         ENDIF
C
      IF (LEVEL.GT.2) THEN
         CALL SSPPD3 (LARRAY,ARRAY,IERR)
         IF (IERR.GT.0) ISTAT=1
         ENDIF
C
110   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SSPPD : ISTAT=',ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
130   FORMAT ('0')
140   FORMAT (' ')
150   FORMAT ('0*** WARNING - NO DAILY DATA FILES ARE SPECIFIED.')
160   FORMAT ('+*--> PREPROCESSOR DATA BASE STATUS ')
170   FORMAT ('0',2X,'USER NAME = ',2A4)
180   FORMAT ('0- ',A,' STATUS -')
190   FORMAT ('0',2X,'FIRST RECORD OF DATA DIRECTORY = ',I3)
200   FORMAT ('0',2X,'FIRST RECORD OF CHARACTER INDEX = ',I3)
210   FORMAT ('0',2X,'FIRST RECORD OF INTEGER   INDEX = ',I3)
220   FORMAT ('0',2X,'MAXIMUM ENTRIES IN CHARACTER AND INTEGER ',
     *   'INDEX = ',I5,5X,
     *   'FIRST INDEX OVERFLOW LOCATION = ',I4)
230   FORMAT ('0*** ERROR - IN SSPPD - DAIO ERROR AT RECORD ',I5,
     *    ' OF UNIT ',I2,'.')
240   FORMAT ('0',2X,'CHARACTER PRIMARY  AREA:',3X,
     *   'ENTRIES DEFINED = ',I5,5X,
     *   'ENTRIES DELETED = ',I5)
250   FORMAT ('0',2X,'INTEGER   PRIMARY  AREA:',3X,
     *   'ENTRIES DEFINED = ',I5,5X,
     *   'ENTRIES DELETED = ',I5)
260   FORMAT (' IOVFL=',I5,3X,'NHASHR=',I5,3X,'I=',I5,3X,
     *   'IPDHSC(I)=',I5,3X,'IPDHSI(I)=',I5)
270   FORMAT ('0',2X,'CHARACTER OVERFLOW AREA:',3X,
     *   'ENTRIES DEFINED = ',I5,5X,
     *   'ENTRIES DELETED = ',I5)
280   FORMAT ('0',2X,'INTEGER   OVERFLOW AREA:',3X,
     *   'ENTRIES DEFINED = ',I5,5X,
     *   'ENTRIES DELETED = ',I5)
290   FORMAT ('0',2X,'RECORD NUMBERS:  FIRST = ',I5,3X,
     *   'LAST=',I5,3X,'LAST USED=',I5)
300   FORMAT ('0',2X,'RECORDS:  MAXIMUM = ',I5,3X,
     *   'USED = ',I5,3X,'UNUSED = ',I5,3X,
     *   'PERCENT USED = ',I3)
310   FORMAT ('0',2X,'MAXIMUM NUMBER OF DAILY DATA FILES = ',I2,5X,
     *   'NUMBER OF DAILY DATA FILES USED = ',I2)
320   FORMAT (
     *  '0',2(' '),1X,
     *      34(' '),3X,
     *      'MAXIMUM',3X,
     *      'LAST USED',3X,
     *      'PERCENT',3X,
     *      'UNIT  ',3X,
     *      'FILE    ' /
     *   ' ',2(' '),1X,
     *      'CONTENTS',26(' '),3X,
     *      'RECORDS',3X,
     *      'RECORD   ',3X,
     *      'USED   ',3X,
     *      'NUMBER',3X,
     *      'NAME    ' /
     *   ' ',2(' '),1X,
     *      34('-'),3X,
     *      7('-'),3X,
     *      9('-'),3X,
     *      7('-'),3X,
     *      6('-'),3X,
     *      8('-'))
340   FORMAT (
     *   ' ',I2,1X,
     *   'INDEX',29(' '),3X,
     *   I7,3X,
     *   I9,3X,
     *   A7,3X,
     *   I6,3X,
     *   'PDBINDEX',3X)
350   FORMAT (
     *   ' ',I2,1X,
     *   'RRS DATA (PRIMARY)',16(' '),3X,
     *   I7,3X,
     *   I9,3X,
     *   I7,3X,
     *   I6,3X,
     *   'PDBRRS  ',3X)
360   FORMAT (
     *   ' ',I2,1X,
     *   'DAILY DATA',24(' '),3X,
     *   I7,3X,
     *   I9,3X,
     *   A7,3X,
     *   I6,3X,
     *   'PDBDLY',I1,1(' '),3X)
370   FORMAT (
     *   ' ',I2,1X,
     *   'DAILY DATA AND RRS DATA (FREEPOOL)',3X,
     *   I7,3X,
     *   I9,3X,
     *   A7,3X,
     *   I6,3X,
     *   'PDBDLY',I1,1(' '),3X)
380   FORMAT ('0',2X,'MAXIMUM DATA TYPES = ',I3,5X,
     *   'DATA TYPES DEFINED = ',I2)
390   FORMAT ('0',2X,'TIME ZONE CODE = ',A4)
400   FORMAT (
     *  '0',2(' '),1X,
     *      'DATA',3X,
     *      'MAXIMUM',3X,
     *      'FIRST DAY  ',3X,
     *      'LAST DAY   ',3X,
     *      '-----ENTRIES-----',3X,
     *      '--POINTER WORDS--',3X,
     *      '---DATA WORDS----',3X,
     *      'FILE    ' /
     *   ' ',2(' '),1X,
     *      'TYPE',3X,
     *      'DAYS   ',3X,
     *      'OF DATA    ',3X,
     *      'OF DATA    ',3X,
     *      'MAX  ',1X,'USED ',1X,'%USED',3X,
     *      'MAX  ',1X,'USED ',1X,'%USED',3X,
     *      'MAX  ',1X,'USED ',1X,'%USED',3X,
     *      'NAME    ' /
     *   ' ',2(' '),1X,
     *       4('-'),3X,
     *       7('-'),3X,
     *       11('-'),3X,
     *       11('-'),3X,
     *       5('-'),1X,5('-'),1X,5('-'),3X,
     *       5('-'),1X,5('-'),1X,5('-'),3X,
     *       5('-'),1X,5('-'),1X,5('-'),3X,
     *       8('-'))
410   FORMAT (' IDDTDR(',I1,')=',I5,1X,2(A2,4X),7(I5,1X),
     *   2(/T11,10(I5,1X)))
420   FORMAT (' TIME(3)=',A4,3X,'IEDAY=',I6,3X,'ILDAY=',I6)
430   FORMAT (' ',
     *   I2,1X,
     *   A4,3X,
     *   I7,3X,
     *   I2.2,'/',I2.2,'/',I2.2,'/',I2.2,3X,
     *   I2.2,'/',I2.2,'/',I2.2,'/',I2.2,3X,
     *   I5,1X,I5,1X,A,3X,
     *   I5,1X,I5,1X,A,3X,
     *   I5,1X,I5,1X,A,3X,
     *   'PDBDLY',I1,1X)
440   FORMAT ('0*** WARNING - PERCENT OF ',A,' USED (',I3,
     *   ') EXCEEDS ',I3,' PERCENT FOR DATA TYPE ',A,'.')
450   FORMAT ('0',2X,'MAXIMUM STATIONS = ',I4,5X,
     *   'STATIONS DEFINED = ',I4,5X,
     *   'PERCENT USED = ',I3)
460   FORMAT ('0',2X,'MAXIMUM DAYS BETWEEN LAST OBSERVED DATA ',
     *   'DAY AND FIRST DAY OF FUTURE DATA = ',I3)
470   FORMAT ('0*** NOTE - DAILY DATA TYPE ',A4,' NOT DEFINED.')
480   FORMAT ('0*** NOTE - NO DATES FOUND FOR DAILY DATA TYPE ',A4,'.')
490   FORMAT ('0',2X,'RECORD NUMBER OF DATES RECORD = ',I4)
500   FORMAT ('0',2X,'NUMBER OF DATES OF DATA = ',I2)
510   FORMAT ('0',3X,2X,1X,'DATE',7X,3X,'RECORD #' /
     *   ' ',3X,2X,1X,11('-'),3X,8('-'))
520   FORMAT (' ',3X,I2,1X,I2.2,'/',I2.2,'/',I2.2,'/',I2.2,3X,I8)
530   FORMAT (' MXFRER=',I4,3X,'IFREEN=',I4,3X,'IFREE1=',I4,3X,
     *   'NFREMX=',I5,3X,'NUMUSD=',I5)
540   FORMAT ('0',2X,A,' RECORDS:  MAXIMUM = ',I5,5X,
     *   'USED = ',I5,5X,
     *   'PERCENT USED = ',I3)
C
      END
