C MODULE PDSTAT
C-----------------------------------------------------------------------
C
       SUBROUTINE PDSTAT
C
C  ROUTINE TO PRINT PREPROCESSOR DATA BASE STATUS INFORMATION.
C
      CHARACTER*4 DLYTYP
      CHARACTER*3 CNA
C
      DIMENSION IFPBUF(16)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdrrsc'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdddfc'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil/RCS/pdstat.f,v $
     . $',                                                             '
     .$Id: pdstat.f,v 1.3 2002/02/11 20:53:54 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,*) 'ENTER PDSTAT'
C
      IF (IPDDB.GT.0) THEN
         WRITE (LP,60) NWDCTL,LRCPDD,LRCPDR,LRCPDI,MXDTYP,NMDTYP,
     *     IPTTYP,NHASHR,IH8CHR,IHINRC,INFREC,MXSIFR,LSTSIF,MXDDOD,
     *     MAXDDF,NUMDDF
         ENDIF
C
C  PRINT DAILY FILE STATUS
      CALL ULINE (LP,2)
      WRITE (LP,70) NUMDDF,MAXDDF
C
      CNA='N/A'
      IPCTI=(FLOAT(LSTSIF)/FLOAT(MXSIFR))*100.+.5
      IPCTR=(FLOAT(LXRRSR)/FLOAT(MXRRSF))*100.+.5
      CALL ULINE (LP,2)
      WRITE (LP,80)
      CALL ULINE (LP,2)
CCC      WRITE (LP,90) IPCTI,IPCTR
      WRITE (LP,90) CNA,IPCTR
C
C  CALCULATE PERCENTAGE OF SPACE USED ON DAILY FILES
      CNA='N/A'
      DO 10 I=1,NUMDDF
         IF (LUFREE.EQ.I) THEN
            MAXREC=IPDDFC(1,LUFREE)
            LSTREC=IPDDFC(2,LUFREE)
            IPCT=(FLOAT(LSTREC)/FLOAT(MAXREC))*100.+.5
            CALL ULINE (LP,1)
CCC            WRITE (LP,110) I,IPCT
            WRITE (LP,110) I,CNA
            GO TO 10
            ENDIF
         MAXREC=IPDDFC(1,I)
         LSTREC=IPDDFC(2,I)
         IPCT=(FLOAT(LSTREC)/FLOAT(MAXREC))*100.+.5
         CALL ULINE (LP,1)
CCC         WRITE (LP,100) I,IPCT
         WRITE (LP,100) I,CNA
10       CONTINUE
C
C  COUNT RRS FREEPOOL RECORDS USED
      NUMFPR=0
      DO 20 IREC=IFREE1,MXFRER
C     READ RRS FREEPOOL RECORD
         CALL UREADT (KPDDDF(LUFREE),IREC,IFPBUF,IERR)
         IF (IERR.NE.0) THEN
            WRITE (LP,170) IERR
            GO TO 40
            ENDIF
C     CHECK IF RECORD USED
         IF (IFPBUF(1).NE.-1) NUMFPR=NUMFPR+1
20       CONTINUE
C
C  CALCULATE PERCENTAGE OF RRS FREEPOOL SPACE USED
      MAXFPR=MXFRER-IFREE1+1
      IPCT=(FLOAT(NUMFPR)/FLOAT(MAXFPR))*100.+.5
      CALL ULINE (LP,2)
      WRITE (LP,180) IPCTR,IPCT
C
C  PRINT DAILY DATA TYPES
      CALL ULINE (LP,2)
      WRITE (LP,120) NMDTYP,MXDTYP
      CALL ULINE (LP,4)
      WRITE (LP,130)
C
C  PRINT DAILY TYPES
      DO 30 I=1,NMDTYP
         IF (IDDTDR(4,I).LT.0) GO TO 30
C     CONVERT DATES FOR REPORT
         CALL UMEMOV (IDDTDR(2,I),DLYTYP,1)
         CALL UMEMOV (IDDTDR(8,I),IEDAY,1)
         CALL UMEMOV (IDDTDR(11,I),ILDAY,1)
         CALL MDYH2 (IEDAY,0,IEMO,IEDA,IEYR,IEHR,ITZ,IDSAV,TIME(3))
         IEYR=MOD(IEYR,100)
         CALL MDYH2 (ILDAY,0,ILMO,ILDA,ILYR,ILHR,ITZ,IDSAV,TIME(3))
         ILYR=MOD(ILYR,100)
         MXENTR=IDDTDR(16,I)
         IF (DLYTYP.EQ.'PPSR'.OR.DLYTYP.EQ.'PPST'.OR.
     *       DLYTYP.EQ.'PG24'.OR.DLYTYP.EQ.'APIG') MXENTR=MXENTR-1
         CALL ULINE (LP,1)
         WRITE (LP,140) DLYTYP,IDDTDR(7,I),IEMO,IEDA,IEYR,IEHR,ILMO,
     *      ILDA,ILYR,ILHR,MXENTR,IDDTDR(17,I),IDDTDR(4,I)
30       CONTINUE
C
      CALL ULINE (LP,2)
      WRITE (LP,150) NPDSTA
      CALL ULINE (LP,2)
      WRITE (LP,160) MXDDOD
C
      WRITE (LP,190) INUSEF
C
40    IF (IPDTR.GT.0) WRITE (IOGDB,*) 'EXIT PDSTAT'
C
      RETURN
C
C-----------------------------------------------------------------------
C
60    FORMAT (' CONTROL RECORD=',16(1X,I4))
70    FORMAT ('0DATA FILE STATUS:',3X,
     *   'NUMBER OF DAILY DATA FILES USED = ',I2,3X,
     *   'MAXIMUM DAILY DATA FILES ALLOWED = ',I2)
80    FORMAT ('0',5X,'FILE NAME',5X,'CONTENTS',26X,'PERCENT USED ' /
     *   ' ',5X,9('-'),5X,8('-'),26X,12('-'))
90    FORMAT (' ',5X,'PDBINDEX',6X,'INDEX',T59,A /
     *   ' ',5X,'PDBRRS  ',6X,'RRS PRIMARY DATA',T59,I3)
100   FORMAT (' ',5X,'PDBDLY',I1,1X,6X,'DAILY DATA',T59,A)
110   FORMAT (' ',5X,'PDBDLY',I1,1X,6X,'DAILY DATA AND ',
     *   'RRS FREEPOOL DATA',T59,A)
120   FORMAT ('0DAILY DATA TYPE STATUS:',3X,
     *   'NUMBER OF DATA TYPES DEFINED = ',I2,3X,
     *   'MAXIMUM DATA TYPES ALLOWED = ',I2)
130   FORMAT ('0',5X,'DATA',2X,'MAXIMUM',2X,
     *      'FIRST DAY  ',2X,'LAST DAY   ',2X,
     *      'MAXIMUM ',2X,'STATIONS',2X,'FILE' /
     *   ' ',5X,'TYPE',2X,'DAYS   ',2X,
     *      'OF DATA    ',2X,'OF DATA    ',2X,
     *      'STATIONS',2X,'DEFINED ',2X,'NAME' /
     *   ' ',5X,4('-'),2X,7('-'),2X,
     *      11('-'),2X,11('-'),2X,
     *      8('-'),2X,8('-'),2X,8('-'))
140   FORMAT (' ',5X,A,4X,I4,3X,
     *   I2.2,'/',I2.2,'/',I2.2,'/',I2.2,2X,
     *   I2.2,'/',I2.2,'/',I2.2,'/',I2.2,4X,
     *   I5,4X,I5,4X,'PDBDLY',I1)
150   FORMAT ('0TOTAL STATIONS DEFINED = ',I4)
160   FORMAT ('0MAXIMUM DAYS BETWEEN LAST OBSERVED DATA ',
     *   'DAY AND FIRST DAY OF FUTURE DATA = ',I2)
170   FORMAT ('***ERROR** IN PDSTAT - FILE READ ERROR. STATUS=',I2)
180   FORMAT ('0RRS DATA TYPE STATUS:',3X,
     *   'PERCENT PRIMARY SPACE USED = ',I3,3X,
     *   'PERCENT FREEPOOL SPACE USED = ',I3)
190   FORMAT ('0FILE IN-USE FLAG = ',I2)
C
      END
