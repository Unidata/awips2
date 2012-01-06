C MEMBER SMPRDI
C----------------------------------------------------------------------
C
C                             LAST UPDATE: 08/18/94.10:33:07 BY $WC20SV
C
C @PROCESS LVL(77)
C
C DESC ROUTINE TO READ PROCESSED DATA BASE INDEX ENTRIES.
C DESC PPINIT COMMAND :  @DUMP PDBINDX
C
      SUBROUTINE SMPRDI (LARRAY,IARRAY,NFLD,ISTAT)
C
      REAL TYPCK(2)/4HPDBI,4HNDX /
      REAL*8 DMPOPT,DPCHK
      REAL*8 XSORT/8HSORT    /
      REAL*8 XNSORT/8HNOSORT  /
C
      DIMENSION CHAR(5),CHK(5)
      DIMENSION IARRAY(5,1)
      DIMENSION TYPE(2)
      DIMENSION IBUF(4)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'prdcommon/pmaxdm'
      INCLUDE 'prdcommon/punits'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_dump/RCS/smprdi.f,v $
     . $',                                                             '
     .$Id: smprdi.f,v 1.1 1995/09/17 19:13:06 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,160)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DUMP')
C
      ISTAT=0
C
      ISTRT=-1
      LCHAR=5
      LCHK=5
      LDPCHK=2
      ILPFND=0
      IRPFND=0
      NUMFLD=0
      NUMERR=0
      NUMID=0
      ISORT=1
      DMPOPT=XSORT
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FIELDS FOR TYPE
C
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *   CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *      CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
      IF (IERR.NE.1) GO TO 20
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,170) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 10
C
C  CHECK FOR END OF INPUT
20    IF (NFLD.EQ.-1) GO TO 100
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) GO TO 100
C
C  CHECK FOR PAIRED PARENTHESIS
      IF (ILPFND.GT.0.AND.IRPFND.EQ.0) GO TO 30
         GO TO 40
30    IF (NFLD.EQ.1) CALL SUPCRD
      WRITE (LP,180) NFLD
      CALL SULINE (LP,2)
      ILPFND=0
      IRPFND=0
40    IF (LLPAR.GT.0) ILPFND=1
      IF (LRPAR.GT.0) IRPFND=1
C
C  CHECK FOR PARENTHESIS IN FIELD
      IF (LLPAR.GT.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LLPAR-1,IERR)
      IF (LLPAR.EQ.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LENGTH,IERR)
C
      NUMFLD=NUMFLD+1
      IF (NUMFLD.GT.1) GO TO 90
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK OPTION
      CALL SUBSTR (CHK,1,8,TYPE,1)
      IF (TYPE(1).EQ.TYPCK(1).AND.TYPE(2).EQ.TYPCK(2)) GO TO 50
         WRITE (LP,190) CHK(1)
         CALL SUERRS (LP,2,NUMERR)
         GO TO 10
50    IF (NFLD.EQ.1) CALL SUPCRD
C
C  CHECK DUMP TYPE
      IF (LLPAR.EQ.0) GO TO 80
      IF (LRPAR.GT.0) GO TO 60
         WRITE (LP,180) NFLD
         CALL SULINE (LP,2)
         LRPAR=LENGTH+1
60    CALL UFPACK (LDPCHK,DPCHK,ISTRT,LLPAR+1,LRPAR-1,IERR)
      IF (DPCHK.EQ.XSORT.OR.DPCHK.EQ.XNSORT) GO TO 70
         WRITE (LP,210) NFLD,DPCHK
         CALL SUERRS (LP,2,NUMERR)
         NUMERR=NUMERR+1
         GO TO 80
70    DMPOPT=DPCHK
      IF (LDEBUG.GT.0) THEN
         WRITE (LP,220) DMPOPT
         CALL SULINE (LP,1)
         ENDIF
80    WRITE (LP,230) DMPOPT
      CALL SULINE (LP,2)
      IF (DMPOPT.EQ.XSORT) ISORT=1
      GO TO 10
C
C  CHECK FOR KEYWORD
90    CALL SUIDCK ('DMPG',CHK,NFLD,0,IKEYWD,IERR)
      IF (IERR.EQ.2) GO TO 100
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,200) CHK
            CALL SULINE (IOSDBG,1)
            ENDIF
C
      IF (NFLD.EQ.1) CALL SUPCRD
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  OPEN DATA BASE
100   CALL SUDOPN (1,'PRD ',IERR)
      IF (IERR.GT.0) GO TO 150
C
C  CHECK IF ANY TIME SERIES DEFINED
      IF (NUMTMS.EQ.0) THEN
         WRITE (LP,240)
         CALL SUERRS (LP,2,NUMERR)
         GO TO 150
         ENDIF
C
      NWORDS=5
      MAXID=LARRAY/NWORDS
C
C  READ INDEX ENTRIES
      NUMREC=MAXTMS*2
      DO 110 IREC=1,NUMREC
         CALL UREADT (KINDEX,IREC,IBUF,IERR)
         IF (IERR.GT.0) THEN
            WRITE (LP,250) IREC,KINDEX
            CALL SUERRS (LP,2,NUMERR)
            GO TO 110
            ENDIF
         IF (IBUF(1).EQ.0.OR.IBUF(1).EQ.-1) GO TO 110
         NUMID=NUMID+1
         IF (NUMID.GT.MAXID) THEN
            WRITE (LP,260) MAXID
            CALL SUERRS (LP,2,NUMERR)
            GO TO 150
            ENDIF
         CALL SUBSTR (IBUF,1,16,IARRAY(1,NUMID),1)
         CALL SUBSTR (IREC,1,4,IARRAY(1,NUMID),17)
110      CONTINUE
C
      IF (NUMID.EQ.0) GO TO 140
C
C  SORT LIST IF OPTION SPECIFIED
      IF (ISORT.EQ.0) GO TO 120
         ISPTR=0
         CALL SUSORT (NWORDS,NUMID,IARRAY,IARRAY,ISPTR,IERR)
C
120   IF (ISLEFT(10).GT.0) CALL SUPAGE
      WRITE (LP,270)
      CALL SULINE (LP,2)
      WRITE (LP,280)
      CALL SULINE (LP,3)
      DO 130 I=1,NUMID
         WRITE (LP,290) I,(IARRAY(J,I),J=1,NWORDS)
         CALL SULINE (LP,1)
         IF (ISNWPG(LP).EQ.1) WRITE (LP,280)
         IF (ISNWPG(LP).EQ.1) CALL SULINE (LP,3)
130      CONTINUE
C
140   WRITE (LP,300) NUMID,MAXID
      CALL SULINE (LP,2)
C
150   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,310)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
160   FORMAT (' *** ENTER SMPRDI')
170   FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
180   FORMAT ('0*** NOTE - RIGHT PARENTHESES ASSUMED IN FIELD ',
     *   I2,'.')
190   FORMAT ('0*** ERROR - IN SMPRDI - ',A4,' IS AN INVALID OPTION.')
200   FORMAT (' INPUT FIELD = ',5A4)
210   FORMAT ('0*** ERROR - CARD FIELD ',I2,' HAS AN INVALID DUMP ',
     *   'PDBINDX OPTION : ',A8)
220   FORMAT (' DUMP PDBINDX OPTION IS ',A8)
230   FORMAT ('0DUMP PDBINDX OPTION IN EFFECT = ',A8)
240   FORMAT ('0*** ERROR - NO TIME SERIES ARE DEFINED.')
250   FORMAT ('0*** ERROR - IN SMPRDI - DAIO ERROR AT RECORD ',I5,
     *    ' OF UNIT ',I2,'.')
260   FORMAT ('0*** ERROR - IN SMPRDI - MAXIMUM NUMBER OF IDENTIFIERS ',
     *   'THAT CAN BE PROCESSED (',I5,') EXCEEDED.')
270   FORMAT ('0- DUMP OF PROCESSED DATA BASE INDEX -')
280   FORMAT (1H0,5X,'TIME SERIES ID',3X,'DATA TYPE',3X,
     *   'FIRST RECORD #',3X,'INDEX RECORD #' /
     *   1H ,5X,'--------------',3X,'---------',3X,'--------------',3X,
     *   '--------------')
290   FORMAT (1X,I4,1X,2A4,9X,A4,10X,I5,15X,I5)
300   FORMAT ('0*** NOTE - ',I5,' INDEX RECORDS PROCESSED. A MAXIMUM ',
     *   'OF ',I5,' CAN BE PROCESSED.')
310   FORMAT (' *** EXIT SMPRDI')
C
      END
