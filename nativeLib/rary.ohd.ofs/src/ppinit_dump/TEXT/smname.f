C MODULE SMNAME
C-----------------------------------------------------------------------
C
C  ROUTINE TO OUTPUT IDENTIFER NAMES.
C
      SUBROUTINE SMNAME (LARRAY,ARRAY,OUTPUT,SORT,NFLD,ISTAT)
C
      CHARACTER*4 OUTPUT,SORT
      PARAMETER (MOPTN=16)
      CHARACTER*4 OPTN(MOPTN)
     *   /'ALL ','    ','STAN','PCPN','TEMP','PE  ','RRS ','BASN',
     *    'MAP ','FMAP','MAT ','MAPE','MAPX','    ','    ','    '/
      CHARACTER*8 IDENT
      CHARACTER*20 STRNG,STRNG2
C
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sworkx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_dump/RCS/smname.f,v $
     . $',                                                             '
     .$Id: smname.f,v 1.6 2002/02/11 21:03:38 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SMNAME'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DUMP')
C
      ISTAT=0
C
      LSTRNG=-LEN(STRNG)
      LSTRNG2=-LEN(STRNG2)
      ISTRT=1
C
      ILPFND=0
      IRPFND=0
      NUMERR=0
      NUMWRN=0
      NUMOPT=0
      IOPTN=0
      IALL=1
      LPUNCH=NPUCRD
C
C  SET SORT OPTION
      ISORT=-1
      IF (SORT.EQ.'NO') ISORT=0
      IF (SORT.EQ.'ID') ISORT=1
      IF (SORT.EQ.'DESC') ISORT=2
      IF (ISORT.EQ.-1) THEN
         WRITE (LP,190) 'ID'
         CALL SUWRNS (LP,2,NUMWRN)
         ISORT=1
         ENDIF
C
C  PRINT HEADER LINE
      WRITE (LP,170)
      CALL SULINE (LP,2)
      WRITE (LP,280)
      CALL SULINE (LP,1)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FIELDS FOR DUMP NAMES OPTIONS
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LSTRNG,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (NFLD.EQ.-1) GO TO 150
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *      LSTRNG,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
      IF (IERR.NE.1) GO TO 20
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,210) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 10
C
C  CHECK FOR COMMAND
20    IF (LATSGN.EQ.1) GO TO 150
C
      IF (ILPFND.GT.0.AND.IRPFND.EQ.0) GO TO 30
         GO TO 40
30    WRITE (LP,180) NFLD
      CALL SULINE (LP,2)
      ILPFND=0
      IRPFND=0
40    IF (LLPAR.GT.0) ILPFND=1
C
C  CHECK FOR PARENTHESIS IN FIELD
      IF (LLPAR.GT.0) CALL UFPACK (LSTRNG2,STRNG2,ISTRT,1,LLPAR-1,IERR)
      IF (LLPAR.EQ.0) CALL UFPACK (LSTRNG2,STRNG2,ISTRT,1,LENGTH,IERR)
C
C  CHECK FOR OPTION
      DO 60 I=1,MOPTN
         CALL SUCOMP (1,STRNG2,OPTN(I),IERR)
         IF (IERR.NE.1) GO TO 60
         IF (I.NE.1) GO TO 50
            IALL=1
            IF (NFLD.EQ.1) CALL SUPCRD
            GO TO 100
50       IOPTN=I
         IF (IOPTN.GE.3.AND.IOPTN.LE.MOPTN) IALL=0
         IF (NFLD.EQ.1) CALL SUPCRD
         GO TO 80
60    CONTINUE
C
C  CHECK FOR GROUP
      CALL SUIDCK ('DMPG',STRNG2,NFLD,0,IKEYWD,IERR)
      IF (IERR.EQ.0) GO TO 70
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,*) 'STRNG2=',STRNG2
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 150
C
70    IF (NFLD.EQ.1) CALL SUPCRD
      ILPFND=0
      IRPFND=0
C
80    GO TO (100,100,100,100,100,
     *       100,100,100,100,100,
     *       100,100,100,90,90,
     *       90),IOPTN
90       WRITE (LP,200) OPTN(IOPTN)
         CALL SUERRS (LP,2,NUMERR)
         GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  GET SORTED LIST OF IDENTIFIERS
100   IPRERR=0
      NUMID=0
C
      IF (IALL.EQ.0) GO TO 110
         IOPTN=3
C
110   NWORDS=2
      MAXID=LSWORK/2
      IF (OPTN(IOPTN).EQ.'FMAP') THEN
         CALL SUGTTS (OPTN(IOPTN),ISORT,NWORDS,MAXID,SWORK,NUMID,IERR)
         ELSE
            ISTATE=0
            IPNTRS=0
            IPRMSG=1
            CALL SURIDS (OPTN(IOPTN),ISORT,MAXID,ISTATE,NWORDS,SWORK,
     *         IPNTRS,NUMID,LARRAY,ARRAY,IPRMSG,IPRERR,IERR)
         ENDIF
C
      IF (NUMID.EQ.0) GO TO 130
C
      IF (OUTPUT.EQ.'PRNT'.OR.OUTPUT.EQ.'BOTH') THEN
         WRITE (LP,230) OPTN(IOPTN)
         CALL SULINE (LP,2)
         ENDIF
      IF (OUTPUT.EQ.'PNCH'.OR.OUTPUT.EQ.'BOTH'.AND.ICDPUN.NE.LP)
     *   WRITE (ICDPUN,240) OPTN(IOPTN)
C
C  OUTPUT LIST OF IDENTIFIERS
      DO 120 I=1,NUMID
         CALL SUBSTR (SWORK(I*2-1),1,8,IDENT,1)
         IF (OUTPUT.EQ.'PRNT'.OR.OUTPUT.EQ.'BOTH') THEN
            WRITE (LP,'(1X,A)') IDENT
            CALL SULINE (LP,1)
            ENDIF
         IF (OUTPUT.EQ.'PNCH'.OR.OUTPUT.EQ.'BOTH'.AND.ICDPUN.NE.LP) THEN
            WRITE (ICDPUN,'(A)') IDENT
            NPUCRD=NPUCRD+1
            ENDIF
120      CONTINUE
C
      WRITE (LP,270) NUMID,OPTN(IOPTN)
      CALL SULINE (LP,2)
C
130   IF (IALL.EQ.1) THEN
         IOPTN=IOPTN+1
         IF (IOPTN.GT.MOPTN) GO TO 140
         IF (OPTN(IOPTN).EQ.' ') GO TO 130
         GO TO 110
         ENDIF
C
140   NUMOPT=NUMOPT+1
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK NUMBER OF OPTIONS PROCESSED
150   IF (NUMOPT.EQ.0) THEN
         WRITE (LP,290)
         CALL SULINE (LP,2)
         ENDIF
C
      IF (OUTPUT.EQ.'PNCH'.OR.OUTPUT.EQ.'BOTH') THEN
C     PRINT NUMBER OF CARD IMAGES OUTPUT
         NPUNCH=NPUCRD-LPUNCH
         IF (NPUNCH.GT.0) THEN
            WRITE (LP,300) NPUNCH
            CALL SULINE (LP,2)
            ENDIF
         ENDIF
C
C  CHECK NUMBER OF ERRORS ENCOUNTERED
      IF (NUMERR.GT.0) THEN
         WRITE (LP,310) NUMERR
         CALL SULINE (LP,2)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SMNAME'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
170   FORMAT ('0*--> DUMP IDENTIFIERS')
280   FORMAT (' ')
180   FORMAT ('0*** NOTE - RIGHT PARENTHESIS ASSUMED IN FIELD ',I2,'.')
190   FORMAT ('0*** WARNING - INVALID SORT OPTION. ',
     *   'SORT OPTION SET TO BY ',A,'.')
200   FORMAT ('0*** ERROR - INVALID OPTION : ',A)
210   FORMAT (' BLANK STRING FOUND IN FIELD ',I2)
230   FORMAT ('0IDENTIFIERS WITH PARAMTER TYPE ',A,':')
240   FORMAT ('$ IDENTIFIERS WITH PARAMETER TYPE ',A)
270   FORMAT ('0*** NOTE - ',I5,' ',A,' IDENTIFIERS PROCESSED.')
290   FORMAT ('0*** NOTE - NO DUMP NAMES OPTIONS SUCCESSFULLY ',
     *   'PROCESSED.')
300   FORMAT ('0*** NOTE - ',I5,' CARD IMAGES OUTPUT.')
310   FORMAT ('0*** NOTE - ',I3,' ERRORS ENCOUNTERED IN DUMP NAMES ',
     *   'COMMAND.')
C
      END
