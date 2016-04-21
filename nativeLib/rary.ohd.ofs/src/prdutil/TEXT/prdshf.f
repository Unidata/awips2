C$PRAGMA C (DDRMCL)
C MODULE PRDSHF
C-----------------------------------------------------------------------
C
C ROUTINES TO CREATE SHEF MESSAGES FROM DATA IN THE PROCESSED DATA BASE.
C
      SUBROUTINE PRDSHF (JULBEG,INTHRB,JULHRB,JULEND,INTHRE,JULHRE,
     *   NFLD,INDERR,IENDIN)
C
      CHARACTER*4 TZCODE
      PARAMETER (MAXTS=1000)
      CHARACTER*4 TSTYPE(MAXTS)
      CHARACTER*5 SHEFCD(MAXTS),DATAFMT(MAXTS)
      CHARACTER*8 TSID(MAXTS),SHEFID(MAXTS)
      PARAMETER (MSHEFCM=25)
      CHARACTER*60 SHEFCM(MSHEFCM)
      DIMENSION ITIME(MAXTS)
      DIMENSION NPERCD(MAXTS)
      DIMENSION ITSCM(MAXTS)
      DIMENSION ITSERR(MAXTS)
      PARAMETER (MDTYPES=25)
      CHARACTER*4 DTYPES(MDTYPES),DUNITS(MDTYPES)
      DIMENSION CONVFS1(MDTYPES)
      CHARACTER*10 CONVOS(MDTYPES),CONVFS2(MDTYPES)
C
      CHARACTER*1 CHKSTR
      CHARACTER*3 HEADERS
      CHARACTER*8 SORTBY
      CHARACTER*9 PRODHDR
      CHARACTER*70 STRNG1,STRNG2,STRNG3
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
      INCLUDE 'ufstcd'
      INCLUDE 'hclcommon/hdflts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/prdutil/RCS/prdshf.f,v $
     . $',                                                             '
     .$Id: prdshf.f,v 1.11 2005/01/12 17:45:47 xfan Exp $
     . $' /
C    ===================================================================
C
C
      LDEBUG=0
C
      IENDIN=0
C
      IBCOL=1
      IECOL=72
      IPRERR=1
      TZCODE='Z'
C
      IHRZ23=0
      NSHEFCM=0
      CALL UREPET2 ('YES',SHEFCM,MSHEFCM)
      IDOTA=1
      IDOTE=0
      CALL UMEMST (0,NPERCD,MAXTS)
      HEADERS='YES'
      PRODHDR='CCCWRKXXX'
      SORTBY=' '
      CALL UREPET2 ('.',DATAFMT,MAXTS)
C
C  CHECK FOR MORE FIELDS ON COMMAND LINE
10    IF (NFLD.LT.NFIELD) THEN
         NFLD=NFLD+1
         NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
         MCHAR=LEN(STRNG1)
         IF (NCHAR.GT.MCHAR) NCHAR=MCHAR
         STRNG1=' '
         CALL UPACK1 (IBUF(IFSTRT(NFLD)),STRNG1,NCHAR)
         IF (STRNG1(1:1).EQ.'@') THEN
            CALL UEROR (LP,2,-1)
            WRITE (LP,20)
20    FORMAT ('0**ERROR** UNEXPECTED END OF INPUT.')
            GO TO 190
            ENDIF
         IF (STRNG1.EQ.'$'.OR.STRNG1.EQ.'&') THEN
            CALL RPCARD (IBUF,IERR)
            IF (IERR.NE.0) THEN
               WRITE (LP,20)
               IENDIN=1
               GO TO 190
               ENDIF
            IFSTCD=1
            CALL WPCARD (IBUF)
C        FIND FIELDS ON CARD
            CALL UFREE (IBCOL,IECOL)
C        CHECK IF NO FIELDS ON CARD
            IF (NFIELD.EQ.0) GO TO 80
            NFLD=0
            GO TO 10
            ENDIF
         NSCAN=1
         CALL USCAN2 (STRNG1,'=',NSCAN,STRNG2,LSTRNG2,IERR)
         IF (STRNG2.EQ.'BYDATE'.OR.
     *       STRNG2.EQ.'BYID'.OR.
     *       STRNG2.EQ.'BOTH') THEN
            SORTBY=STRNG2
            GO TO 10
            ENDIF
         IF (STRNG2.EQ.'COMMENT') THEN
            IF (NSHEFCM+1.GT.MSHEFCM) THEN
               CALL UEROR (LP,2,-1)
               WRITE (LP,30) MSHEFCM
30    FORMAT ('0**ERROR** MAXIMUM NUMBER OF COMMENTS (',I2,
     *   ') EXCEEDED.')
               INDERR=1
               GO TO 10
               ENDIF
            NSHEFCM=NSHEFCM+1
            NSCAN=2
            CALL USCAN2 (STRNG1,'=''',NSCAN,SHEFCM(NSHEFCM),LSHEFCM,
     *         IERR)
            GO TO 10
            ENDIF
         IF (STRNG2.EQ.'FORMAT') THEN
            IFOUND=0
            NSCAN=2
            CALL USCAN2 (STRNG1,'=',NSCAN,STRNG3,LSTRNG3,IERR)
            IF (STRNG3.EQ.'.A') THEN
               IDOTA=1
               IDOTE=0
               IFOUND=1
               ENDIF
            IF (STRNG3.EQ.'.E') THEN
               IDOTE=1
               IDOTA=0
               IFOUND=1
               ENDIF
            IF (IFOUND.EQ.0) THEN
               WRITE (LP,40) 'KEYWORD',STRNG3(1:LENSTR(STRNG3)),NFLD
               INDERR=1
               ENDIF
            GO TO 10
            ENDIF
         IF (STRNG2.EQ.'HEADERS') THEN
            IFOUND=0
            NSCAN=2
            CALL USCAN2 (STRNG1,'=',NSCAN,STRNG3,LSTRNG3,IERR)
            IF (STRNG3.EQ.'YES') THEN
               HEADERS=STRNG3
               IFOUND=1
               ENDIF
            IF (STRNG3.EQ.'NO') THEN
               HEADERS=STRNG3
               IFOUND=1
               ENDIF
            IF (IFOUND.EQ.0) THEN
               WRITE (LP,40) 'KEYWORD',STRNG3(1:LENSTR(STRNG3)),NFLD
               INDERR=1
               ENDIF
            GO TO 10
            ENDIF
         IF (STRNG2.EQ.'HR0TO23') THEN
            IHRZ23=1
            GO TO 10
            ENDIF
         IF (STRNG2.EQ.'HR1TO24') THEN
            IHRZ23=0
            GO TO 10
            ENDIF
         IF (STRNG2.EQ.'PRODHDR') THEN
            NSCAN=2
            CALL USCAN2 (STRNG1,'=',NSCAN,PRODHDR,LPRODHDR,IERR)
            GO TO 10
            ENDIF
         IF (STRNG2.EQ.'SORT') THEN
            IFOUND=0
            NSCAN=2
            CALL USCAN2 (STRNG1,'=',NSCAN,STRNG3,LSTRNG3,IERR)
            IF (STRNG3.EQ.'BYDATE') THEN
               SORTBY=STRNG3
               IFOUND=1
               ENDIF
            IF (STRNG3.EQ.'BYID') THEN
               SORTBY=STRNG3
               IFOUND=1
               ENDIF
            IF (STRNG3.EQ.'BOTH') THEN
               SORTBY=STRNG3
               IFOUND=1
               ENDIF
            IF (IFOUND.EQ.0) THEN
               WRITE (LP,40) 'KEYWORD',STRNG3(1:LENSTR(STRNG3)),NFLD
               INDERR=1
               ENDIF
            GO TO 10
            ENDIF
         CALL UEROR (LP,2,-1)
         WRITE (LP,40) 'OPTION',STRNG1(1:LENSTR(STRNG1)),NFLD
40    FORMAT ('0**ERROR** INVALID ',A,' (',A,') FOUND IN FIELD ',I2,'.')
         INDERR=1
         GO TO 10
         ENDIF
C
C  CHECK IF ERROR ENCOUNTERED DECODING DATES ON COMMAND LINE
      IF (INDERR.EQ.1) GO TO 80
C
C  CHECK IF DATES SPECIFIED ON COMMAND LINE
      IF (JULHRB.GT.0.AND.JULHRE.GT.0) THEN
         JHBEG=JULHRB+12
         JHEND=JULHRE+12
C     CONVERT FROM JULIAN DATE
         CALL MDYH2 (JULBEG,INTHRB,IBEGMO,IBEGDA,IBEGYR,IBEGHR,
     *      ITZ,IDSAV,TZCODE)
         CALL MDYH2 (JULEND,INTHRE,IENDMO,IENDDA,IENDYR,IENDHR,
     *      ITZ,IDSAV,TZCODE)
         GO TO 80
         ENDIF
C
C  READ CARD
50    CALL RPCARD (IBUF,IERR)
      IF (IERR.NE.0) GO TO 190
      IFSTCD=1
      CALL WPCARD (IBUF)
C
C  FIND FIELDS ON CARD
      CALL UFREE (IBCOL,IECOL)
C
C  CHECK IF NO FIELDS ON CARD
      IF (NFIELD.EQ.0) GO TO 50
C
C  CHECK NUMBER OF FIELDS
      NCHK=2
      IF (NFIELD.NE.NCHK) THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,60) NFIELD,NCHK
60    FORMAT ('0**ERROR** ',I2,' FIELDS FOUND ON CARD. ',I2,
     *   ' WERE EXPECTED.')
         INDERR=1
         GO TO 80
         ENDIF
C
C  GET FIRST FIELD
      NFLD=1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NCHAR.NE.LEN(STRNG2)) THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,70) LEN(STRNG2),NFLD,NCHAR
         INDERR=1
70    FORMAT ('0**ERROR** ',I2,' CHARACTERS WERE EXPECTED IN FIELD ',
     *   I2,' BUT ',I2,' WERE FOUND.')
         GO TO 80
         ENDIF
      STRNG2=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),STRNG2,NCHAR)
C
C  CHECK FOR COMMENT
      IF (STRNG2.EQ.'$') GO TO 50
C
C  GET BEGINING 6 HOUR PERIOD FROM FIRST FIELD
C
C  GET MONTH
      CALL UFA2I (STRNG2,1,2,IBEGMO,IPRERR,LP,IERR)
      IF (IERR.NE.0) THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,*) '**ERROR** INVALID BEGINING MONTH.'
         INDERR=1
         ENDIF
C
C  GET DAY
      CALL UFA2I (STRNG2,3,2,IBEGDA,IPRERR,LP,IERR)
      IF (IERR.NE.0) THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,*) '**ERROR** INVALID BEGINING DAY.'
         INDERR=1
         ENDIF
C
C  GET YEAR
      CALL UFA2I (STRNG2,5,2,IBEGYR,IPRERR,LP,IERR)
      IF (IERR.NE.0) THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,*) '**ERROR** INVALID BEGINING YEAR.'
         INDERR=1
         ENDIF
C
C  GET HOUR
      CALL UFA2I (STRNG2,7,2,IBEGHR,IPRERR,LP,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,*) '**ERROR** INVALID BEGINING HOUR.'
         INDERR=1
         ENDIF
C
C  CHECK THAT TIME ZONE CODE IS 'Z'
      IF (STRNG2(9:9).NE.'Z') THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,*) '**ERROR** INVALID TIME ZONE CODE : ',STRNG2(9:9)
         INDERR=1
         ENDIF
C
C  GET SECOND FIELD
      NFLD=2
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NCHAR.NE.LEN(STRNG2)) THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,70) LEN(STRNG2),NFLD,NCHAR
         GO TO 80
         ENDIF
      STRNG2=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),STRNG2,NCHAR)
C
C  GET ENDING 6 HOUR PERIOD FROM SECOND FIELD
C
C  GET MONTH
      CALL UFA2I (STRNG2,1,2,IENDMO,IPRERR,LP,IERR)
      IF (IERR.NE.0) THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,*) '**ERROR** INVALID ENDING MONTH.'
         INDERR=1
         ENDIF
C
C  GET DAY
      CALL UFA2I (STRNG2,3,2,IENDDA,IPRERR,LP,IERR)
      IF (IERR.NE.0) THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,*) '**ERROR** INVALID ENDING DAY.'
         INDERR=1
         ENDIF
C
C  GET YEAR
      CALL UFA2I (STRNG2,5,2,IENDYR,IPRERR,LP,IERR)
      IF (IERR.NE.0) THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,*) '**ERROR** INVALID ENDING YEAR.'
         INDERR=1
         ENDIF
C
C  GET HOUR
      CALL UFA2I (STRNG2,7,2,IENDHR,IPRERR,LP,IERR)
      IF (IERR.NE.0) THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,*) '**ERROR** INVALID ENDING HOUR.'
         INDERR=1
         ENDIF
C
C  CHECK THAT TIME ZONE CODE IS 'Z'
      IF (STRNG2(9:9).NE.'Z') THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,*) '**ERROR** INVALID TIME ZONE CODE : ',STRNG2(9:9)
         INDERR=1
         ENDIF
C
      IF (IPRDB.GT.0) THEN
         WRITE (IOGDB,*) 'BEGIN: ',IBEGYR,IBEGMO,IBEGDA,IBEGHR
         WRITE (IOGDB,*) 'END:   ',IENDYR,IENDMO,IENDDA,IENDHR
         ENDIF
C
C  GET 4 DIGIT YEAR
      CALL DDYCDL (IBEGYR,IBEGMO,IBEGDA)
      CALL DDYCDL (IENDYR,IENDMO,IENDDA)
C
C  SET JULIAN HOUR
      CALL DDGCH2 (JHBEG,IBEGYR,IBEGMO,IBEGDA,IBEGHR)
      CALL DDGCH2 (JHEND,IENDYR,IENDMO,IENDDA,IENDHR)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    NUMTS=0
C
C  READ CARD
90    CALL RPCARD (IBUF,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,*) '**NOTE** END OF FILE ENCOUNTERED. ',
     *     'END CARD ASSUMED.'
         IENDIN=1
         GO TO 110
         ENDIF
      IFSTCD=1
      CALL WPCARD (IBUF)
C
C  FIND FIELDS ON CARD
      CALL UFREE (IBCOL,IECOL)
C
C  CHECK IF NO FIELDS ON CARD
      IF (NFIELD.EQ.0) GO TO 90
C
      NFLD=1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      STRNG2=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),STRNG2,NCHAR)
C
C  CHECK FOR COMMENT
      IF (STRNG2.EQ.'$') GO TO 90
C
C  CHECK FOR END CARD
      IF (NFIELD.EQ.1) THEN
         IF (STRNG2.NE.'END') THEN
            CALL UEROR (LP,2,-1)
            WRITE (LP,*) '**ERROR** ''END'' CARD EXPECTED.'
            INDERR=1
            GO TO 90
            ENDIF
         GO TO 110
         ENDIF
C
C  CHECK NUMBER OF FIELDS
      NCHK=5
      IF (NFIELD.GT.NCHK+2) THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,60) NFIELD,NCHK
         INDERR=1
         GO TO 90
         ENDIF
C
      IF (NUMTS+1.GT.MAXTS) THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,100) MAXTS
100   FORMAT ('0**ERROR** MAXIMUM NUMBER OF TIME SERIES THAT CAN BE ',
     *   'PROCESSED (',I4,') EXCEEDED.')
         GO TO 190
         ENDIF
C
      NUMTS=NUMTS+1
C
C  GET FIRST FIELD (TIMES SERIES IDENTIFIER)
      NFLD=1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NCHAR.GT.LEN(TSID(1))) THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,70) LEN(TSID(1)),NFLD,NCHAR
         INDERR=1
         ELSE
            TSID(NUMTS)=' '
            CALL UPACK1 (IBUF(IFSTRT(NFLD)),TSID(NUMTS),NCHAR)
         ENDIF
C
C  GET SECOND FIELD (TIMES SERIES DATA TYPE CODE)
      NFLD=2
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NCHAR.GT.LEN(TSTYPE(1))) THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,70) LEN(TSTYPE(1)),NFLD,NCHAR
         INDERR=1
         ELSE
            TSTYPE(NUMTS)=' '
            CALL UPACK1 (IBUF(IFSTRT(NFLD)),TSTYPE(NUMTS),NCHAR)
         ENDIF
C
C  GET THIRD FIELD (SHEF STATION IDENTIFIER)
      NFLD=3
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NCHAR.GT.LEN(SHEFID(1))) THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,70) LEN(SHEFID(1)),NFLD,NCHAR
         INDERR=1
         ELSE
            SHEFID(NUMTS)=' '
            CALL UPACK1 (IBUF(IFSTRT(NFLD)),SHEFID(NUMTS),NCHAR)
         ENDIF
C
C  GET FOURTH FIELD (SHEF DATA TIME INTERVAL)
      NFLD=4
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      MCHAR=2
      IF (NCHAR.GT.MCHAR) THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,70) MCHAR,NFLD,NCHAR
         INDERR=1
         ELSE
            STRNG2=' '
            CALL UPACK1 (IBUF(IFSTRT(NFLD)),STRNG2,NCHAR)
            CALL UFA2I (STRNG2,1,NCHAR,ITIME(NUMTS),IPRERR,LP,IERR)
            IF (IERR.NE.0) THEN
               CALL UEROR (LP,2,-1)
               WRITE (LP,*) '**ERROR** INVALID TIME INTERVAL.'
               INDERR=1
               ENDIF
         ENDIF
C
C  GET FIFTH FIELD (SHEF PARAMETER CODE)
      NFLD=5
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NCHAR.GT.LEN(SHEFCD(1))) THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,70) LEN(SHEFCD(1)),NFLD,NCHAR
         INDERR=1
         ELSE
            SHEFCD(NUMTS)=' '
            CALL UPACK1 (IBUF(IFSTRT(NFLD)),SHEFCD(NUMTS),NCHAR)
         ENDIF
C
C  CHECK IF DATA FORMAT SPECIFIED
      NFLD=6
      IF (NFIELD.GE.NFLD) THEN
         NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
         IF (NCHAR.GT.LEN(DATAFMT(1))) THEN
            CALL UEROR (LP,2,-1)
            WRITE (LP,70) LEN(DATAFMT(1)),NFLD,NCHAR
            INDERR=1
            ELSE
               CALL UPACK1 (IBUF(IFSTRT(NFLD)),DATAFMT(NUMTS),NCHAR)
               CHKSTR='.'
               IF (DATAFMT(NUMTS).NE.'.') THEN
                  CALL UINDEX (DATAFMT(NUMTS),NCHAR,CHKSTR,LEN(CHKSTR),
     *               LFOUND)
                  IF (LFOUND.EQ.0) THEN
                     CALL UEROR (LP,2,-1)
                     WRITE (LP,'(1H0,9A)')
     *                  '**ERROR** CHARACTER ''',
     *                  CHKSTR(1:LENSTR(CHKSTR)),
     *                  ''' NOT FOUND IN DATA FORMAT ''',
     *                  DATAFMT(NUMTS)(1:NCHAR),
     *                  '''.'
                     INDERR=1
                     ELSE
                        IF (LFOUND.GT.1.AND.LFOUND.LT.NCHAR) THEN
                           ELSE
                              CALL UEROR (LP,2,-1)
                              WRITE (LP,'(1H0,9A)')
     *                           '**ERROR** POSITION OF CHARACTER ''',
     *                           CHKSTR(1:LENSTR(CHKSTR)),
     *                           ''' IN DATA FORMAT ''',
     *                           DATAFMT(NUMTS)(1:NCHAR),
     *                           ''' IS INVALID.'
                              INDERR=1
                           ENDIF
                     ENDIF
                  ENDIF
               IBEG=1
               NDEC=0
               IPRERR=0
               CALL UFA2F (DATAFMT(NUMTS),IBEG,NCHAR,NDEC,VALUE,IPRERR,
     *            LP,IERR)
               IF (IERR.NE.0) THEN
                  CALL UEROR (LP,2,-1)
                  WRITE (LP,'(1H0,9A)')
     *               '**ERROR** DATA FORMAT ''',
     *               DATAFMT(NUMTS)(1:LENSTR(DATAFMT(NUMTS))),
     *               ''' IS INVALID.'
                  INDERR=1
                  ENDIF
            ENDIF
         ENDIF
C
C  CHECK IF NUMBER OF VALUES PER CARD SPECIFIED
      NFLD=7
      IF (NFIELD.GE.NFLD) THEN
         NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
         MCHAR=2
         IF (NCHAR.GT.MCHAR) THEN
            CALL UEROR (LP,2,-1)
            WRITE (LP,70) MCHAR,NFLD,NCHAR
            INDERR=1
            ELSE
               STRNG2=' '
               CALL UPACK1 (IBUF(IFSTRT(NFLD)),STRNG2,NCHAR)
               CALL UFA2I (STRNG2,1,NCHAR,NPERCD(NUMTS),IPRERR,LP,IERR)
               IF (IERR.NE.0) THEN
                  CALL UEROR (LP,2,-1)
                  WRITE (LP,*) '**ERROR** INVALID NUMBER OF VALUES ',
     *               'PER CARD.'
                  INDERR=1
                  ENDIF
            ENDIF
         ENDIF
C
      IF (INDERR.EQ.1) THEN
         NUMTS=NUMTS-1
         INDERR=0
         ENDIF
      GO TO 90
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
110   IF (NUMTS.EQ.0) GO TO 190
C
      IF (SORTBY.EQ.' ') THEN
         SORTBY='BYID'
         WRITE (LP,*) '**NOTE** SORTBY OPTION SET TO ',
     *      SORTBY(1:LENSTR(SORTBY)),'.'
         ENDIF
C
C  GET CURRENT DATE
      CALL DDRMCL (IYEAR,IMON,IDAY,IHOUR,IMIN,ISEC)
C
C  CHECK IF TO OUTPUT SHEF HEADERS
      IF (HEADERS.EQ.'YES') THEN
         WRITE (ICDPUN,120) IDAY,IHOUR,IMIN
120   FORMAT ('TTAA00 CCCC ',I2.2,I2.2,I2.2)
         WRITE (ICDPUN,'(A)') PRODHDR
         ENDIF
C
      IBEGHRO=IBEGHR
      IBEGDAO=IBEGDA
      IBEGMOO=IBEGMO
      IBEGYRO=IBEGYR
C
      IDATE=0
      NDTYPES=-1
      CALL UMEMST (0,ITSERR,MAXTS)
C
C  CHECK IF TO OUTPUT BY NAME
      IF (SORTBY.EQ.'BYID'.OR.SORTBY.EQ.'BOTH') THEN
         CALL UMEMST (0,ITSCM,MAXTS)
C     PROCESS EACH TIME SERIES
         DO 140 ICOUNT=1,NUMTS
C        PROCESS EACH PERIOD FROM BEGINING HOUR TO END
            IBEGHR=IBEGHRO
            IBEGDA=IBEGDAO
            IBEGMO=IBEGMOO
            IBEGYR=IBEGYRO
            ISTEP=ITIME(ICOUNT)
            NBEGHR=IBEGHR
            NADDAY=0
            IF (IDOTA.EQ.1) NPERCD(ICOUNT)=1
            IF (IDOTE.EQ.1) THEN
               IF (NPERCD(ICOUNT).EQ.0) NPERCD(ICOUNT)=8
               ENDIF
            DO 130 JHOUR=JHBEG,JHEND,ISTEP*NPERCD(ICOUNT)
               IREPL=0
               CALL PRDSHF1 (IBEGYR,IBEGMO,IBEGDA,IBEGHR,IDATE,IREPL,
     *            IHRZ23,IDOTA,IDOTE,NBEGHR,NADDAY,LDEBUG)
               CALL PRDSHF2 (ICOUNT,JHOUR,JHBEG,JHEND,ISTEP,TZCODE,
     *            IYEAR,IMON,IDAY,IHOUR,IMIN,
     *            IBEGYR,IBEGMO,IBEGDA,IBEGHR,
     *            MDTYPES,NDTYPES,DTYPES,DUNITS,CONVFS1,CONVOS,CONVFS2,
     *            MAXTS,TSID,TSTYPE,ITIME,SHEFID,SHEFCD,DATAFMT,
     *            ITSCM,SHEFCM,NSHEFCM,
     *            IDOTA,IDOTE,NDOTE,NPERCD(ICOUNT),NBEGHR,NADDAY,
     *            LDEBUG,IERR)
               IF (IERR.NE.0) GO TO 140
               IBEGHR=IBEGHR+ISTEP*NPERCD(ICOUNT)
130            CONTINUE
140         CONTINUE
         ENDIF
C
C  CHECK IF TO OUTPUT BY DATE
      IF (SORTBY.EQ.'BYDATE'.OR.SORTBY.EQ.'BOTH') THEN
         DO 150 I=1,NSHEFCM
            SHEFCM(I)='NO'
150         CONTINUE
         CALL UMEMST (0,ITSCM,MAXTS)
C     PROCESS EACH PERIOD FROM BEGINING HOUR TO END
         IBEGHR=IBEGHRO
         ISTEP=ITIME(ICOUNT)
         IF (IDOTA.EQ.1) NPERCD(ICOUNT)=1
         IF (IDOTE.EQ.1) THEN
            IF (NPERCD(ICOUNT).EQ.0) NPERCD(ICOUNT)=8
            ENDIF
         DO 170 JHOUR=JHBEG,JHEND,ISTEP*NPERCD(ICOUNT)
            IREPL=0
            CALL PRDSHF1 (IBEGYR,IBEGMO,IBEGDA,IBEGHR,IDATE,IREPL,
     *         IHRZ23,IDOTA,IDOTE,NBEGHR,NADDAY,LDEBUG)
C        PROCESS EACH TIME SERIES
            DO 160 ICOUNT=1,NUMTS
               IF (ITSERR(ICOUNT).EQ.1) GO TO 160
               CALL PRDSHF2 (ICOUNT,JHOUR,JHBEG,JHEND,ISTEP,TZCODE,
     *            IYEAR,IMON,IDAY,IHOUR,IMIN,
     *            IBEGYR,IBEGMO,IBEGDA,IBEGHR,
     *            MDTYPES,NDTYPES,DTYPES,DUNITS,CONVFS1,CONVOS,CONVFS2,
     *            MAXTS,TSID,TSTYPE,ITIME,SHEFID,SHEFCD,DATAFMT,
     *            ITSCM,SHEFCM,NSHEFCM,
     *            IDOTA,IDOTE,NDOTE,NPERCD(ICOUNT),NBEGHR,NADDAY,
     *            NPERCD,LDEBUG,IERR)
               IF (IERR.NE.0) ITSERR(ICOUNT)=1
               IBEGHR=IBEGHR+ISTEP*NPERCD(ICOUNT)
160            CONTINUE
170         CONTINUE
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      WRITE (LP,180) NUMTS
180   FORMAT ('0**NOTE** ',I4,' TIME SERIES PROCESSED.')
C
190   RETURN
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE PRDSHF1 (IBEGYR,IBEGMO,IBEGDA,IBEGHR,IDATE,IREPL,
     *   IHRZ23,IDOTA,IDOTE,NBEGHR,NADDAY,LDEBUG)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
C
C
      MAXHR=24
      IF (IHRZ23.EQ.1) MAXHR=23
C
C  CHECK BEGINNING HOUR
      IF (IBEGHR.GE.MAXHR) THEN
C     CHECK IF NEED TO RESET HOUR
         IF (IBEGHR.GT.MAXHR) THEN
            IF (IDOTA.EQ.1) THEN
               NADDAY=1
               IBEGHR=IBEGHR-24
               ENDIF
            IF (IDOTE.EQ.1) THEN
               IBEGHR=NBEGHR
               ENDIF
            IF (IHRZ23.EQ.1) IBEGHR=0
            IBEGDA=IBEGDA+NADDAY
            NADDAY=0
C        GET MAXIMUM DAYS PER MONTH
            CALL DDGCDM (IBEGYR,IBEGMO,MAXDAY)
C        CHECK IF NEED TO RESET MONTH AND YEAR
            IF (IBEGDA.GT.MAXDAY) THEN
               IBEGDA=1
               IBEGMO=IBEGMO+1
               IF (IBEGMO.GT.12) THEN
                  IBEGMO=1
                  IBEGYR=IBEGYR+1
                  ENDIF
               ENDIF
            NHOUR=IBEGHR
            NDAY=IBEGDA
            NMON=IBEGMO
            NYEAR=IBEGYR
            ELSE
               NHOUR=0
               NDAY=IBEGDA+1
               IF (NDAY.GT.MAXDAY) THEN
                  NDAY=1
                  NMON=IBEGMO+1
                  ENDIF
               IF (NMON.GT.12) THEN
                  NMON=1
                  NYEAR=IBEGYR+1
                  ENDIF
               ENDIF
         ELSE
            NHOUR=IBEGHR
            NDAY=IBEGDA
            NMON=IBEGMO
            NYEAR=IBEGYR
         ENDIF
C
      IF (IDATE.EQ.1) THEN
         WRITE (LP,10) NMON,NDAY,NYEAR,NHOUR
10    FORMAT (' BEGIN PROCESSING DATA FOR ',
     *   I2.2,'/',I2.2,'/',I4.4,' ',I2.2,'Z')
         ENDIF
C
      IF (IREPL.EQ.1) THEN
         WRITE (ICDPUN,20) NMON,NDAY,NHOUR
20    FORMAT ('./ REPL NAME=RR',I1,I2.2,I2.2,'Z')
         ENDIF
C
      RETURN
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE PRDSHF2 (ICOUNT,JHOUR,JHBEG,JHEND,ISTEP,TZCODE,
     *   IYEAR,IMON,IDAY,IHOUR,IMIN,
     *   IBEGYR,IBEGMO,IBEGDA,IBEGHR,
     *   MDTYPES,NDTYPES,DTYPES,DUNITS,CONVFS1,CONVOS,CONVFS2,
     *   MAXTS,TSID,TSTYPE,ITIME,SHEFID,SHEFCD,DATAFMT,
     *   ITSCM,SHEFCM,NSHEFCM,
     *   IDOTA,IDOTE,NDOTE,NPERCD,NBEGHR,NADDAY,
     *   LDEBUG,ISTAT)
C
      CHARACTER*(*) TZCODE
      CHARACTER*(*) DTYPES(MDTYPES),DUNITS(MDTYPES)
      CHARACTER*(*) SHEFCD(MAXTS),DATAFMT(MAXTS)
      CHARACTER*(*) TSTYPE(MAXTS),TSID(MAXTS),SHEFID(MAXTS)
      CHARACTER*(*) SHEFCM(NSHEFCM)
      DIMENSION ITIME(MAXTS),ITSCM(MAXTS)
      DIMENSION CONVFS1(MDTYPES)
      CHARACTER*(*) CONVOS(MDTYPES),CONVFS2(MDTYPES)
      CHARACTER*4 DTYPE,DUNIT,DDIMN,BUNIT
      CHARACTER*5 SUNIT
      CHARACTER*10 CONVO
      CHARACTER*100 FILNAM
      SAVE FILNAM
C
      CHARACTER*8 CTSID,FTSID
      CHARACTER*10 STRNG
      CHARACTER*72 CHAR1,CHAR2
      CHARACTER*100 FORMT/' '/,FORMT2/' '/
      CHARACTER*200 BUFDAT,TBUF1,TBUF2,TBUF0    !!TBUF0 was added by xfan
      CHARACTER*8 TMBR
      CHARACTER*8 DDNAME/' '/
      CHARACTER*8 MBR/'PRD2SHEF'/
      CHARACTER*80 REC
C
C  PROCESSED DATA BASE ARRAYS
C
C  HSD bug r25-49 10/2004 xfan
C  When doing a dumpshef of REEO3_FX the output file shows an error
C  to the effect that the work array is not big enough.
C
C     PARAMETER (LIWORK=2000)
      PARAMETER (LIWORK=20000)
      DIMENSION IWORK(LIWORK)
      PARAMETER (LTSDAT=1500)
      DIMENSION TSDAT(LTSDAT)
      DIMENSION XBUF(1),IHEAD(22)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
C
C
      ISTAT=0
      INDERR=0
C
C  CHECK IF NEED TO READ PRD TO SHEF FILE
      IF (NDTYPES.EQ.-1) THEN
         NDTYPES=0
C     CHECK IF DATASET IS ALLOCATED
         IPRERR=1
         CALL UDDNST (DDNAME,LSYS,IPRERR,IERR)
         IF (IERR.GT.0) THEN
            GO TO 160
            ENDIF
C      GET FILE NAME
         CALL UPPFIX ('SYST',MBR,FILNAM,LFILNAM)
         TMBR=MBR
         IPRERR=0
         NUMREC=0
C     READ FILE
10       CALL URDPDS (DDNAME,TMBR,IPRERR,REC,LRECL,NUMREC,IFLAG,IRCPDS)
         IF (IPRDB.GT.0) THEN
            WRITE (IOGDB,*) ' REC=',REC
            ENDIF
         IF (IRCPDS.EQ.0.OR.IRCPDS.EQ.2) THEN
            ELSE
               WRITE (LP,20) IRCPDS
20    FORMAT ('+*** ERROR - IN PRDSHF2 - URDPDS STATUS CODE=',I3)
               GO TO 160
            ENDIF
C     CHECK FOR BLANK RECORD
         IF (REC(1:72).EQ.' ') THEN
            IF (IRCPDS.EQ.2) GO TO 40
            GO TO 10
            ENDIF
C     CHECK FOR COMMENT CARD
         IF (REC(1:1).EQ.'*'.OR.REC(1:1).EQ.'$') THEN
            IF (IPRDB.GT.0) THEN
               WRITE (IOGDB,*) ' REC=',rec
               ENDIF
            IF (IRCPDS.EQ.2) GO TO 40
            GO TO 10
            ENDIF
C     CHECK IF MAXIMUM TYPES EXCEEDED
         IF (NDTYPES+1.GT.MDTYPES) THEN
            WRITE (LP,30) MDTYPES
30    FORMAT ('+*** ERROR - IN PRDSHF2 - MAXIMUM NUMBER OF DATA TYPES ',
     *   'THAT CAN BE PROCESSED (',I2,') EXCEEDED.')
            GO TO 160
            ENDIF
         NDTYPES=NDTYPES+1
C     GET DATA TYPE CODE
         NSCAN=1
         CALL USCAN2 (REC,' ',NSCAN,STRNG,LSTRNG,IERR)
         DTYPES(NDTYPES)=STRNG
C     GET UNIT CODE
         NSCAN=NSCAN+1
         CALL USCAN2 (REC,' ',NSCAN,STRNG,LSTRNG,IERR)
         DUNITS(NDTYPES)=STRNG
C     GET CONVERSION FACTOR 1
         NSCAN=NSCAN+1
         CALL USCAN2 (REC,' ',NSCAN,STRNG,LSTRNG,IERR)
         IBEG=1
         NDEC=0
         IPRERR=1
         CALL UFA2F (STRNG,IBEG,LSTRNG,NDEC,REAL,IPRERR,LP,IERR)
         IF (IERR.NE.0) INDERR=1
         CONVFS1(NDTYPES)=REAL
C     GET OPERATOR
         NSCAN=NSCAN+1
         CALL USCAN2 (REC,' ',NSCAN,STRNG,LSTRNG,IERR)
         IF (STRNG.EQ.'+'.OR.
     *       STRNG.EQ.'-'.OR.
     *       STRNG.EQ.'*'.OR.
     *       STRNG.EQ.'/') THEN
            ELSE
               WRITE (LP,'(99A)')
     *            '+*** ERROR - IN PRDSHF2 - ',
     *            STRNG(1:LENSTR(STRNG)),
     *            ' IS AN INVALID OPERATOR.'
               INDERR=1
            ENDIF
         CONVOS(NDTYPES)=STRNG
C     GET CONVERSION FACTOR 2
         NSCAN=NSCAN+1
         CALL USCAN2 (REC,' ',NSCAN,STRNG,LSTRNG,IERR)
         CONVFS2(NDTYPES)=STRNG
         IF (IPRDB.GT.0) THEN
            WRITE (IOGDB,*)
     *         ' NDTYPES=',NDTYPES,
     *         ' DTYPES(NDTYPES)=',DTYPES(NDTYPES),
     *         ' DUNITS(NDTYPES)=',DUNITS(NDTYPES),
     *         ' CONVFS1(NDTYPES)=',CONVFS1(NDTYPES),
     *         ' CONVOS(NDTYPES)=',CONVOS(NDTYPES),
     *         ' CONVFS2(NDTYPES)=',CONVFS2(NDTYPES)
               ENDIF
         IF (IRCPDS.EQ.2) GO TO 40
         GO TO 10
C     CLOSE FILE
40       TMBR=' '
         IPRERR=1
         CALL URDPDS (DDNAME,TMBR,IPRERR,REC,LRECL,NUMREC,IFLAG,IRCPDS)
         ENDIF
C
C  SET IDENTIFIER AND DATA TYPE
      CTSID=TSID(ICOUNT)
      DTYPE=TSTYPE(ICOUNT)
C
C  SET DATA UNITS AND CONVERSION FACTORS
      DO 50 I=1,NDTYPES
         IF (DTYPE.EQ.DTYPES(I)) THEN
            DUNIT=DUNITS(I)
            CONVF1=CONVFS1(I)
            CONVO=CONVOS(I)
            IF (CONVFS2(I).EQ.' ') THEN
               CONVF=CONVF1
               ELSE
                  IF (CONVFS2(I).EQ.'24/ISTEP') THEN
                     CONVF2=24/ISTEP
                     ELSE
C                    CHECK IF DATA UNITS CODE
                        DDIMN=' '
                        IPRERR=0
                        STRNG=CONVFS2(I)
                        CALL UDUCHK (DDIMN,STRNG,BUNIT,IPRERR,IERR)
                        IF (IERR.EQ.0) THEN
                           ICONV=2
                           CALL UDUCNV (DUNIT,STRNG,ICONV,NVAL,
     *                        VALUE,VALUE2,IERR)
                           IF (IERR.NE.0) INDERR=1
                           ELSE
                              IBEG=1
                              NCHAR=LENSTR(STRNG)
                              NDEC=0
                              IPRERR=1
                              CALL UFA2F (STRNG,IBEG,NCHAR,NDEC,VALUE,
     *                           IPRERR,LP,IERR)
                              IF (IERR.NE.0) INDERR=1
                           ENDIF
                        CONVF2=VALUE
                     ENDIF
                  IF (CONVO.EQ.'+') THEN
                     CONVF=CONVF1+CONVF2
                     ENDIF
                  IF (CONVO.EQ.'-') THEN
                     CONVF=CONVF1-CONVF2
                     ENDIF
                  IF (CONVO.EQ.'*') THEN
                     CONVF=CONVF1*CONVF2
                     ENDIF
                  IF (CONVO.EQ.'/') THEN
                     CONVF=CONVF1/CONVF2
                     ENDIF
               ENDIF
            IF (IPRDB.GT.0) THEN
               WRITE (IOGDB,*)
     *            ' DTYPE=',DTYPE,
     *            ' DUNIT=',DUNIT,
     *            ' CONVF1=',CONVF1,
     *            ' CONVF2=',CONVF2,
     *            ' CONVF=',CONVF
               ENDIF
            GO TO 70
            ENDIF
50        CONTINUE
      CALL UEROR (LP,2,-1)
      WRITE (LP,60) CTSID,DTYPE,FILNAM(1:LENSTR(FILNAM))
60    FORMAT ('0**ERROR** DATA TYPE SPECIFIED FOR STATION ',A,
     *   ' (',A,') NOT FOUND IN FILE ',A,'.')
      GO TO 160
C
70    IF (INDERR.EQ.1) GO TO 160
C
C  READ TIME SERIES HEADER
      LXBUF=1
      CALL RPRDH (CTSID,DTYPE,LXBUF,IHEAD,NXBUF,XBUF,FTSID,IERR)
      IF (IERR.NE.0) THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,80) CTSID,DTYPE
80    FORMAT ('0**ERROR** TIME SERIES NOT FOUND FOR IDENTIFIER ',
     *      A,' AND DATA TYPE ',A,'.')
         IF (IERR.EQ.1) THEN
            ELSE
               CALL UEROR (LP,2,-1)
               WRITE (LP,*) '**ERROR** RPRDH ERROR:',
     *            ' IERR=',IERR,
     *            ' CTSID=',CTSID,
     *            ' DTYPE=',DTYPE
            ENDIF
         ISTAT=1
         GO TO 160
         ENDIF
C
C  READ TIME SERIES DATA
      NTSDAT=NPERCD
      IF (LDEBUG.GT.0) THEN
         WRITE (LP,*) 'JHOUR=',JHOUR,' NTSDAT=',NTSDAT,' JHEND=',JHEND
         ENDIF
      IHEND=JHOUR+NTSDAT*ISTEP
      IF (IHEND.GT.JHEND) THEN
         NTSDAT=NTSDAT-(IHEND-JHEND)/ISTEP+1
         IF (LDEBUG.GT.0) THEN
            WRITE (LP,*) 'NTSDAT=',NTSDAT
            ENDIF
         ENDIF
      RMISS=-999.
      IFPTR=0
      IF (IPRDB.GT.0) THEN
         WRITE (IOGDB,*)
     *      ' CTSID=',CTSID,
     *      ' DTYPE=',DTYPE,
     *      ' JHOUR=',JHOUR,
     *      ' ISTEP=',ISTEP,
     *      ' NTSDAT=',NTSDAT,
     *      ' DUNIT=',DUNIT,
     *      ' RMISS=',RMISS,
     *      ' IFPTR=',IFPTR
         ENDIF
      CALL RPRDD (CTSID,DTYPE,JHOUR,ISTEP,NTSDAT,DUNIT,
     *   RMISS,LTSDAT,TSDAT,IFPTR,LIWORK,IWORK,IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.EQ.2) THEN
            CALL UEROR (LP,2,-1)
            WRITE (LP,90)
     *         IBEGMO,IBEGDA,IBEGYR,IBEGHR,TZCODE(1:LENSTR(TZCODE)),
     *         CTSID,DTYPE
90    FORMAT (' ',
     *   '**ERROR** TIME PERIOD (',
     *   I2.2,'/',I2.2,'/',I4.4,'-',I2.2,A,
     *   ') NOT FOUND OR NOT ENOUGH DATA ',
     *   'FOR IDENTIFIER ',A,' AND ',
     *   'DATA TYPE ',A,'.')
            ELSE
               CALL UEROR (LP,2,-1)
               WRITE (LP,*) '**ERROR** RPRDD ERROR:',
     *            ' IERR=',IERR,
     *            ' CTSID=',CTSID,
     *            ' DTYPE=',DTYPE,
     *            ' JHOUR=',JHOUR,
     *            ' '
            ENDIF
         GO TO 160
         ENDIF
C
C  CHECK IF TO OUTPUT SHEF COMMENT CARD
      IF (ITSCM(ICOUNT).EQ.0) THEN
         ITSCM(ICOUNT)=1
         IF (SHEFCM(1).EQ.'YES') THEN
            SUNIT=DUNIT
            IF (CONVF1.EQ.0.001) SUNIT='K'//SUNIT
            WRITE (TBUF1,100)
     *         TSID(ICOUNT),
     *         TSTYPE(ICOUNT),
     *         SUNIT,
     *         SHEFID(ICOUNT),
     *         SHEFCD(ICOUNT)
100   FORMAT (
     *   ':',1X,
     *   'TS_ID=',A,3X,
     *   'TS_DATA_TYPE=',A,3X,
     *   'DATA_UNITS=',A,3X,
     *   'SHEF_ID=',A,3X,
     *   'SHEF_DATA_TYPE=',A,3X,
     *   ' ')
            CALL PRDSHF3 (TBUF1,LTBUF1,LDEBUG)
            WRITE (ICDPUN,'(A)') TBUF1(1:LTBUF1)
            ELSE
               DO 110 I=1,NSHEFCM
                  IF (SHEFCM(1).NE.'NO') THEN
                     CALL PRDSHF3 (SHEFCM(I),LSHEFCM,LDEBUG)
                     WRITE (ICDPUN,'(A)') SHEFCM(I)(1:LSHEFCM)
                     ENDIF
110               CONTINUE
               CALL UMEMST (1,ITSCM,MAXTS)
            ENDIF
         ENDIF
C
C  CHECK IF TO OUTPUT DATA IN .A FORMAT
      IF (IDOTA.EQ.1) THEN
         FORMT='(''.A '',A,1X,I4.4,I2.2,I2.2,1X,'''
         CALL UCNCAT (FORMT,TZCODE(1:LENSTR(TZCODE)),IERR)
         CALL UCNCAT (FORMT,' DH'',I2.2,',IERR)
         CALL UCNCAT (FORMT,'''/DC'',',IERR)
         CALL UCNCAT (FORMT,'I4.4,I2.2,I2.2,I2.2,I2.2,',IERR)
         CALL UCNCAT (FORMT,'''/DUE/'',A,1X,A',IERR)
         CALL UCNCAT (FORMT,')',IERR)
         I=1
C     CHECK IF MISSING DATA
         IF (TSDAT(I).EQ.RMISS) THEN
            TBUF2='M'
            ELSE
               TSVAL=TSDAT(I)*CONVF
               IF (IPRDB.GT.0) THEN
                  WRITE (IOGDB,*)
     *              ' TSVAL=',TSVAL,
     *               ' CONVF=',CONVF
                  ENDIF
               NSIG=5
               IF (TSVAL.GT.-1.0.AND.TSVAL.LT.1.0) THEN
                  NPLACE=5
                  NDECML=2
                  ELSE
                     INEG=0
                     IF (TSVAL.LT.0) INEG=1
                     NDIGIT=INT(LOG10(ABS(TSVAL)))+1
                     IF (INEG.EQ.1) NDIGIT=NDIGIT+1
                     NDECML=NSIG-NDIGIT
                     IF (NDIGIT.LT.1) NDIGIT=1
                     IF (NDECML.LT.0) NDECML=0
                     NPLACE=NDIGIT+NDECML+1
                  ENDIF
               IF (NPLACE.GE.1.AND.NPLACE.LE.9) NCHAR1=1
               IF (NPLACE.GE.10.AND.NPLACE.LE.99) NCHAR1=2
               IPRERR=1
               CHAR1=' '
               CALL UFI2A (NPLACE,CHAR1,1,NCHAR1,IPRERR,LP,IERR)
               IF (NDECML.GE.1.AND.NDECML.LE.9) NCHAR2=1
               IF (NDECML.GE.10.AND.NDECML.LE.99) NCHAR2=2
               CHAR2=' '
               CALL UFI2A (NDECML,CHAR2,1,NCHAR2,IPRERR,LP,IERR)
               FORMT2='(F'
               IF (DATAFMT(ICOUNT).EQ.'.') THEN
                  CALL UCNCAT (FORMT2,CHAR1,IERR)
                  CALL UCNCAT (FORMT2,'.',IERR)
                  CALL UCNCAT (FORMT2,CHAR2,IERR)
                  ELSE
                     CALL UCNCAT (FORMT2,DATAFMT(ICOUNT),IERR)
                  ENDIF
               CALL UCNCAT (FORMT2,')',IERR)
               WRITE (TBUF2,FORMT2) TSVAL
            ENDIF
C     FORMAT, CHECK AND OUTPUT SHEF LINE
         WRITE (TBUF1,FORMT) SHEFID(ICOUNT),
     *      IBEGYR,IBEGMO,IBEGDA,IBEGHR,
     *      IYEAR,IMON,IDAY,IHOUR,IMIN,
     *      SHEFCD(ICOUNT),TBUF2(1:LENSTR(TBUF2))
         CALL PRDSHF3 (TBUF1,LTBUF1,LDEBUG)
         WRITE (ICDPUN,'(A)') TBUF1(1:LTBUF1)
         ENDIF
C
C  CHECK IF TO OUTPUT DATA IN .E FORMAT
      IF (IDOTE.EQ.1) THEN
         DO 120 I=1,NTSDAT
C        CHECK IF MISSING DATA
            IF (TSDAT(I).EQ.RMISS) THEN
               TSVAL=TSDAT(I)
               ELSE
                  TSVAL=TSDAT(I)*CONVF
               ENDIF
            IF (IPRDB.GT.0) THEN
               WRITE (IOGDB,*)
     *            ' I=',I,
     *            ' TSVAL=',TSVAL,
     *            ' '
               ENDIF
            NSIG=4
            IF (TSVAL.GT.-1.0.AND.TSVAL.LT.1.0) THEN
               NPLACE=5
               NDECML=2
               ELSE
                  INEG=0
                  IF (TSVAL.LT.0) INEG=1
                  NDIGIT=INT(LOG10(ABS(TSVAL)))+1
                  IF (INEG.EQ.1) NDIGIT=NDIGIT+1
                  NDECML=NSIG-NDIGIT
                  IF (NDIGIT.LT.1) NDIGIT=1
                  IF (NDECML.LT.0) NDECML=0
                  NPLACE=NDIGIT+NDECML+1
               ENDIF
            IF (NPLACE.GE.1.AND.NPLACE.LE.9) NCHAR1=1
            IF (NPLACE.GE.10.AND.NPLACE.LE.99) NCHAR1=2
            IPRERR=1
            CHAR1=' '
            CALL UFI2A (NPLACE,CHAR1,1,NCHAR1,IPRERR,LP,IERR)
            IF (NDECML.GE.1.AND.NDECML.LE.9) NCHAR2=1
            IF (NDECML.GE.10.AND.NDECML.LE.99) NCHAR2=2
            CHAR2=' '
            CALL UFI2A (NDECML,CHAR2,1,NCHAR2,IPRERR,LP,IERR)
            IF (TSVAL.NE.RMISS) THEN
               FORMT=' '
               CALL UCNCAT (FORMT,'(F',IERR)
               IF (DATAFMT(ICOUNT).EQ.'.') THEN
                  CALL UCNCAT (FORMT,CHAR1,IERR)
                  CALL UCNCAT (FORMT,'.',IERR)
                  CALL UCNCAT (FORMT,CHAR2,IERR)
                  ELSE
                     CALL UCNCAT (FORMT,DATAFMT(ICOUNT),IERR)
                  ENDIF
               IF (I.LT.NTSDAT) THEN
                  CALL UCNCAT (FORMT,',''/'')',IERR)
                  ELSE
                     CALL UCNCAT (FORMT,')',IERR)
                  ENDIF
               WRITE (TBUF1,FORMT) TSVAL
               IF (TBUF1(1:1).EQ.'*') THEN
                  CALL UEROR (LP,2,-1)
                  WRITE (LP,*) '**ERROR** CANNOT OUTPUT DATA VALUE ',
     *               TSVAL,' USING FORMAT ',FORMT(1:LENSTR(FORMT)),'.'
                  ENDIF
               LTBUF1=LENSTR(TBUF1)
               ELSE
                  IF (DATAFMT(ICOUNT).EQ.'.') THEN
                     LTBUF1=NPLACE
                     ELSE
                        NSCAN=1
                        CALL USCAN2 (DATAFMT(ICOUNT),'.',NSCAN,STRNG,
     *                     LSTRNG,IERR)
                        IBEG=1
                        IPRERR=1
                        CALL UFA2I (STRNG,IBEG,LSTRNG,LTBUF1,IPRERR,LP,
     *                     IERR)
                     ENDIF
                  TBUF1=' '
                  TBUF1(LTBUF1:LTBUF1)='M'
                  LTBUF1=LTBUF1+1
                  IF (I.LT.NTSDAT) TBUF1(LTBUF1:LTBUF1)='/'
               ENDIF
            IF (I.EQ.1) THEN
               WRITE (BUFDAT,'(A)') TBUF1(1:LTBUF1)
               ELSE
                  WRITE (TBUF2,'(A,A)')
     *               BUFDAT(1:LENSTR(BUFDAT)),TBUF1(1:LTBUF1)
                  BUFDAT=TBUF2
               ENDIF
120         CONTINUE
         IF (LDEBUG.GT.0) THEN
            WRITE (LP,*) 'NBEGHR=',NBEGHR
            ENDIF
         IF (JHOUR.EQ.JHBEG) THEN
C        FORMAT, CHECK AND OUTPUT SHEF HEADER LINE
            WRITE (TBUF1,130) SHEFID(ICOUNT),
     *         IBEGYR,IBEGMO,IBEGDA,TZCODE(1:LENSTR(TZCODE)),
     *         NBEGHR,
     *         IYEAR,IMON,IDAY,IHOUR,IMIN,
     *         SHEFCD(ICOUNT),ISTEP
130   FORMAT ('.E ',A,' ',
     *    I4.4,I2.2,I2.2,' ',A,
     *   ' DH',I2.2,
     *   '/DC',I4.4,I2.2,I2.2,I2.2,I2.2,
     *   '/DUE/',A,'/','DIH',I2.2)

            CALL PRDSHF3 (TBUF1,LTBUF1,LDEBUG)
            WRITE (ICDPUN,'(A)') TBUF1(1:LTBUF1)
            NDOTE=0
            ENDIF
C     FORMAT, CHECK AND OUTPUT SHEF DATA LINE
         NDOTE=NDOTE+1
         WRITE (TBUF1,'(I3)') NDOTE
         CALL ULEFTC (TBUF1,LEN(TBUF1),LTBUF1)
cfan
cfan     HSD bug r23-47
cfan     When running prdutil dumpshef the shef message is returning a ".E." instead of a .E1, .E2 etc
cfan     Originaly it works fine on HP, but doesn't work on LINUX.
cfan     Find: when write string to the variable itself, there are different behavior on HP and LINUX.
cfan
cfan     WRITE (TBUF1,140) TBUF1(1:LTBUF1),
cfan *      BUFDAT(1:LENSTR(BUFDAT))
         WRITE (TBUF2,140) TBUF1(1:LTBUF1),        !cfan
     *      BUFDAT(1:LENSTR(BUFDAT))               !cfan
140   FORMAT ('.E',A,' ','/',A)
cfan     CALL PRDSHF3 (TBUF1,LTBUF1,LDEBUG)
         CALL PRDSHF3 (TBUF2,LTBUF1,LDEBUG)        !cfan
cfan     WRITE (ICDPUN,'(A)') TBUF1(1:LTBUF1)
         WRITE (ICDPUN,'(A)') TBUF2(1:LTBUF1)      !cfan
c
         DO 150 I=1,NTSDAT
            NBEGHR=NBEGHR+ISTEP
            IF (NBEGHR.GT.24) THEN
               NADDAY=NADDAY+1
               IF (LDEBUG.GT.0)  THEN
                  WRITE (LP,*) 'NADDAY=',NADDAY
                  ENDIF
               NBEGHR=ISTEP
               ENDIF
150         CONTINUE
         ENDIF
C
160   RETURN
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE PRDSHF3 (STRNG,LSTRNG,LDEBUG)
C
      CHARACTER*(*) STRNG
C
      INCLUDE 'uiox'
C
      LSTRNG=LENSTR(STRNG)
C
      IF (LDEBUG.GT.0)  THEN
          WRITE (LP,*) 'STRNG=',LSTRNG
          ENDIF
C
      MSTRNG=80

      IF (LSTRNG.GT.MSTRNG) THEN
         CALL UWARN (LP,2,-1)
         WRITE (LP,10) LSTRNG,MSTRNG
10    FORMAT ('0**WARNING** NUMBER OF CHARACTERS (',I3,
     *      ') IN THE FOLLOWING SHEF LINE EXCEEDS ',I3,' CHARACTERS:')
         IF (ICDPUN.NE.LP) THEN
            WRITE (LP,'(1H ,A)') STRNG(1:LSTRNG)
            ENDIF
         ENDIF
C
      RETURN
C
      END
