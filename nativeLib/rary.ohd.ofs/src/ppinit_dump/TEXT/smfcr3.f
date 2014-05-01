C MEMBER SMFCR3
C  (from old member SMFCRT)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/07/95.14:22:22 BY $WC21DT
C
C @PROCESS LVL(77)
C
      SUBROUTINE SMFCR3 (IUNIT,IPRERR,INDMSG,IPRTBL,IPUJCL,CKUNIT,
     *   NDSN,IDSTRK,NDSTRK,NTRKS,ISTAT)
C
      CHARACTER*4 CKUNIT
      CHARACTER*4 RECFM
      CHARACTER*6 VOL
      CHARACTER*8 DDN/'?'/
      CHARACTER*44 DSN/' '/
C
      DIMENSION NDSTRK(1)
      DIMENSION CARD(20)
C
      INCLUDE 'uio'
      INCLUDE 'udsatx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_dump/RCS/smfcr3.f,v $
     . $',                                                             '
     .$Id: smfcr3.f,v 1.1 1995/09/17 19:12:45 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      ISTAT=0
C
      IF (IUNIT.EQ.0) GO TO 50
C
C
C  SET DDNAME
      DDN='FT??F001'
      CALL UFXDDN (DDN,IUNIT,IERR)
      IF (IERR.EQ.0) GO TO 10
         CALL SUERRS (LP,2,-1)
         GO TO 50
C
C  GET DATASET ATTRIBUTES
10    CALL UDSATR (DDN,DSN,VOL,RECFM,LREC,LBLK,DSORG,NTRK,
     *   NTRKU,IPRERR,IERR)
      IF (IERR.EQ.0) GO TO 20
         IF (INDMSG.EQ.-1) CALL SUWRNS (LP,2,-1)
         GO TO 50
C
C  CHECK UNIT TYPE
20    IF (CKUNIT.EQ.' ') CKUNIT=DSUNIT
      IF (CKUNIT.EQ.DSUNIT) GO TO 30
         WRITE (LP,60) DSUNIT,CKUNIT
         CALL SUWRNS (LP,2,-1)
C
C  CHECK IF NUMBER OF TRACKS FOR UNIT TO BE RETURNED
30    IF (IDSTRK.EQ.1) NDSTRK(IUNIT)=NTRK
C
C  CHECK IF DDNAME TABLE PRINT OPTION SPECIFIED
      IF (IPRTBL.EQ.0) GO TO 50
C
C  PRINT DATASET ATTRIBUTES
      IF (NDSN.GT.0) GO TO 40
         IF (ISLEFT(10).GT.0) CALL SUPAGE
         WRITE (LP,70)
         CALL SULINE (LP,3)
40    NDSN=NDSN+1
      WRITE (LP,80) NDSN,DDN,DSN,VOL,DSUNIT,RECFM,LREC,LBLK,NTRK
      CALL SULINE (LP,1)
      IF (ISNWPG(LP).EQ.1) THEN
         WRITE (LP,70)
         CALL SULINE (LP,3)
         ENDIF
      NTRKS=NTRKS+NTRK
C
C  CHECK IF JCL PUNCH OPTION SPECIFIED
      IF (IPUJCL.EQ.0) GO TO 50
C
C  PUNCH DATASET ATTRIBUTES
      CALL UREPET (' ',CARD,20*4)
      MAXCHR=4
      NPOS=1
      NSPACE=0
      ITYPE=3
      NCHECK=0
      CALL UTOCRD (ICDPUN,NPOS,'//',2,NSPACE,CARD,ITYPE,NCHECK,
     *             LNUM,IERR)
      NPOS=NPOS+5
      CALL UTOCRD (ICDPUN,NPOS,'F',1,NSPACE,CARD,ITYPE,NCHECK,
     *             LNUM,IERR)
      CALL SUBSTR ('0',1,1,CARD,NPOS)
      CALL SUBSTR (DDN,3,2,UNIT,1)
      CALL UTOCRD (ICDPUN,NPOS,UNIT,2,NSPACE,CARD,ITYPE,NCHECK,
     *             LNUM,IERR)
      CALL UTOCRD (ICDPUN,NPOS,'=',1,NSPACE,CARD,ITYPE,NCHECK,
     *             LNUM,IERR)
      CALL UINTCH (NTRK,MAXCHR,CHAR,NFILL,IERR)
      CALL UTOCRD (ICDPUN,NPOS,CHAR,MAXCHR,NSPACE,CARD,ITYPE,NCHECK,
     *             LNUM,IERR)
      CALL UTOCRD (ICDPUN,NPOS,',',1,NSPACE,CARD,ITYPE,NCHECK,
     *             LNUM,IERR)
      CALL UPNCRD (ICDPUN,CARD)
      GO TO 50
C
50    RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT ('0*** WARNING - DISK UNIT TYPE (',A4,
     *   ') DIFFERS FROM FIRST UNIT PROCESSED (',A4,').')
70    FORMAT ('0',3X,'DDNAME  ',3X,'DSNAME',38X,3X,'VOLUME',3X,
     *       'UNIT',3X,'RECFM',3X,'LRECL',3X,'BLKSIZE',3X,'TRACKS' /
     *   ' ',3X,8('-'),3X,44('-'),3X,6('-'),3X,
     *       4('-'),3X,5('-'),3X,5('-'),3X,7('-'),3X,6('-'))
80    FORMAT (' ',I2,1X,A,3X,A,3X,A,3X,A4,3X,A,1X,3X,I5,3X,
     *   I7,3X,I6)
C
      END
