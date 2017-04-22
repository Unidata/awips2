C MODULE SMFCR2
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT THE ATTRIBUES OF THE DATA FILES CURRENTLY ALLOCATED.
C
      SUBROUTINE SMFCR2 (IUSFC,IPRTBL,IPUJCL,NDSTRK,DSUNIT,ISTAT)
C
      CHARACTER*4 DSUNIT
      CHARACTER*8 FILES(99)
      CHARACTER*8 UNDEF/'--NONE--'/
      CHARACTER*44 QLFDSN/' '/
C
      DIMENSION NDSTRK(1)
      DIMENSION KPRDTS(5),KFCFIL(10)
C
      INCLUDE 'uio'
      INCLUDE 'uunits'
      INCLUDE 'ufreex'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pppcommon/ppunts'
      INCLUDE 'prdcommon/punits'
      INCLUDE 'common/fcunit'
C
      EQUIVALENCE (KPRDTS(1),KMAPTS)
      EQUIVALENCE (KFCFIL(1),KFCGD)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_dump/RCS/smfcr2.f,v $
     . $',                                                             '
     .$Id: smfcr2.f,v 1.2 1998/07/06 12:41:30 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,60)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DUMP')
C
      ISTAT=0
      MFILES=99
      DSUNIT=' '
C
C  CHECK IF NUMBER OF TRACKS FOR EACH UNIT NUMBER TO BE RETURNED
      IDSTRK=0
      IF (NDSTRK(1).GT.0) THEN
         IDSTRK=1
         DO 10 I=1,99
            NDSTRK(I)=0
10          CONTINUE
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SET INDICATOR TO PRINT NOTES IF DATA BASE NOT ALLOCATED
      INDMSG=-2
      IDUPRM=0
      IDHCL=0
      IDCLB=0
      IDPPD=0
      IDPPP=0
      IDPRD=0
      IDFC=0
      IDESP=0
C
C  CHECK IF FORECAST COMPONENT DATA FILES TO BE USED
      IF (IUSFC.EQ.0) IDFC=-1
C
C  SET INDICATOR NOT TO USE CALIBRATION DATA FILES
      IDCLB=-1
C
C  CHECK WHICH FILE ARE ALLOCATED
      CALL SUDACK (INDMSG,IDUPRM,IDHCL,IDCLB,IDPPD,IDPPP,IDPRD,
     *   IDFC,IDESP,IERR)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  GET FILE NAMES AND UNIT NUMBERS
      CALL UDSFLN ('OFS5',UNDEF,MFILES,NFILES,FILES,QLFDSN,IERR)
C
      IPRERR=1
      NTRKS=0
      NDSN=0
C
C  USER PARAMETER DATA FILE
      IF (IDUPRM.GT.0) THEN
         IF (IPUJCL.EQ.0) THEN
            WRITE (LP,70)
            CALL SULINE (LP,1)
            ENDIF
         CALL SMFCR3 (KUPARM,IPRERR,INDMSG,IPRTBL,IPUJCL,DSUNIT,
     *      NDSN,IDSTRK,NDSTRK,NTRKS,IERR)
         ENDIF
C
C  HCL LOCAL DATA BASE
      IF (IDHCL.GT.0) THEN
         IF (IPUJCL.EQ.0) THEN
            WRITE (LP,70)
            CALL SULINE (LP,1)
            ENDIF
         CALL SMFCR3 (KINDXL,IPRERR,INDMSG,IPRTBL,IPUJCL,DSUNIT,
     *      NDSN,IDSTRK,NDSTRK,NTRKS,IERR)
         CALL SMFCR3 (KDEFNL,IPRERR,INDMSG,IPRTBL,IPUJCL,DSUNIT,
     *      NDSN,IDSTRK,NDSTRK,NTRKS,IERR)
         CALL SMFCR3 (KLDFGD,IPRERR,INDMSG,IPRTBL,IPUJCL,DSUNIT,
     *      NDSN,IDSTRK,NDSTRK,NTRKS,IERR)
         ENDIF
C
C  CALIBRATION DATA FILES
      IF (IDCLB.GT.0) THEN
         ENDIF
C
C  PREPROCESSOR DATA BASE
      IF (IDPPD.GT.0) THEN
         IF (IPUJCL.EQ.0) THEN
            WRITE (LP,70)
            CALL SULINE (LP,1)
            ENDIF
         CALL SMFCR3 (KPDSIF,IPRERR,INDMSG,IPRTBL,IPUJCL,DSUNIT,
     *      NDSN,IDSTRK,NDSTRK,NTRKS,IERR)
         CALL SMFCR3 (KPDRRS,IPRERR,INDMSG,IPRTBL,IPUJCL,DSUNIT,
     *      NDSN,IDSTRK,NDSTRK,NTRKS,IERR)
         IOFSET=0
         IF (IAMORD.EQ.1) IOFSET=-10
         DO 20 I=1,5
            IUNIT=KPDDDF(I)
            IF (FILES(IUNIT+IOFSET).EQ.UNDEF) GO TO 20
               CALL SMFCR3 (IUNIT,IPRERR,INDMSG,IPRTBL,IPUJCL,DSUNIT,
     *            NDSN,IDSTRK,NDSTRK,NTRKS,IERR)
20          CONTINUE
         ENDIF
C
C  PARAMETRIC DATA BASE
      IF (IDPPP.GT.0) THEN
         IF (IPUJCL.EQ.0) THEN
            WRITE (LP,70)
            CALL SULINE (LP,1)
            ENDIF
         CALL SMFCR3 (KPPIDX,IPRERR,INDMSG,IPRTBL,IPUJCL,DSUNIT,
     *      NDSN,IDSTRK,NDSTRK,NTRKS,IERR)
         IOFSET=0
         IF (IAMORD.EQ.1) IOFSET=20
         DO 30 I=1,5
            IUNIT=KPPRMU(I)
            IF (FILES(IUNIT+IOFSET).EQ.UNDEF) GO TO 30
               CALL SMFCR3 (IUNIT,IPRERR,INDMSG,IPRTBL,IPUJCL,DSUNIT,
     *            NDSN,IDSTRK,NDSTRK,NTRKS,IERR)
30          CONTINUE
         ENDIF
C
C  PROCESSED DATA BASE
      IF (IDPRD.GT.0) THEN
         IF (IPUJCL.EQ.0) THEN
            WRITE (LP,70)
            CALL SULINE (LP,1)
            ENDIF
         CALL SMFCR3 (KRFCPR,IPRERR,INDMSG,IPRTBL,IPUJCL,DSUNIT,
     *      NDSN,IDSTRK,NDSTRK,NTRKS,IERR)
         CALL SMFCR3 (KINDEX,IPRERR,INDMSG,IPRTBL,IPUJCL,DSUNIT,
     *      NDSN,IDSTRK,NDSTRK,NTRKS,IERR)
         IOFSET=0
         IF (IAMORD.EQ.1) IOFSET=20
         DO 40 I=1,5
            IUNIT=KPRDTS(I)
            IF (FILES(IUNIT+IOFSET).EQ.UNDEF) GO TO 40
               CALL SMFCR3 (IUNIT,IPRERR,INDMSG,IPRTBL,IPUJCL,DSUNIT,
     *            NDSN,IDSTRK,NDSTRK,NTRKS,IERR)
40          CONTINUE
         ENDIF
C
C  FORECAST COMPONENT DATA BASE
      IF (IDFC.GT.0) THEN
         IF (IPUJCL.EQ.0) THEN
            WRITE (LP,70)
            CALL SULINE (LP,1)
            ENDIF
         IOFSET=0
         IF (IAMORD.EQ.1) IOFSET=-10
         DO 50 I=1,10
            IUNIT=KFCFIL(I)
            IF (FILES(IUNIT+IOFSET).EQ.UNDEF) GO TO 50
               CALL SMFCR3 (IUNIT,IPRERR,INDMSG,IPRTBL,IPUJCL,DSUNIT,
     *            NDSN,IDSTRK,NDSTRK,NTRKS,IERR)
50          CONTINUE
         ENDIF
C
C  PRINT TOTAL TRACKS ALLOCATED TO FILES
      IF (IPUJCL.EQ.0) THEN
         WRITE (LP,80) NTRKS
         CALL SULINE (LP,2)
         ENDIF
C
      CALL UCLOSL
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,90)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT (' *** ENTER SMFCR2')
70    FORMAT (' ')
80    FORMAT (T105,6('-') /
     *   T105,I6)
90    FORMAT (' *** EXIT SMFCR2')
C
      END
