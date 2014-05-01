C MODULE SUDOPN
C-----------------------------------------------------------------------
C
C  ROUTINE TO OPEN DATA BASES FOR READING AND WRITING
C
      SUBROUTINE SUDOPN (NDB,DBNMS,ISTAT)
C
      CHARACTER*4 DBNMS(NDB)
C
      DIMENSION KFCUNT(1)
C
      INCLUDE 'uiox'
      INCLUDE 'uunits'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suddsx'
      INCLUDE 'scommon/supagx'
      INCLUDE 'scommon/swrk2x'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'common/fcunit'
C
      EQUIVALENCE (KFCGD,KFCUNT(1))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/sudopn.f,v $
     . $',                                                             '
     .$Id: sudopn.f,v 1.3 2001/06/13 13:32:08 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,*) 'ENTER SUDOPN'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG CODE
      LDEBUG=ISBUG('SYS ')
C
      ISTAT=0
      NUMERR=0
      NUMWRN=0
C
      NANDB=IABS(NDB)
C
      IF (LDEBUG.GT.1) THEN
         WRITE (IOSDBG,60) NDB,NANDB,(DBNMS(I),I=1,NANDB)
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,70) IDBFIL
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,80) 'IDBALC=',IDBALC
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,80) 'IDBOPN=',IDBOPN
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,80) 'IDBWRT=',IDBWRT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (NANDB.EQ.0) THEN
         WRITE (LP,110)
         CALL SUWRNS (LP,2,NUMWRN)
         GO TO 40
         ENDIF
C
      IF (IDBFIL.EQ.0) THEN
         CALL SUDDST (IERR)
         IF (IERR.GT.0) GO TO 40
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      DO 30 IDB=1,NANDB
         IF (DBNMS(IDB).EQ.'SYS') THEN
C        OPEN SYSTEM DATASET
            GO TO 30
            ENDIF
         IF (DBNMS(IDB).EQ.'UPRM') THEN
            IPOS=2
            IF (LDEBUG.GT.1) THEN
               WRITE (IOSDBG,90) IPOS,IDBALC(IPOS),IDBOPN(IPOS)
               CALL SULINE (IOSDBG,1)
               ENDIF
            IF (IDBALC(IPOS).EQ.0) THEN
               WRITE (LP,120) DBNMS(IDB)
               CALL SUERRS (LP,2,NUMERR)
               GO TO 30
               ENDIF
            IF (IDBOPN(IPOS).EQ.1) GO TO 30
C        READ USER PARAMETER FILE CONTROL INFORMATION
            CALL HGETPM (IERR)
            IF (LDEBUG.GT.1) THEN
               WRITE (IOSDBG,100) IERR
               CALL SULINE (IOSDBG,1)
               ENDIF
             CALL HGETP2 (IERR2)
            IF (IERR.GT.0.OR.IERR2.GT.0) THEN
               IF (NPSPAG.EQ.0) CALL SUPAGE
               WRITE (LP,130) IERR,IERR2
               CALL SUERRS (LP,2,NUMERR)
               GO TO 30
               ENDIF
            CALL SUDOP2 (NLSTZ,LOCAL)
            IDBOPN(IPOS)=1
            GO TO 30
            ENDIF
         IF (DBNMS(IDB).EQ.'HCL') THEN
            IPOS=3
            IF (LDEBUG.GT.1) THEN
               WRITE (IOSDBG,90) IPOS,IDBALC(IPOS),IDBOPN(IPOS)
               CALL SULINE (IOSDBG,1)
               ENDIF
            IF (IDBALC(IPOS).EQ.0) THEN
               WRITE (LP,120) DBNMS(IDB)
               CALL SUERRS (LP,2,NUMERR)
               GO TO 30
               ENDIF
            IF (IDBOPN(IPOS).EQ.1) GO TO 30
C        READ HYDROLOGIC COMMAND LANGUAGE CONTROL INFORMATION
            CALL HRDCTL (IERR1)
            CALL HRIDXC (IERR2)
            IREC=1
            CALL UREADT (KLDFGD,IREC,SWRK2,IERR3)
            IF (IERR1.GT.0.OR.IERR2.GT.0.OR.IERR3.GT.0) THEN
               IF (NPSPAG.EQ.0) CALL SUPAGE
               WRITE (LP,140)
               CALL SUERRS (LP,2,NUMERR)
               WRITE (LP,150) IERR1,IERR2,IERR3
               CALL SULINE (LP,1)
               GO TO 30
               ENDIF
            IDBOPN(IPOS)=1
            GO TO 30
            ENDIF
         IF (DBNMS(IDB).EQ.'PPD') THEN
            IPOS=4
            IF (LDEBUG.GT.1) THEN
               WRITE (IOSDBG,90) IPOS,IDBALC(IPOS),IDBOPN(IPOS)
               CALL SULINE (IOSDBG,1)
               ENDIF
            IF (IDBALC(IPOS).EQ.0) THEN
               WRITE (LP,120) DBNMS(IDB)
               CALL SUERRS (LP,2,NUMERR)
               GO TO 30
               ENDIF
            IF (IDBOPN(IPOS).EQ.1) GO TO 30
C        READ PREPROCESSOR DATA BASE CONTROL INFORMATION AND
C        INTEGER AND CHARACTER HASH
            CALL RPPDCO (IERR)
            IKEY=0
            CALL RPDHSH (IKEY,IERR2)
            IF (IERR.GT.0.OR.IERR2.GT.0) THEN
               IF (NPSPAG.EQ.0) CALL SUPAGE
               WRITE (LP,160) IERR,IERR2
               CALL SUERRS (LP,2,NUMERR)
               GO TO 30
               ENDIF
C        READ FIRST RECORD FROM EACH DAILY DATA FILE
            IREC=1
            DO 10 NUNIT=1,NMPPDF
               IF (KPDDDF(NUNIT).EQ.0) GO TO 10
               CALL UREADT (KPDDDF(NUNIT),IREC,SWRK2,IERR)
10             CONTINUE
            IDBOPN(IPOS)=1
            GO TO 30
            ENDIF
         IF (DBNMS(IDB).EQ.'PPP') THEN
            IPOS=5
            IF (LDEBUG.GT.1) THEN
               WRITE (IOSDBG,90) IPOS,IDBALC(IPOS),IDBOPN(IPOS)
               CALL SULINE (IOSDBG,1)
               ENDIF
            IF (IDBALC(IPOS).EQ.0) THEN
               WRITE (LP,120) DBNMS(IDB)
               CALL SUERRS (LP,2,NUMERR)
               GO TO 30
               ENDIF
            IF (IDBOPN(IPOS).EQ.1) GO TO 30
C        READ PARAMETRIC DATA BASE CONTROL INFORMATION
            CALL RPPPCO (IERR)
            IF (IERR.GT.0) THEN
               IF (NPSPAG.EQ.0) CALL SUPAGE
               WRITE (LP,170) IERR
               CALL SUERRS (LP,2,NUMERR)
               GO TO 30
               ENDIF
            IDBOPN(IPOS)=1
            GO TO 30
            ENDIF
         IF (DBNMS(IDB).EQ.'PRD') THEN
            IPOS=6
            IF (LDEBUG.GT.1) THEN
               WRITE (IOSDBG,90) IPOS,IDBALC(IPOS),IDBOPN(IPOS)
               CALL SULINE (IOSDBG,1)
               ENDIF
            IF (IDBALC(IPOS).EQ.0) THEN
               WRITE (LP,120) DBNMS(IDB)
               CALL SUERRS (LP,2,NUMERR)
               GO TO 30
               ENDIF
            IF (IDBOPN(IPOS).EQ.1) GO TO 30
C        READ PROCESSED DATA BASE CONTROL INFORMATION
            CALL RPDBCI (IERR)
            IF (IERR.GT.0) THEN
               IF (NPSPAG.EQ.0) CALL SUPAGE
               WRITE (LP,180) IERR
               CALL SUERRS (LP,2,NUMERR)
               GO TO 30
               ENDIF
            IDBOPN(IPOS)=1
            GO TO 30
            ENDIF
         IF (DBNMS(IDB).EQ.'FC') THEN
            IPOS=7
            IF (LDEBUG.GT.1) THEN
               WRITE (IOSDBG,90) IPOS,IDBALC(IPOS),IDBOPN(IPOS)
               CALL SULINE (IOSDBG,1)
               ENDIF
            IF (IDBALC(IPOS).EQ.0) THEN
               WRITE (LP,120) DBNMS(IDB)
               CALL SUERRS (LP,2,NUMERR)
               GO TO 30
               ENDIF
            IF (IDBOPN(IPOS).EQ.1) GO TO 30
C        READ FIRST RECORD FROM EACH FILE
            IREC=1
            DO 20 NUNIT=1,10
               IF (KFCUNT(NUNIT).EQ.0) GO TO 20
               CALL UREADT (KFCUNT(NUNIT),IREC,SWRK2,IERR)
20             CONTINUE
            IDBOPN(IPOS)=1
            GO TO 30
            ENDIF
         IF (DBNMS(IDB).EQ.'SASM') THEN
            IPOS=8
            IF (LDEBUG.GT.1) THEN
               WRITE (IOSDBG,90) IPOS,IDBALC(IPOS),IDBOPN(IPOS)
               CALL SULINE (IOSDBG,1)
               ENDIF
            IF (IDBALC(IPOS).EQ.0) THEN
               WRITE (LP,120) DBNMS(IDB)
               CALL SUERRS (LP,2,NUMERR)
               GO TO 30
               ENDIF
            IDBOPN(IPOS)=1
            GO TO 30
            ENDIF
         IF (DBNMS(IDB).EQ.'GOES') THEN
            IPOS=9
            IF (LDEBUG.GT.1) THEN
               WRITE (IOSDBG,90) IPOS,IDBALC(IPOS),IDBOPN(IPOS)
               CALL SULINE (IOSDBG,1)
               ENDIF
            IF (IDBALC(IPOS).EQ.0) THEN
               WRITE (LP,120) DBNMS(IDB)
               CALL SUERRS (LP,2,NUMERR)
               GO TO 30
               ENDIF
            IDBOPN(IPOS)=1
            GO TO 30
            ENDIF
         IF (DBNMS(IDB).EQ.' ') THEN
            IPOS=10
            GO TO 30
            ENDIF
         WRITE (LP,230) DBNMS(IDB)
         CALL SUERRS (LP,2,NUMERR)
30       CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF ERRORS ENCOUNTERED
40    IF (NUMERR.GT.0) ISTAT=1
C
      IF (LDEBUG.GT.1) THEN
         WRITE (IOSDBG,80) 'IDBOPN=',IDBOPN
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,*) 'EXIT SUDOPN : ISTAT=',ISTAT
         CALL SULINE (IOSDBG,2)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT (' NDB=',I2,3X,' NANDB=',I2,3X,'DBNMS=',20(A,1X))
70    FORMAT (' IDBFIL=',I2)
80    FORMAT (' ',A,10(I2,1X))
90    FORMAT (' IPOS=',I2,3X,'IDBALC(IPOS)=',I2,3X,'IDBOPN(IPOS)=',I2)
100   FORMAT (' HGETPM CALLED : STATUS CODE=',I2)
110   FORMAT ('0*** WARNING - IN SUDOPN - NO DATA BASES ',
     *   'SPECIFIED TO BE OPENED.')
120   FORMAT ('0*** ERROR - THE ',A,' DATA BASE IS NOT ALLOCATED.')
130   FORMAT ('0*** ERROR - USERPARM ',
     *   'CONTROL INFORMATION NOT SUCCESSFULLY READ. ',
     *   'HGETPM RETURN CODE=',I3,3X,
     *   'HGETP2 RETURN CODE=',I3)
140   FORMAT ('0*** ERROR - HYDROLOGIC COMMAND LANGUAGE ',
     *   'CONTROL INFORMATION NOT SUCCESSFULLY READ.')
150   FORMAT (T14,
     *   'HRDCTL RETURN CODE=',I3,3X,
     *   'HRIDXC RETURN CODE=',I3,3X,
     *   'UREADT RETURN CODE=',I3)
160   FORMAT ('0*** ERROR - PREPROCESSOR DATA BASE ',
     *   'CONTROL INFORMATION NOT SUCCESSFULLY READ. ',
     *   'RPPDCO RETURN CODE=',I3,3X,
     *   'RPDHSH RETURN CODE=',I3)
170   FORMAT ('0*** ERROR - PARAMETRIC DATA BASE ',
     *   'CONTROL INFORMATION NOT SUCCESSFULLY READ. ',
     *   'RPPPCO RETURN CODE=',I3)
180   FORMAT ('0*** ERROR - PROCESSED DATA BASE ',
     *   'CONTROL INFORMATION NOT SUCCESSFULLY READ. ',
     *   'RPDBCI RETURN CODE=',I3)
230   FORMAT ('0*** ERROR - IN SUDOPN - INVALID DATA BASE NAME : ',A)
C
      END
