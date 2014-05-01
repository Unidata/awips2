C MEMBER SUDACK
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
C DESC ROUTINE SUDACK CHECKS WHICH DATASETS ARE ALLOCATED
C
C
      SUBROUTINE SUDACK (INDMSG,IDUPRM,IDHCL,IDCLB,IDPPD,IDPPP,IDPRD,
     *   IDFC,IDESP,ISTAT)
C
      CHARACTER*8 DDN/'FT??F001'/
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suddsx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/sudack.f,v $
     . $',                                                             '
     .$Id: sudack.f,v 1.1 1995/09/17 19:15:20 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,90)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG(4HSYS )
C
      ISTAT=0
C
      IPRERR=0
C
C  CHECK IF DATA FILE ALLOCATED COMMON BLOCK FILLED
      IF (IDBFIL.EQ.0) CALL SUDDST (IERR)
C
C  USER DATASET PARAMETERS
      IF (IDUPRM.GE.0) THEN
         IF (IDBALC(2).GT.0) THEN
            IDUPRM=1
            CALL SUDALC (0,IDUPRM,0,0,0,0,0,0,0,0,INDMSG,IERR)
            IF (IERR.EQ.0) GO TO 10
            ENDIF
         ENDIF
      IDUPRM=0
C
C  HCL DATASETS
10    IF (IDHCL.GE.0) THEN
         IF (IDBALC(3).GT.0) THEN
            IDHCL=1
            CALL SUDALC (0,0,IDHCL,0,0,0,0,0,0,0,INDMSG,IERR)
            IF (IERR.EQ.0) GO TO 20
            ENDIF
         ENDIF
      IDHCL=0
C
C  CALIBRATION DATASETS
20    IF (IDCLB.GE.0) THEN
         IDCLB=1
         NINDX=36
         CALL UFXDDN (DDN,NINDX,IERR)
         IF (IERR.GT.0) THEN
            IF (NPSPAG.EQ.0) CALL SUPAGE
            WRITE (LP,100) IERR
            CALL SUERRS (LP,2,-1)
            GO TO 30
            ENDIF
         CALL UDDST (DDN,IPRERR,IERR)
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,110) DDN,IERR
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (IERR.EQ.0) GO TO 30
         ENDIF
      IDCLB=0
C
C  PREPROCESSOR DATASETS
30    IF (IDPPD.GE.0) THEN
         IF (IDBALC(4).GT.0) THEN
            IDPPD=1
            CALL SUDALC (0,0,0,IDPPD,0,0,0,0,0,0,INDMSG,IERR)
            IF (IERR.EQ.0) GO TO 40
            ENDIF
         ENDIF
      IDPPD=0
C
C  PARAMETRIC DATASETS
40    IF (IDPPP.GE.0) THEN
         IF (IDBALC(5).GT.0) THEN
            IDPPP=1
            CALL SUDALC (0,0,0,0,IDPPP,0,0,0,0,0,INDMSG,IERR)
            IF (IERR.EQ.0) GO TO 50
            ENDIF
         ENDIF
      IDPPP=0
C
C  PROCESSED DATA DATASETS
50    IF (IDPRD.GE.0) THEN
         IF (IDBALC(6).GT.0) THEN
            IDPRD=1
            CALL SUDALC (0,0,0,0,0,IDPRD,0,0,0,0,INDMSG,IERR)
            IF (IERR.EQ.0) GO TO 60
            ENDIF
         ENDIF
      IDPRD=0
C
C  FORECAST COMPONENT DATASETS
60    IF (IDFC.GE.0) THEN
         IF (IDBALC(7).GT.0) THEN
            IDFC=1
            CALL SUDALC (0,0,0,0,0,0,IDFC,0,0,0,INDMSG,IERR)
            IF (IERR.EQ.0) GO TO 70
            ENDIF
         ENDIF
      IDFC=0
C
C  ESP COMPONENT DATASETS
70    IF (IDESP.GE.0) THEN
         IDESP=1
         CALL UDDST ('FT96F001',IPRERR,IERR)
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,110) DDN,IERR
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (IERR.EQ.0) GO TO 80
         ENDIF
      IDESP=0
      WRITE (LP,120)
      CALL SULINE (LP,2)
C
80    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,130) IDUPRM,IDHCL,IDCLB,IDPPD,IDPPP,IDPRD,IDFC,
     *      IDESP
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,140)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
90    FORMAT (' *** ENTER SUDACK')
100   FORMAT ('0*** ERROR - IN SUDACK - UFXDDN NOT SUCCESSFULLY ',
     *   'CALLED. IERR=',I2)
110   FORMAT (' UDDST CALLED : DDN=',A,3X,'IERR=',I2)
120   FORMAT ('0*** NOTE - ESP DATA FILES NOT ALLOCATED.')
130   FORMAT (1H ,'IDUPRM=',I1,3X,'IDHCL=',I1,3X,'IDCLB=',I1,3X,
     *   'IDPPD=',I1,3X,'IDPPP=',I1,3X,'IDPRD=',I1,3X,
     *   'IDFC=',I1,3X,'IDESP=',I1)
140   FORMAT (' *** EXIT SUDACK')
C
      END
