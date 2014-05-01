C MODULE UPRIMO_PARS
C  =====================================================================
C  pgm: UPRIMO_PARS .. Initialize I/O units for shefpars
C
C  use:     CALL UPRIMO_PARS(ICD,LP,LCHN,JCHN,LUPARM,ICOND)
C
C  out: ICD .... unit number for input control file (shefpars.in) - INT
C  out: LP ..... unit number for output messages - INT
C  out: LCHN ... unit number for shef-input data file - INT
C  out: JCHN ... unit number for binary shefout file - INT
C  out: LUPARM . unit number for the SHEFPARM system file - INT
C  out: ICOND .. status: 0 if ok, 1 if fatal error - INT
C
C  rqd: UPINIO,UPCHKD,UPRIMR,UPRIMW,UPPFIX,UPOPEN,UPCLOS
C  rqd: SET_OFS_LOCK,FREE_OFS_LOCK
C  rqd: COMMON: UPDAIO and other nwsrfs-ofs common
C  =====================================================================
      SUBROUTINE UPRIMO_PARS(ICD,LP,LCHN,JCHN,LUPARM,ICOND)

      EXTERNAL   UPINIO,UPCHKD,UPRIMR,UPRIMW,UPPFIX,UPOPEN,UPCLOS
      EXTERNAL   SET_OFS_LOCK,FREE_OFS_LOCK

      CHARACTER     INSTMT*37, OTSTMT*38, SISTMT*33, SOSTMT*34
      CHARACTER*128 PATHNM
      CHARACTER*5   LOCK_TYPE
      INTEGER       ICD,LP,LCHN,JCHN,LUPARM,ICOND,KOND,ISTAT,NOCH,IERR

      INCLUDE 'updaio'

C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_sub/RCS/uprimo_pars.f,v $
     . $',                                                             '
     .$Id: uprimo_pars.f,v 1.8 2004/07/21 13:43:49 hank Exp $
     . $' /
C    ===================================================================
C

      DATA  INSTMT / ' *** Enter input filename, TTY or Q:'  /
      DATA  OTSTMT / ' *** Enter output filename, TTY or Q:' /
      DATA  SISTMT / ' *** Enter SHEFIN file, TTY or Q:'      /
      DATA  SOSTMT / ' *** Enter SHEFOUT file, TTY or Q:'     /

C                         Set system level i/o unit numbers

        CALL UPINIO()

C                         Try to establish the OFS file lock

CHDH SHEFPARS does not need locks (Hank Herr, 2004-07-20)
C        LOCK_TYPE='write'
C        CALL SET_OFS_LOCK(LOCK_TYPE,KOND)
C        IF (KOND .GT. 0) STOP 16

C                          Set the two globals needed by every program

        CALL UPCHKD('SYST OPER',KOND)

C                         Open input, output, data, and shefout files

        ICD    = 0
        LP     = 0
        LCHN   = 0
        JCHN   = 0
        LUPARM = 0
        IF (KOND .EQ. 0) CALL UPRIMR(INSTMT,'F',59,ICD)
CCC        IF (ICD  .GT. 0) CALL UPRIMW(OTSTMT,'F',6,LP)
        IF (ICD  .GT. 0) CALL UPRIMW(OTSTMT,'F',98,LP)
        IF (LP   .GT. 0) CALL UPRIMR(SISTMT,'F',25,LCHN)
        IF (LCHN .GT. 0) CALL UPRIMW(SOSTMT,'U',22,JCHN)
        IF (JCHN .GT. 0) THEN
          LUPARM = 23
          CALL UPPFIX ('SYST','SHEFPARM',PATHNM,NOCH)
          ISTAT = 1
          IF (NOCH .GT. 0) CALL UPOPEN (LUPARM,PATHNM,0,'F',ISTAT)
          IF (ISTAT .NE. 0) LUPARM = 0
        ENDIF

        IF (LUPARM .LE. 0) THEN

C                         Need an emergency stop here

          IF (LP   .GT. 0) CALL UPCLOS(LP,' ',IERR)
          IF (ICD  .GT. 0) CALL UPCLOS(ICD,' ',IERR)
          IF (LCHN .GT. 0) CALL UPCLOS(LCHN,' ',IERR)
          IF (JCHN .GT. 0) CALL UPCLOS(JCHN,' ',IERR)
          CALL FREE_OFS_LOCK(KOND)
          ICOND = 1

        ELSE

          ICOND = 0

        ENDIF

      RETURN
      END
