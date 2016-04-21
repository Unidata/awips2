C MODULE UPRIMO_BAT
C  =====================================================================
C  pgm: UPRIMO_BAT .. Initialize I/O units for batchpst
C
C  use:     CALL UPRIMO_BAT()
C
C  rqd: URTIMR,UPINIO,UPRIMR,UPRIMW,UPRIMT,UPCLOS
C  =====================================================================
      SUBROUTINE UPRIMO_BAT()

      EXTERNAL   UPRIMR,URTIMR,UPRIMW,UPRIMT,UPCHKD,UPCLOS
      EXTERNAL SET_OFS_LOCK, FREE_OFS_LOCK

      CHARACTER*39  INSTMT,OTSTMT,PUSTMT
      CHARACTER*5   LOCK_TYPE
      INTEGER       KOND,ISTAT,IERR,LAPSE,ITMBEG

      INCLUDE 'uiox'
      INCLUDE 'upagex'
      INCLUDE 'udebug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/errdat'
      INCLUDE 'common/sysbug'

C                    "UIOX" is the shared library version of uio
C                    "UCMDBX" is for the shared library debug

      COMMON /UIOX/ LP2,ICD2,LPD2,LPE2,ICDPU2,LSYS2
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/batchpst/RCS/uprimo_bat.f,v $
     . $',                                                             '
     .$Id: uprimo_bat.f,v 1.7 2004/06/23 13:29:44 hank Exp $
     . $' /
C    ===================================================================
C

      DATA  INSTMT / ' *** Enter input filename, TTY or Q:  ' /
      DATA  OTSTMT / ' *** Enter output filename, TTY or Q: ' /
      DATA  PUSTMT / ' *** Enter punch filename, TTY or Q:  ' /


C                         Set cpu timer, sys level i/o unit numbers

        CALL URTIMR(LAPSE,ITMBEG)
        CALL UPINIO()

C                         Try to establish the OFS file lock

        LOCK_TYPE='write'

CHDH  Change made on 2004-06-22 by Hank Herr.  Lock the appropriate
CHDH  files.
C        CALL SET_OFS_LOCK(LOCK_TYPE,KOND)
        CALL HLOCKFILES('BATCHPST',KOND)

        IF (KOND .GT. 0) STOP 16

C                         Get user id (used only by "UPAGE2"); also
C                         set in "ublock.f"

        PUSRID = ' '

C                         Initialize common for function "ifbug".

        NDEBGS = 0
        IALL   = 0

        LSYS2  = 3
        ITRACE = 0

C                          Set the two globals needed by every program

        CALL UPCHKD('SYST OPER',ISTAT)

        ICD    = 0
        LP     = 0
        ICDPUN = 0
        IF (ISTAT  .EQ. 0) CALL UPRIMR(INSTMT,'F',59,ICD)
CCC      IF (ICD    .GT. 0) CALL UPRIMW(OTSTMT,'F',6,LP)
        IF (ICD    .GT. 0) CALL UPRIMW(OTSTMT,'F',98,LP)
        IF (LP     .GT. 0) CALL UPRIMW(PUSTMT,'F',8,ICDPUN)
        IF (ICDPUN .LE. 0) THEN

C                         Need an emergency stop here

          IF (LP  .GT. 0) CALL UPCLOS(LP,' ',IERR)
          IF (ICD .GT. 0) CALL UPCLOS(ICD,' ',IERR)

CHDH  Change made on 2004-06-22 by Hank Herr. Use new lock mechanism.          
          CALL HUNLOCKFILES('BATCHPST',KOND)
C          CALL FREE_OFS_LOCK(KOND)
          STOP 32

        ELSE

C                         Files opened, set other global variables

          LPD    = LP
          LPE    = LP
          IOGDB  = LP
          IN     = ICD
          IPR    = LP
          IPU    = ICDPUN
          IODBUG = LP
          IOERR  = LP

C                         Set second common area for i/o for
C                         routines using UIOX instead of UIO

          LP2    = LP
          ICD2   = ICD
          LPD2   = LPD
          LPE2   = LPE
          ICDPU2 = ICDPUN

C                         Set shared library debug parameters

          ICMPRU = LP
          ICMTRC = 0
          ICMDBG = 0

        ENDIF

      RETURN
      END
