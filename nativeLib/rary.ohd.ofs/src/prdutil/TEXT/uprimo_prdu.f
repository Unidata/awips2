C MODULE UPRIMO_PRDU
C  =====================================================================
C  pgm: UPRIMO_PRDU .. Initialize I/O units for prdutil
C
C  rqd: URTIMR,UPINIO,UPCHKD,UPRIMR,UPRIMW,UPCLOS
C  rqd: SET_OFS_LOCK,FREE_OFS_LOCK
C  rqd: COMMON: UPDAIO and other nwsrfs-ofs common
C  =====================================================================
      SUBROUTINE UPRIMO_PRDU()

      EXTERNAL   URTIMR,UPINIO,UPCHKD,UPRIMR,UPRIMW,UPCLOS
      EXTERNAL   SET_OFS_LOCK,FREE_OFS_LOCK

      CHARACTER*39  INSTMT,OTSTMT,PUSTMT
      INTEGER       KOND

      CHARACTER*5   LOCK_TYPE

      INCLUDE 'uio'
      INCLUDE 'updaio'
      common /CMPRDUTIL/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
      INCLUDE 'upagex'
      INCLUDE 'udebug'
      INCLUDE 'udsi'
      INCLUDE 'uunits'
      INCLUDE 'ucmdbx'
      INCLUDE 'common/ionum'
      INCLUDE 'common/errdat'
      INCLUDE 'common/sysbug'
      INCLUDE 'common/fdbug'

C                    "UIOX" is the shared library version of uio

      COMMON /UIOX/ LP2,ICD2,LPD2,LPE2,ICDPU2,LSYS2
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/prdutil/RCS/uprimo_prdu.f,v $
     . $',                                                             '
     .$Id: uprimo_prdu.f,v 1.4 2002/02/11 21:10:29 dws Exp $
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
        CALL SET_OFS_LOCK(LOCK_TYPE,KOND)
        IF (KOND .GT. 0) STOP 16

C                         Get user id

        PUSRID = ' '

C                         Initialize common sysbug for function "ifbug"

        NDEBGS = 0
        IALL   = 0

        KDTYPE = 3
        LSYS2  = KDTYPE

        ITRACE = 0

C                          Set the two globals needed by every program

        CALL UPCHKD('SYST OPER',ISTAT)

C                          Set input, output, punch unit numbers

        ICD    = 0
        LP     = 0
        ICDPUN = 0
        IF (ISTAT  .EQ. 0) CALL UPRIMR(INSTMT,'F',59,ICD)
CCC        IF (ICD    .GT. 0) CALL UPRIMW(OTSTMT,'F',6,LP)
        IF (ICD    .GT. 0) CALL UPRIMW(OTSTMT,'F',98,LP)
        IF (LP     .GT. 0) CALL UPRIMW(PUSTMT,'F',8,ICDPUN)

        IF (ICDPUN .LE. 0) THEN

C                         Need an emergency stop here

          IF (LP  .GT. 0) CALL UPCLOS(LP,' ',IERR)
          IF (ICD .GT. 0) CALL UPCLOS(ICD,' ',IERR)
          CALL FREE_OFS_LOCK(KOND)
          STOP 32

        ELSE

C                         Set debug, error unit numbers

          LPD    = LP
          LPE    = LP
          IOGDB  = LP
          IOERR  = LPE
          IODBUG = LP
          IN     = ICD
          IPR    = LP
          IPU    = ICDPUN

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
