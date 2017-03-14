C MODULE UPRIMO_SIZE
C  =====================================================================
C  pgm: UPRIMO_SIZE .. Initialize I/O units
C  =====================================================================
      SUBROUTINE UPRIMO_SIZE()

      EXTERNAL   URTIMR,UPINIO,UPCHKD,UPRIMR,UPRIMW,UPCLOS
      EXTERNAL   SET_OFS_LOCK,FREE_OFS_LOCK

      CHARACTER*39  INSTMT,OTSTMT,PUSTMT
      INTEGER       KOND

      CHARACTER*5   LOCK_TYPE

      INCLUDE 'uio'
      INCLUDE 'upagex'
      INCLUDE 'updaio'
      INCLUDE 'udebug'
      INCLUDE 'udsi'
      INCLUDE 'uunits'
      INCLUDE 'ucmdbx'

C                    "UIOX" is the shared library version of uio

      COMMON /UIOX/ LP2,ICD2,LPD2,LPE2,ICDPU2,LSYS2
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/filesize/RCS/uprimo_size.f,v $
     . $',                                                             '
     .$Id: uprimo_size.f,v 1.3 2002/02/11 20:45:12 dws Exp $
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

C                         Set the two globals needed by every program

        CALL UPCHKD('SYST OPER',ISTAT)

C                         Set input, output, punch unit numbers

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

          LPD   = LP
          LPE   = LP
          IOGDB = LP

C                         Set second common area for i/o for
C                         routines using UIOX instead of UIO

          LP2    = LP
          ICD2   = ICD
          LPD2   = LPD
          LPE2   = LPE
          ICDPU2 = ICDPUN
          LSYS2  = 3

C                         Set shared library debug parameters

          ICMPRU = LP
          ICMTRC = 0
          ICMDBG = 0

          KUPARM = 4
          KDTYPE = 3

        ENDIF

      RETURN
      END
