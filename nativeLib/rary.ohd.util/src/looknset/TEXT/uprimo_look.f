C MODULE UPRIMO_LOOK
C  =====================================================================
C  pgm: UPRIMO_LOOK .. Initialize I/O units for looknset
C
C  rqd: URTIMR,UPINIO,UPRIMR,UPRIMW,UPCHKD
C  rqd: SET_OFS_LOCK,FREE_OFS_LOCK
C  =====================================================================
      SUBROUTINE UPRIMO_LOOK()

      EXTERNAL   URTIMR,UPINIO,UPRIMR,UPRIMW,UPCHKD
      EXTERNAL   SET_OFS_LOCK,FREE_OFS_LOCK

      CHARACTER*39  INSTMT,OTSTMT,PUSTMT
      INTEGER       KOND

      CHARACTER*5   LOCK_TYPE

      INCLUDE 'uiox'
      INCLUDE 'upvrsx'
      INCLUDE 'ucmdbx'
      INCLUDE 'udaiox'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/looknset/RCS/uprimo_look.f,v $
     . $',                                                             '
     .$Id: uprimo_look.f,v 1.3 2001/06/13 14:50:16 dws Exp $
     . $' /
C    ===================================================================
C

      DATA  INSTMT / ' *** Enter input filename, TTY, or Q:  ' /
      DATA  OTSTMT / ' *** Enter output filename, TTY, or Q: ' /
      DATA  PUSTMT / ' *** Enter punch filename, TTY, or Q:  ' /


C                         Set cpu timer, sys level i/o unit numbers

        CALL URTIMR(LAPSE,ITMBEG)
        CALL UPINIO()

C                         Try to establish the OFS file lock

        LOCK_TYPE='write'
        CALL SET_OFS_LOCK(LOCK_TYPE,KOND)
        IF (KOND .GT. 0) STOP 16

C                          Set the two globals needed by every program
 
        CALL UPCHKD('SYST OPER',ISTAT)

        ICD    = 0
        LP     = 0
        ICDPUN = 0
        IF (ISTAT .EQ. 0) CALL UPRIMR(INSTMT,'F',59,ICD)
        IF (ICD   .GT. 0) CALL UPRIMW(OTSTMT,'F',6,LP)
        IF (LP    .GT. 0) CALL UPRIMW(PUSTMT,'F',8,ICDPUN)
 
        IF (ICDPUN .LE. 0) THEN
 
C                         Need an emergency stop here
 
          IF (LP  .GT. 0) CALL UPCLOS(LP,' ',IERR)
          IF (ICD .GT. 0) CALL UPCLOS(ICD,' ',IERR)
          CALL FREE_OFS_LOCK(KOND)
          STOP 32
 
        ELSE
 
C                         Set debug, error unit numbers

          LPD = LP
          LPE = LP

          ICMPRU = LP
          ICMTRC = 0
          ICMDBG = 0

          IDAERP = 1
          IOPDBG = LP

        ENDIF

      RETURN
      END
