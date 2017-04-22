C$PRAGMA C (GET_APPS_DEFAULTS)
C  =====================================================================
C  pgm: FREE_OFS_LOCK .. Release the OFS file lock 
C
C  use:     CALL FREE_OFS_LOCK(LOCK_ID,KOND)
C
C   in: LOCK_ID ....... An identifier for the lock name to create.  It
C   in:                 must be one of "PPP", "PDB", "PRD", "FCESP", or 
C   in:                 "HCLUSER".
C  out: KOND .......... status of lock release command - INT
C  out:                   = 0 ... lock file released
C  out:                   = 1 ... lock file release error
C  out:                   = 2 ... unable to build lock file name
C
C  =====================================================================
      SUBROUTINE HFREE_OFS_LOCK(LOCK_ID,KOND)

CC AV pgf90 7/2/01     EXTERNAL   GET_APPS_DEFAULTS,LOCKOFF
CC      EXTERNAL   LOCKOFF

      CHARACTER*7   LOCK_ID
      INTEGER       KOND

      CHARACTER*240 LOCK_NAME
      CHARACTER*240 LOCKS_DIR
      INTEGER       LLD,LLF
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/hfree_ofs_lock.f,v $
     . $',                                                             '
     .$Id: hfree_ofs_lock.f,v 1.2 2004/08/11 13:11:34 hank Exp $
     . $' /
C    ===================================================================
C

      DATA  LOCK_NAME /' '/

C  Get locks directory

      CALL GET_APPS_DEFAULTS('locks_dir',9,LOCKS_DIR,LLD)

        IF (LLD .EQ. 4) THEN
          IF (LOCKS_DIR(1:LLD) .EQ. 'skip') RETURN
        ENDIF

C  Get the OFS lock name

      CALL HMAKE_OFS_LOCK(LOCK_ID,LOCK_NAME,LLF,KOND)
      IF (KOND.NE.0) THEN
        KOND = 2
        GOTO 999
      ENDIF

C  Release the lock

      CALL LOCKOFF(LOCKS_DIR(1:LLD),LLD,LOCK_NAME(1:LLF),LLF,KOND)

C  That's it!

999   RETURN
      END
