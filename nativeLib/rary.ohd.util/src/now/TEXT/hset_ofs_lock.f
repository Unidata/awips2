C$PRAGMA C (GET_APPS_DEFAULTS)
C  =====================================================================
C  pgm: SET_OFS_LOCK .. Establish the OFS file lock 
C
C  use:     CALL SET_OFS_LOCK(LOCK_TYPE,LOCK_ID,KOND)
C
C   in: LOCK_TYPE ..... type of lock, either 'read' or 'write' - CHAR*5
C   in:                  only first character is significant
C   in: LOCK_ID ....... An identifier for the lock name to create.  It
C   in:                 must be one of "PPP", "PDB", "PRD", "FCESP", or 
C   in:                 "HCLUSER".
C  out: KOND .......... status of lock open command - INT
C  out:                   = 0 ... lock file opened
C  out:                   = 1 ... lock file already in use, cannot open
C  out:                   = 2 ... error during lock file access
C  out:                   = 3 ... unable to build lock file name
C
C  rqd: GET_APPS_DEFAULTS,MAKE_OFS_LOCK,LOCKON,KKPOS
C  =====================================================================
      SUBROUTINE HSET_OFS_LOCK(LOCK_TYPE,LOCK_ID,KOND)

CC AV pgf90 port 7/2/01      EXTERNAL     GET_APPS_DEFAULTS,MAKE_OFS_LOCK,LOCKON,KKPOS
CC      EXTERNAL MAKE_OFS_LOCK,LOCKON,KKPOS
      CHARACTER*5    LOCK_TYPE
      CHARACTER*7    LOCK_ID
      INTEGER        KOND

      CHARACTER*240  LOCK_NAME
      CHARACTER*5    STRNG
      CHARACTER*240  LOCKS_DIR
      INTEGER        LLD,LLF,MAXT,INTWT,NOFC
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/hset_ofs_lock.f,v $
     . $',                                                             '
     .$Id: hset_ofs_lock.f,v 1.2 2004/08/11 13:12:26 hank Exp $
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
        KOND = 3
        GOTO 999
      ENDIF

C  Get the max waiting time for setting the OFS lock (default 10 min)

      CALL GET_APPS_DEFAULTS('ofs_lock_max_wait',17,STRNG,NOFC)
      CALL KKPOS(STRNG(1:NOFC),MAXT)
      IF (MAXT.LE.0 .OR. MAXT.GT.60) MAXT = 10

C  Get the retry interval time for setting OFS lock if lock
C   is in use by another process (in seconds)  (default 5 secs)

      CALL GET_APPS_DEFAULTS('ofs_lock_wait_interval',22,STRNG,NOFC)
      CALL KKPOS(STRNG(1:NOFC),INTWT)
      IF (INTWT.LE.0 .OR. INTWT.GT.300) INTWT = 5

C  Establish the lock

      CALL LOCKON(LOCKS_DIR(1:LLD),LLD,LOCK_NAME(1:LLF),LLF,
     $            LOCK_TYPE,MAXT,INTWT,KOND)

999   RETURN
      END
