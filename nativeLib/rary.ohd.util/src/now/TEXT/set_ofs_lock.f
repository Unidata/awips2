C$PRAGMA C (GET_APPS_DEFAULTS)
C  =====================================================================
C  pgm: SET_OFS_LOCK .. Establish the OFS file lock 
C
C  use:     CALL SET_OFS_LOCK(LOCK_TYPE,KOND)
C
C   in: LOCK_TYPE ..... type of lock, either 'read' or 'write' - CHAR*5
C   in:                  only first character is significant
C  out: KOND .......... status of lock open command - INT
C  out:                   = 0 ... lock file opened
C  out:                   = 1 ... lock file already in use, cannot open
C  out:                   = 2 ... error during lock file access
C
C  rqd: GET_APPS_DEFAULTS,MAKE_OFS_LOCK,LOCKON,KKPOS
C  =====================================================================
      SUBROUTINE SET_OFS_LOCK(LOCK_TYPE,KOND)

      CHARACTER*5    LOCK_TYPE
      INTEGER        KOND
      CHARACTER*24   PROGNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/set_ofs_lock.f,v $
     . $',                                                             '
     .$Id: set_ofs_lock.f,v 1.6 2004/08/11 13:12:46 hank Exp $
     . $' /
C    ===================================================================
C
      PROGNAME = 'GENERAL'
      CALL HLOCKFILES(PROGNAME,KOND)

      RETURN
      END
