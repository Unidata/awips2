C$PRAGMA C (GET_APPS_DEFAULTS)
C  =====================================================================
C  pgm: FREE_OFS_LOCK .. Release the OFS file lock 
C
C  use:     CALL FREE_OFS_LOCK(KOND)
C
C  out: KOND .......... status of lock release command - INT
C  out:                   = 0 ... lock file released
C  out:                   = 1 ... lock file release error
C
C  =====================================================================
      SUBROUTINE FREE_OFS_LOCK(KOND)

      INTEGER       KOND
      CHARACTER*24  PROGNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/free_ofs_lock.f,v $
     . $',                                                             '
     .$Id: free_ofs_lock.f,v 1.6 2004/08/11 13:11:15 hank Exp $
     . $' /
C    ===================================================================
C

      
      PROGNAME = 'GENERAL'
      CALL HUNLOCKFILES (PROGNAME, KOND)

      RETURN
      END
