C$PRAGMA C (DATIM2)
cc AV pgf90 port lockset is a c routine
C$PRAGMA C (LOCKSET) 
C  =====================================================================
C  pgm: LOCKON .. Open read/write lock
C
C  use:     CALL LOCKON(DIR,DLEN,NAME,NLEN,LOCK_TYPE,MAXT,INTWT,KOND)
C
C   in: DIR ........... directory for lock files - CHAR*(*)
C   in: DLEN .......... number of chars in "DIR" - INT
C   in: NAME .......... lock name - CHAR*(*)
C   in: NLEN .......... number of chars in "NAME" - INT
C   in: LOCK_TYPE ..... lock type, either 'read' or 'write' - CHAR*(*)
C   in:                  only first character is significant
C   in: MAXT .......... maximum number of minutes to wait while trying
C   in:                  to open the lock file - INT
C   in: INTWT ......... retry interval (seconds) if lock is in use
C   in:                  by another process - INT
C  out: KOND .......... status of lock open command - INT
C  out:                   = 0 ... lock file opened
C  out:                   = 1 ... lock file already in use, cannot open
C  out:                   = 2 ... error during lock file access
C
C  rqd: LOCKSET,DATIM2,UWAIT
C  =====================================================================
      SUBROUTINE LOCKON(DIR,DLEN,NAME,NLEN,LOCK_TYPE,MAXT,INTWT,KOND)

cc AV pgf90 port 7/2/01      EXTERNAL   DATIM2,LOCKSET,UWAIT
cc AV pgf90 port 7/2/01      EXTERNAL   LOCKSET,UWAIT
      EXTERNAL  UWAIT

      CHARACTER*(*) DIR,NAME
      CHARACTER*(*) LOCK_TYPE
      INTEGER       DLEN,NLEN,MAXT,KOND
      CHARACTER*28  TIMEX
      CHARACTER*240 FILE
      INTEGER       WAIT,WAITC,MAXWAIT,ITRY,INTWT
      INTEGER       LOCKSET,LLF

      INCLUDE 'updaio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/lockon.f,v $
     . $',                                                             '
     .$Id: lockon.f,v 1.5 2002/02/11 14:34:16 michaelo Exp $
     . $' /
C    ===================================================================
C

      DATA FILE/' '/

C  Initialize total waiting period

      WAITC   = 0
      MAXWAIT = 60 * MAXT
      WAIT    = INTWT

C   Establish complete lock file path name

      LLF  = DLEN + NLEN + 1
      IF (LLF .LE. 240) FILE = DIR(1:DLEN) // '/' // NAME(1:NLEN)

      CALL DATIM2(TIMEX)

C  Display lock request information
 
      IF( UE .GE. 0 ) THEN
        WRITE(UE,'()')
        WRITE(UE,'('' Lock requested:'',a)') TIMEX(1:24)
        WRITE(UE,'(''      lock dir.: '',a)') DIR
        WRITE(UE,'(''      lock name: '',a)') NAME
        WRITE(UE,'(''      lock type: '',a)') LOCK_TYPE
      ENDIF

C  Try to open lock file for writing or reading

 100    CONTINUE 
        ITRY = LOCKSET(FILE(1:LLF),LLF,LOCK_TYPE)

C  Check on success of setting lock

         IF (ITRY .EQ. 0) THEN

C  Lock is granted

            CALL DATIM2(TIMEX)
            IF(UE.GE.0) WRITE(UE,'('' Lock received :'',a)') TIMEX(1:24)
            KOND = 0
            RETURN
          
         ELSEIF (ITRY .EQ. -1) THEN

C  Lock is in use, keep retrying at retry interval until max. wait
C   is exceeded or lock is granted.

            IF (WAITC .GE. MAXWAIT) THEN
              IF(UE.GE.0) WRITE(UE,'('' Lock file still in use after ''
     $                          ,I3,'' min. wait!'')') MAXT
              KOND = 1
              RETURN
            ELSE

C  Wait for a while (units are hundredths of seconds)

              WAITC = WAITC + WAIT
              CALL UWAIT(WAIT)
              GOTO 100
            ENDIF 

         ELSE 

C  Error in trying to establish lock

            KOND = 2
            IF(UE.GE.0) WRITE(UE,'('' Error in setting lock file !'')')
            IF(UE.GE.0) WRITE(UE,'(''    ITRY is: '',I4)') ITRY
            RETURN

         ENDIF 
   
      END
