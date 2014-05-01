C$PRAGMA C (DATIM2)
C$PRAGMA C (LOCKFREE)
C  =====================================================================
C  pgm: LOCKOFF .. Release read/write lock
C
C  use:     CALL LOCKOFF(DIR,DLEN,NAME,NLEN,KOND)
C
C   in: DIR ........... directory for lock files - CHAR*(*)
C   in: DLEN .......... number of chars in "DIR" - INT
C   in: NAME .......... lock name - CHAR*(*)
C   in: NLEN .......... number of chars in "NAME" - INT
C  out: KOND .......... status of lock release command - INT
C  out:                   = 0 ... lock file released
C  out:                   = 1 ... lock file release error
C
C  =====================================================================
      SUBROUTINE LOCKOFF(DIR,DLEN,NAME,NLEN,KOND)

CC      EXTERNAL   LOCKFREE
CC AV pgf90 7/2/01      EXTERNAL   DATIM2,LOCKFREE

      CHARACTER*(*) DIR,NAME
      INTEGER       DLEN,NLEN,KOND

      CHARACTER*28  TIMEX
      CHARACTER*240 FILE
      INTEGER       ITRY
      INTEGER       LOCKFREE,LLF

      INCLUDE 'updaio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/lockoff.f,v $
     . $',                                                             '
     .$Id: lockoff.f,v 1.5 2004/06/23 13:37:48 hank Exp $
     . $' /
C    ===================================================================
C

      DATA FILE/' '/

C   Establish complete lock file path name

      FILE = DIR(1:DLEN) // '/' // NAME(1:NLEN)
      LLF = DLEN + NLEN + 1

      IF(UE.GE.0) WRITE(UE,'()')

C  Try to release lock file

 100    CONTINUE 
        ITRY = LOCKFREE(FILE(1:LLF),LLF)

C  Check on success of releasing lock

         IF (ITRY .EQ. 0) THEN

C  Lock is released

            CALL DATIM2(TIMEX)
            IF(UE.GE.0) WRITE(UE,'('' Lock released :'',a)') TIMEX(1:24)
            KOND = 0
            RETURN
          
         ELSEIF (ITRY .EQ. -1) THEN

C  Error in releasing lock

CHDH  Change made by Hank Herr (2004-06-23).  When an error occurs in
CHDH  releasing a lock, return with a condition of 0.  Here is the
CHDH  reason: LOCKFREE will only return an error (-1) if lock to be
CHDH  freed was never created.  However, this is not necessarily a bad
CHDH  thing, particularly with the new 3-lock system where we may 
CHDH  want to free all held locks not knowing which locks are actually
CHDH  held.  Hence, we no longer consider a failed LOCKFREE to be bad.
c         KOND = 1
           KOND = 0
c         IF(UE.GE.0) WRITE(UE,'('' Error in releasing lock file !'')')
           RETURN

         ENDIF 
   
      END
