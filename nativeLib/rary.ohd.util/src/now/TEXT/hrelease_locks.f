C$PRAGMA C (GET_APPS_DEFAULTS)
C  =====================================================================
C  pgm: FREE_OFS_LOCK .. Release the OFS file lock 
C
C  use:     CALL RELEASE_LOCKS(LOCK_IDS,KOND)
C
C   in: LOCK_ID ....... list of lock identifiers separated by spaces. 
C   in:                 Each item in list must be one of "PPP", "PDB", 
C   in:                 "PRD", "FCESP", or "HCLUSER".
C  out: KOND .......... status of lock release command - INT
C  out:                   = 0 ... lock file released
C  out:                   = 1 ... lock file release error
C
C  =====================================================================
      SUBROUTINE HRELEASE_LOCKS(LOCK_IDS,KOND)

      CHARACTER*(*)  LOCK_IDS
      INTEGER        KOND

      INTEGER IIDLEN
      CHARACTER*7 CURID
      INTEGER CURIDLEN
      INTEGER LASTID
      
      INCLUDE 'updaio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/hrelease_locks.f,v $
     . $',                                                             '
     .$Id: hrelease_locks.f,v 1.2 2004/08/11 13:12:10 hank Exp $
     . $' /
C    ===================================================================
C
      
      IIDLEN    = LEN(LOCK_IDS)
      CURIDLEN  = 0
      LASTID    = 0
      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C MAIN LOOP
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C     Generate a message for the request.
      CALL KKTRIM(LOCK_IDS,IBEG2,IEND2)
      IF(UE.GE.0) WRITE(UE,'(A,A,A,A,A)') 'Releasing locks: {',
     +   LOCK_IDS(IBEG2:IEND2),'}'
     
C     Acquire the next word from the LOCK_IDS array.
 10   CALL KKNXWD(LOCK_IDS, LASTID, IIDLEN, CURID, 7, CURIDLEN)
      
C     If the length is 0, we are done.
      IF(UE.GE.0) WRITE(UE,'(A,A)') '  LOCK TO RELEASE...',CURID
      IF (CURIDLEN.EQ.0) THEN
        KOND = 0
        GOTO 999
      ENDIF
      
C     Try to release the just found lock.
      CALL HFREE_OFS_LOCK(CURID,KOND)
      IF (KOND.NE.0) THEN
        KOND = 1
        GOTO 999
      ENDIF
      
C     Go to the next lock.
      GOTO 10
      
999   RETURN
      END
