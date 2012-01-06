C$PRAGMA C (GET_APPS_DEFAULTS)
C  =====================================================================
C  pgm: REQUEST_LOCKS .. Establish the OFS locks that are requested.
C  pgm:                  If anyone of the locks cannot be established,
C  pgm:                  then none of the locks will be created.  
C
C  use:     CALL REQUEST_LOCKS(LOCK_TYPES,LOCK_IDS,KOND)
C
C   in: LOCK_TYPES ..... Space separated list of lock types, each item
C   in:                  in the list being "read" or "write".  Only the
C   in:                  first letter of each item is important.  Total
C   in:                  string length cannot exceed 128 characters. 
C   in: LOCK_IDS ....... Identifiers for each of the locks.  Each
C   in:                  identifier must be either "PPPPPP", "FCESP", or 
C   in:                  "HCLUSER".  Total string length cannot exceed
C   in:                  128 characters.
C  out: KOND .......... status of lock open command - INT
C  out:                   = 0 ... lock files opened
C  out:                   = 1 ... could not open all lock files after
C  out:                           a max number of passes.
C  out:                   = 2 ... invalid arguments passed in.  Check
C  out:                           the number of types and ids... they
C  out:                           must be identical.
C
C  rqd: GET_APPS_DEFAULTS,MAKE_OFS_LOCK,LOCKON,KKPOS
C  =====================================================================
      SUBROUTINE HREQUEST_LOCKS(LOCK_TYPES,LOCK_IDS,KOND)

CC      EXTERNAL SET_OFS_LOCK
      CHARACTER*(*)  LOCK_TYPES
      CHARACTER*(*)  LOCK_IDS
      INTEGER        KOND
      
      INTEGER ITYPELEN,IIDLEN
      INTEGER I,J
      INTEGER LASTTYPE,LASTID
      
      CHARACTER*5 CURTYPE
      INTEGER CURTYPELEN
      CHARACTER*7 CURID
      INTEGER CURIDLEN
      INTEGER LOCKSACQ
      CHARACTER*5    STRNG
      INTEGER INTWT,NOFC
      INTEGER MAXPASS,CURPASS
      INTEGER IBEG1,IEND1
      INTEGER IBEG2,IEND2
      
      INCLUDE 'updaio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/hrequest_locks.f,v $
     . $',                                                             '
     .$Id: hrequest_locks.f,v 1.1 2004/06/01 18:01:51 hank Exp $
     . $' /
C    ===================================================================
C
C
C
C     Initialize lengths, locks acquired, and type/id counter.
      ITYPELEN  = LEN(LOCK_TYPES)
      IIDLEN    = LEN(LOCK_IDS)
      CURTYPELEN= 0
      CURIDLEN  = 0
      LOCKSACQ  = 0
      LASTTYPE  = 0
      LASTID    = 0
      CURPASS   = 1

C     Get the wait interval and record it in INTWT.
      CALL GET_APPS_DEFAULTS('ofs_lock_wait_interval',22,STRNG,NOFC)
      CALL KKPOS(STRNG(1:NOFC),INTWT)
      IF (INTWT.LE.0 .OR. INTWT.GT.300) INTWT = 5

C     Get the maximum passes and record it in MAXPASS.
      CALL GET_APPS_DEFAULTS('ofs_locks_max_pass',18,STRNG,NOFC)
      CALL KKPOS(STRNG(1:NOFC),MAXPASS)
      IF (MAXPASS.LE.0 .OR. MAXPASS.GT.300) MAXPASS = 4

C     Generate a message for the request.
      CALL KKTRIM(LOCK_TYPES,IBEG1,IEND1)
      CALL KKTRIM(LOCK_IDS,IBEG2,IEND2)
      IF(UE.GE.0) WRITE(UE,'(A,A,A,A,A)') 'Requesting locks: {',
     +   LOCK_IDS(IBEG2:IEND2),'}, {',LOCK_TYPES(IBEG1:IEND1),'}'

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C MAIN LOOP
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C     Acquire the next word from both LOCK_TYPES and LOCK_IDS
C     arrays.
 10   CALL KKNXWD(LOCK_TYPES, LASTTYPE, ITYPELEN, CURTYPE, 
     +            5, CURTYPELEN)
      CALL KKNXWD(LOCK_IDS, LASTID, IIDLEN, CURID, 7, CURIDLEN)
      
C     If either word has a length of 0, then we are at the end of
C     that list.  If one has length 0 and the other does not, then
C     something weird happened.
      IF (CURIDLEN.EQ.0.OR.CURTYPELEN.EQ.0) THEN
      
C       In this case, we have a valid end.  So, set the KOND to 0
C       and goto the end.
        IF (CURIDLEN.EQ.0.AND.CURTYPELEN.EQ.0) THEN
          KOND = 0
          GOTO 999
        ENDIF
      
C       So, one of the two is 0 but the other is not.  Free all 
C       locks...
        LASTID = 1
        DO 20 I = 1,LOCKSACQ
          CALL KKNXWD(LOCK_IDS, LASTID, IIDLEN, CURID, 7, CURIDLEN)
          CALL HFREE_OFS_LOCK(CURID, KOND)
 20     CONTINUE
        LOCKSACQ = 0
 
C       .. and generate an error message followed by a return.
        IF(UE.GE.0) WRITE(UE,'(''Invalid locks request.  The''
     +      '' number of types and ids do not match!'')')
        KOND = 2
        GOTO 999
      ENDIF     

C     Try to acquire a lock for the given type and id.      
      CALL HSET_OFS_LOCK(CURTYPE,CURID,KOND)
      
C     If I fail to acquire the lock, then I must free the previous
C     locks and wait.
      IF (KOND.NE.0) THEN

C       For each lock that was acquired, get its id and free it.
        LASTID = 0
        DO 30 I = 1,LOCKSACQ
          CALL KKNXWD(LOCK_IDS, LASTID, IIDLEN, CURID, 7, CURIDLEN)
          CALL HFREE_OFS_LOCK(CURID, KOND)
 30     CONTINUE
        LOCKSACQ = 0

C       Reset the next word loops.
        LASTTYPE = 0
        LASTID = 0

C       Check on the max-passes condition.
        CURPASS = CURPASS + 1
        IF (CURPASS.GT.MAXPASS) THEN
              IF(UE.GE.0) WRITE(UE,'(''Could not acquire locks after ''
     +                          ,I3,'' passes!'')') MAXPASS
              KOND = 1
              GOTO 999
        ENDIF
 
C       Wait the appropriate amount of time.
        CALL UWAIT(WAIT)
        IF(UE.GE.0) WRITE(UE,'(''Requesting locks again...'')')
        
C     KOND is 0, so increment the locks acquired by one.
      ELSE
         LOCKSACQ = LOCKSACQ + 1
      ENDIF

C     Return to the top of the main loop.
      GOTO 10
      
999   RETURN
      END
