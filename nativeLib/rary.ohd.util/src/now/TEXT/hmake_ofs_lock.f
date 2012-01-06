C$PRAGMA C (GET_APPS_DEFAULTS)
C  =====================================================================
C  pgm: MAKE_OFS_LOCK .. Create the OFS file lock name 
C
C  use:     CALL MAKE_OFS_LOCK(LOCK_NAME,LOCK_LEN,KOND)
C
C   in: LOCK_ID ....... An identifier for the lock name to create.  It
C   in:                 must be one of "PPP", "PDB", "PRD", "FCESP", or 
C   in:                 "HCLUSER".
C  out: LOCK_NAME ..... OFS lock name - CHAR*(*)
C  out: LOCK_LEN ...... number of chars in the lock name - INT
C  out: KOND .......... status of lock name creation - INT
C  out:                   = 0 ... file name created
C  out:                   = 1 ... error in creating file name
C
C  =====================================================================
      SUBROUTINE HMAKE_OFS_LOCK(LOCK_ID,LOCK_NAME,LOCK_LEN,KOND)

CC AV pgf90 7/2/01     EXTERNAL   GET_APPS_DEFAULTS

      CHARACTER*(*) LOCK_NAME
      INTEGER       LOCK_LEN,KOND
      CHARACTER*7   LOCK_ID

      CHARACTER*240 OFS_LEVEL,OFS_FILES
      CHARACTER*240 OFS_FS5FILES,FS5FILES
      INTEGER       LLV,LOF,LOF5,LFS5
      INTEGER       IEEE
      INTEGER       LENID
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/hmake_ofs_lock.f,v $
     . $',                                                             '
     .$Id: hmake_ofs_lock.f,v 1.2 2004/08/11 13:11:54 hank Exp $
     . $' /
C    ===================================================================
C
      KOND = 0
      
C  Find the last non-blank character in the LOCK_ID and record as IEEE.
      LENID=LEN(LOCK_ID)
      IEEE = 0
 5    IEEE = IEEE + 1
      IF (IEEE.GT.LENID) GOTO 10
      IF (LOCK_ID(IEEE:IEEE).NE.' ') GOTO 5

C  Back up 1 in order to point at the last character and not the first
C  ' '.
 10   IEEE = IEEE - 1
      
C  Verify the lock identifier.
      IF(LOCK_ID(1:IEEE).NE.'PPP'.AND.
     +   LOCK_ID(1:IEEE).NE.'PDB'.AND.
     +   LOCK_ID(1:IEEE).NE.'PRD'.AND.
     +   LOCK_ID(1:IEEE).NE.'FCESP'.AND.
     +   LOCK_ID(1:IEEE).NE.'HCLUSER')  THEN
        KOND = 1
        GOTO 999
      ENDIF
      
C  Get info to construct OFS fs5files directory from standard
C   components and compare to that returned by the token 'ofs_fs5files'
C   If the same, construct lock name as ofs.'ofs_level'.  If not,
C   construct the lock name by using the fs5files path, substituting
C   "_" for every "/".

      CALL GET_APPS_DEFAULTS('ofs_files',9,OFS_FILES,LOF)
      CALL GET_APPS_DEFAULTS('ofs_level',9,OFS_LEVEL,LLV)

      FS5FILES = OFS_FILES(1:LOF)//'/'//OFS_LEVEL(1:LLV)//'/fs5files'
      LFS5 = LOF+LLV+10

      CALL GET_APPS_DEFAULTS('ofs_fs5files',12,OFS_FS5FILES,LOF5)

C  Construct the lock file name

      IF (LFS5.NE.LOF5 .OR. 
     .   INDEX(FS5FILES(1:LFS5),OFS_FS5FILES(1:LOF5)) .EQ. 0) THEN

            LOCK_NAME=OFS_FS5FILES(1:LOF5)//'_'//LOCK_ID(1:IEEE)
            LOCK_LEN = LOF5 + IEEE + 1
            DO 20 I=1,LOF5
               IF (LOCK_NAME(I:I) .EQ. '/') THEN
                  LOCK_NAME(I:I) = '_'
                  ENDIF
 20            CONTINUE

         ELSE
            LOCK_NAME='ofs.'//OFS_LEVEL(1:LLV)//'_'//
     +          LOCK_ID(1:IEEE)
            LOCK_LEN = LLV+4 + IEEE + 1

         ENDIF

C  That's it!

999   RETURN
      END
