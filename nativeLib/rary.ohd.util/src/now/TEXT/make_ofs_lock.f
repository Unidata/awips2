C$PRAGMA C (GET_APPS_DEFAULTS)
C  =====================================================================
C  pgm: MAKE_OFS_LOCK .. Create the OFS file lock name 
C
C  use:     CALL MAKE_OFS_LOCK(LOCK_NAME,LOCK_LEN,KOND)
C
C  out: LOCK_NAME ..... OFS lock name - CHAR*(*)
C  out: LOCK_LEN ...... number of chars in the lock name - INT
C  out: KOND .......... status of lock name creation - INT
C  out:                   = 0 ... file name created
C  out:                   = 1 ... error in creating file name
C
C  =====================================================================
      SUBROUTINE MAKE_OFS_LOCK(LOCK_NAME,LOCK_LEN,KOND)

CC AV pgf90 7/2/01     EXTERNAL   GET_APPS_DEFAULTS

      CHARACTER*(*) LOCK_NAME
      INTEGER       LOCK_LEN,KOND

      CHARACTER*240 OFS_LEVEL,OFS_FILES
      CHARACTER*240 OFS_FS5FILES,FS5FILES
      INTEGER       LLV,LOF,LOF5,LFS5
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/make_ofs_lock.f,v $
     . $',                                                             '
     .$Id: make_ofs_lock.f,v 1.3 2002/02/11 13:20:44 michaelo Exp $
     . $' /
C    ===================================================================
C

      KOND = 0

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

            LOCK_NAME=OFS_FS5FILES(1:LOF5)
            LOCK_LEN = LOF5
            DO 10 I=1,LOF5
               IF (LOCK_NAME(I:I) .EQ. '/') THEN
                  LOCK_NAME(I:I) = '_'
                  ENDIF
 10            CONTINUE

         ELSE
            LOCK_NAME='ofs.'//OFS_LEVEL(1:LLV)
            LOCK_LEN = LLV+4

         ENDIF

C  That's it!

      RETURN
      END
