C$PRAGMA C (GET_APPS_DEFAULTS)
C  =====================================================================
C  pgm: HLOCKFILES .. Establish the OFS locks necessary for the passed
C  pgm:              in program name.  
C
C  use:     CALL HLOCKFILES(PROGNAME,KOND)
C
C   in: PROGNAME ..... Name of the program that is requesting the lock.
C   in:                It is cross referenced with a table of locks to
C   in:                request, and then HREQUEST_LOCKS is called for
C   in:                those locks.  Pass in 'GENERAL' if you want all
C   in:                locks requested for write permission.
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
      SUBROUTINE HLOCKFILES(PROGNAME,KOND)
      
      CHARACTER*(*)  PROGNAME
      INTEGER        KOND
      
      CHARACTER*1    BLANK
      CHARACTER*48   LOCK_TYPES
      CHARACTER*48   LOCK_IDS
      
      INTEGER        IBBB,IEEE,ILLL
      
      PARAMETER      ( BLANK=' ' )
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/hlockfiles.f,v $
     . $',                                                             '
     .$Id: hlockfiles.f,v 1.4 2004/09/21 16:00:20 hank Exp $
     . $' /
C    ===================================================================
C
      
C Get the length of PROGNAME and the last non-blank character
C as IEEE.
      ILLL=LEN(PROGNAME)
      IEEE = ILLL+1
  100 IEEE = IEEE-1
      IF( IEEE .LE. 1 ) GO TO 110
      IF( PROGNAME(IEEE:IEEE) .EQ. BLANK ) GO TO 100
  110 CONTINUE
            
C Get the first non-blank character in PROGNAME as IBBB.

      IBBB = 0
  120 IBBB = IBBB+1
      IF( IBBB .GE. IEEE ) GO TO 130
      IF( PROGNAME(IBBB:IBBB) .EQ. BLANK ) GO TO 120
  130 CONTINUE
      
C This is one big if clause.  Most conditions will result in setting
C only the LOCK_TYPES variable, but some may reset the LOCK_IDS to
C include only some of the locks; not all of them.

      IF (PROGNAME(IBBB:IEEE) .EQ. 'BATCHPST') THEN
          LOCK_IDS   = 'PDB'
          LOCK_TYPES = 'write'

C An fcst (NON-FCST) run will only need the HCLUSER files.
      ELSE IF (PROGNAME(IBBB:IEEE) .EQ. 'NONFCST') THEN
          LOCK_IDS   = 'HCLUSER'
          LOCK_TYPES = 'write'
          
C Breaking apart the FCEXEC_NOFA from FCEXEC_FA will only be useful
C in the future when the PPPPPP db is broken down into more pieces.
C The non FFG or ASSIM run will not require PPP files... only
C the PRD files.  The "FCEXEC" lock below will need to be removed
C eventually.
      ELSE IF (PROGNAME(IBBB:IEEE) .EQ. 'FCEXEC_NOFA') THEN
          LOCK_IDS   = 'PRD   FCESP HCLUSER'
          LOCK_TYPES = 'write write read'
      ELSE IF (PROGNAME(IBBB:IEEE) .EQ. 'FCEXEC_FA') THEN
          LOCK_IDS   = 'PPP  PRD    FCESP HCLUSER'
          LOCK_TYPES = 'read write  write read'
      ELSE IF (PROGNAME(IBBB:IEEE) .EQ. 'FCST') THEN
          LOCK_IDS   = 'HCLUSER'
          LOCK_TYPES = 'read'

C Running ESP without blend does not require the PPPPPP files, while
C a run with blend does.
      ELSE IF (PROGNAME(IBBB:IEEE) .EQ. 'ESP_BLEND') THEN
          LOCK_IDS   = 'PRD  FCESP HCLUSER'
          LOCK_TYPES = 'read write read'
      ELSE IF (PROGNAME(IBBB:IEEE) .EQ. 'ESP_NOBLEND') THEN
          LOCK_IDS   = 'FCESP HCLUSER'
          LOCK_TYPES = 'write read'
          
C The program name of "PREPROC" is used by teh FMAP, MAP, MAPX,
C MAT, and MAPE.
      ELSE IF (PROGNAME(IBBB:IEEE) .EQ. 'PREPROC') THEN
          LOCK_IDS   = 'PDB  PPP  PRD   HCLUSER'
          LOCK_TYPES = 'write read write read'
          
      ELSE IF (PROGNAME(IBBB:IEEE) .EQ. 'SHEFPOST') THEN
          LOCK_IDS   = 'PDB   HCLUSER'
          LOCK_TYPES = 'write read'
          
CHDH We will leave ppinit and fcinit out of it, for now.  These
CHDH are not sources of slow downs for the system.          
C      ELSE IF (PROGNAME(IBBB:IEEE) .EQ. 'PPINIT') THEN
C          LOCK_IDS   = 'PPPPPP FCESP HCLUSER'
C          LOCK_TYPES = 'write  read read'
C          
C      ELSE IF (PROGNAME(IBBB:IEEE) .EQ. 'FCINIT') THEN
C          LOCK_IDS   = 'PPPPPP FCESP HCLUSER'
C          LOCK_TYPES = 'write  write read'

C All other programs will be assumed to need write locks to all
C three dbs.  This will also catch the 'GENERAL' lock.
      ELSE 
          LOCK_IDS   = 'PDB   PPP   PRD   FCESP HCLUSER'
          LOCK_TYPES = 'write write write write write'
      ENDIF
      
      CALL HREQUEST_LOCKS(LOCK_TYPES,LOCK_IDS,KOND)

      RETURN
      END
      
      

