C$PRAGMA C (GET_APPS_DEFAULTS)
C  =====================================================================
C  pgm: HUNLOCKFILES .. Free the OFS locks necessary for the passed
C  pgm:                 in program name.  
C
C  use:     CALL HLOCKFILES(PROGNAME,KOND)
C
C   in: PROGNAME ..... Name of the program that is requesting the lock.
C   in:                It is cross referenced with a table of locks to
C   in:                request, and then HREQUEST_LOCKS is called for
C   in:                those locks.  Pass in 'GENERAL' if you want all
C   in:                locks requested for write permission.
C  out: KOND .......... status of lock release command - INT
C  out:                   = 0 ... lock file released
C  out:                   = 1 ... lock file release error
C
C  rqd: GET_APPS_DEFAULTS,MAKE_OFS_LOCK,LOCKON,KKPOS
C  =====================================================================
      SUBROUTINE HUNLOCKFILES(PROGNAME,KOND)
      
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
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/hunlockfiles.f,v $
     . $',                                                             '
     .$Id: hunlockfiles.f,v 1.3 2004/08/11 13:14:16 hank Exp $
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
          LOCK_IDS = 'PDB'
      ELSE IF (PROGNAME(IBBB:IEEE) .EQ. 'NONFCST') THEN
          LOCK_IDS = 'HCLUSER'
      ELSE IF (PROGNAME(IBBB:IEEE) .EQ. 'FCEXEC_NOFA') THEN
          LOCK_IDS = 'PRD FCESP HCLUSER'
      ELSE IF (PROGNAME(IBBB:IEEE) .EQ. 'FCEXEC_FA') THEN
          LOCK_IDS = 'PPP PRD FCESP HCLUSER'
      ELSE IF (PROGNAME(IBBB:IEEE) .EQ. 'FCST') THEN
          LOCK_IDS = 'HCLUSER'
      ELSE IF (PROGNAME(IBBB:IEEE) .EQ. 'ESP_BLEND') THEN
          LOCK_IDS = 'PRD FCESP HCLUSER'
      ELSE IF (PROGNAME(IBBB:IEEE) .EQ. 'ESP_NOBLEND') THEN
          LOCK_IDS = 'FCESP HCLUSER'
          
C The program name of "PREPROC" is used by teh FMAP, MAP, MAPX,
C MAT, and MAPE.
      ELSE IF (PROGNAME(IBBB:IEEE) .EQ. 'PREPROC') THEN
          LOCK_IDS = 'PDB PPP PRD HCLUSER'
          
      ELSE IF (PROGNAME(IBBB:IEEE) .EQ. 'SHEFPOST') THEN
          LOCK_IDS = 'PDB HCLUSER'
          
C The catch all.  If a user passes in "GENERAL" as the progname,
C this will catch it.
      ELSE
          LOCK_IDS = 'PDB PPP PRD FCESP HCLUSER'
      ENDIF
      
      CALL HRELEASE_LOCKS(LOCK_IDS,KOND)

      RETURN
      END
      
      

