C$PRAGMA C (GET_APPS_DEFAULTS)
C MODULE UPINIO
C  =====================================================================
C  pgm: UPINIO .. Initialize block common "/UPDAIO/" for up i/o routines
C
C  use:     CALL UPINIO()
C
C  rqd: FTN_STD_ERR, GET_APPS_DEFAULTS
C
C  cmt:        Common UPDAIO is the unit numbers used by the "UP..."
C  cmt:        routines input-output routines for system i/o.
C  cmt:        It also contains an array to hold record lengths for
C  cmt:        files that are open, else it holds -1's.
C  cmt:          UTR .. unit for terminal input
C  cmt:          UTW .. unit for terminal output
C  cmt:          UR ... unit for user input
C  cmt:          UW ... unit for user output
C  cmt:          UE ... unit for all i/o errors for system routines
C  cmt:          UU ... unit for all open, close, deletion, etc., of
C  cmt:                 file messages ... else set to -1
C  cmt:          UPRECL .. record lengths for the indexed unit,
C  cmt:                    else 0 for SAM file, else -1 not openned
C  =====================================================================
      SUBROUTINE UPINIO()
 
cc      EXTERNAL   FTN_STD_ERR, GET_APPS_DEFAULTS
      EXTERNAL   FTN_STD_ERR
cc AV pgf90 port 7/2/01      , GET_APPS_DEFAULTS
 
      INCLUDE 'updaio'
 
      INTEGER        FTN_STD_ERR,II,REPLEN,INITZ
      CHARACTER*128  REP
      CHARACTER*2    RON

      SAVE           INITZ
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/upinio.f,v $
     . $',                                                             '
     .$Id: upinio.f,v 1.4 2002/02/11 16:35:08 dws Exp $
     . $' /
C    ===================================================================
C

      DATA    INITZ / 0 /

C                         Skip this routine if it has been run before

        IF (INITZ .EQ. 0) THEN
          INITZ = 1

C                         Set system level i/o rt unit numbers
 
          UTR =  5
          UTW =  6
          UR  = UTR
          UW  = FTN_STD_ERR()
          UE  = -1
          UU  = -1
          MUPRECL = 256
 
C                         Initialize unit number record lengths used by
C                          DAIO routines (set in UPOPEN, UPCLOS, UPDELE)
 
          DO 80 II=1,MUPRECL
            UPRECL(II) = -1
   80       CONTINUE
 
C                         Look for environment "ofs_log_output" to
C                         see if it is "on".  If so, then output all
C                         i/o messages in the "up..." routines to the
C                         standard error output (ie. set UU = UW).
 
          CALL GET_APPS_DEFAULTS('ofs_log_output',14,REP,REPLEN)
            RON = REP(1:2)
            IF (RON.EQ.'on' .OR. RON.EQ.'ON' .OR. RON.EQ.'On') UU = UW

C                         Look for environment "ofs_error_output" to
C                         see if it is "on".  If so, then output all
C                         error messages in the "up..." routines to the
C                         standard error output (ie. set UE = UW).

          CALL GET_APPS_DEFAULTS('ofs_error_output',16,REP,REPLEN)
            RON = REP(1:2)
            IF (RON.EQ.'on' .OR. RON.EQ.'ON' .OR. RON.EQ.'On') UE = UW
 
          IF (UR .GT. 0) UPRECL(UR) = 0
          IF (UW .GT. 0) UPRECL(UW) = 0
          IF (UU .GT. 0) UPRECL(UU) = 0
          IF (UE .GT. 0) UPRECL(UE) = 0

        ENDIF

      RETURN
      END
