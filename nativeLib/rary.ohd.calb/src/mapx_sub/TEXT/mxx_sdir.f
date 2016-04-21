C$PRAGMA C (GET_APPS_DEFAULTS)
C MODULE MXX_SDIR
C  =====================================================================
C  pgm: MXX_SDIR .. Get pathname of the desired dir from a name-token
C
C  use:     CALL MXX_SDIR(NAM,TOKEN,DIR,ISTAT)
C
C   in: NAM ........ name of dir searched for (may be full pathname
C   in:              or just the directory name under the directory
C   in:              given by the apps-defaults input token) - CHAR*(*)
C   in: TOKEN ...... if "NAM" is not the found, then use "TOKEN" to get
C   in:              a default directory and append "/<NAM>" to it to
C   in:              make the desired dir , and check for its
C   in:              existence - CHAR*(*)
C  out: DIR ........ pathname of the resulting found directory, else
C  out:              set to blanks - CHAR*(*)
C  out: ISTAT ...... output pathname status: - INT
C  out:               -2 = directory found using apps-defaults token
C  out:               -1 = directory found using only the input path
C  out:                0 = dir NOT found, apps-token not used
C  out:                1 = dir NOT found, though apps-token dir found
C  out:                2 = dir NOT found, output dir string too short
C  out:                3 = dir NOT found, apps-token dir NOT found
C  out:                4 = dir NOT found, given token itself NOT found
C  out:                5 = dir NOT found, no input name given
C  out:                6 = dir NOT found, no input token given
C
C  rqd: KKLAST,GET_APPS_DEFAULTS,UPEXIS
C  =====================================================================
      SUBROUTINE MXX_SDIR(NAM,TOKEN,DIR,ISTAT)

      INTRINSIC      LEN
      INTEGER        LEN

      EXTERNAL       KKLAST,UPEXIS

      CHARACTER*(*)  NAM,TOKEN,DIR
      CHARACTER*256  DFLT
      INTEGER        ISTAT,LENNAM,LENTOK,LENDIR,LENDFL,IUN,JSTAT
      INTEGER        MAXDIR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxx_sdir.f,v $
     . $',                                                             '
     .$Id: mxx_sdir.f,v 1.2 2002/02/15 20:08:36 dws Exp $
     . $' /
C    ===================================================================
C

        DIR   = ' '
        ISTAT = 5

        LENNAM = KKLAST(0,NAM)
        MAXDIR = LEN(DIR)
        IF (LENNAM .GT. 0) THEN
          IUN = 3
          CALL UPEXIS(IUN,NAM(1:LENNAM),JSTAT)
          ISTAT = 0
          IF (JSTAT .EQ. 0) THEN
            ISTAT = 2
            IF (LENNAM .LE. MAXDIR) THEN
              ISTAT = -1
              DIR   = NAM(1:LENNAM)
            ENDIF
          ELSEIF (NAM(1:1) .NE. '/') THEN
            LENTOK = KKLAST(0,TOKEN)
            ISTAT = 6
            IF (LENTOK .GT. 0) THEN
              CALL GET_APPS_DEFAULTS(TOKEN,LENTOK,DFLT,LENDFL)
              ISTAT = 4
              IF (LENDFL .GT. 0) THEN
                CALL UPEXIS(IUN,DFLT(1:LENDFL),JSTAT)
                ISTAT = 3
                IF (JSTAT .EQ. 0) THEN
                  LENDIR = LENDFL+LENNAM+1
                  ISTAT = 2
                  IF (LENDIR .LE. MAXDIR) THEN
                    DIR = DFLT(1:LENDFL) // '/' // NAM(1:LENNAM)
                    CALL UPEXIS(IUN,DIR(1:LENDIR),JSTAT)
                    ISTAT = -2
                    IF (JSTAT .NE. 0) THEN
                      ISTAT = 1
                      DIR = ' '
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF

      RETURN
      END
