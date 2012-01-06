C$PRAGMA C (GET_APPS_DEFAULTS)
C MODULE MXL_PATH
C  =====================================================================
C  pgm: MXL_PATH .. Get pathnames of ll_pairs and line_segs files
C
C  use:     CALL MXL_PATH(LLRUN,NAMLLG,PTHLLG,PTHHRP,IERR,ISTAT)
C
C   in: LLRUN .... control code for obtaining latlon/hrap files and
C   in:            what to compute (4 to 8 causes these files to be
C   in:            obtained from "ofs" files so they need not exist)
C   in: NAMLLG ... name (or pathname) of input ll_grid file to be
C   in:            found (must not begin with a blank) - CHAR*(*)
C  out: PTHLLG ... pathname of found ll_grid file found, else all
C  out:            blanks; list of rules to find file: - CHAR*(*)
C  out:              1) look for exact <name> as pathname or in cur dir
C  out:              2) look for <name>.latlon as pathname or in cur dir
C  out:              3) look for 'calb_param'/ll_pairs/<exact name>
C  out:              4) look for 'calb_param'/ll_pairs/<name>.latlon
C  out: PTHHRP ... pathname of line_segments file to be created; note
C  out:            that if a file <name>.hrap exists in the cur dir,
C  out:            it will be used as the output pathname - CHAR*(*)
C  out:              1) set to <name>.hrap in cur dir if it already
C  out:                 exists
C  out:              2) set to 'calb_param'/line_segs/<name>.hrap
C  out: IERR ..... error status of routine: - INT
C  out:              -4 = no err, neither file exists nor is needed
C  out:              -3 = no err, only ouput file, PTHHRP, found locally
C  out:              -2 = no err, but file PTHHRP already exists
C  out:              -1 = no err, only ouput file, PTHHRP, found
C  out:               0 = no err, only input file, PTHLLG, found
C  out:               1 = error, neither file exists
C  out:               2 = error, a name length is too long
C  out:               3 = error, given name, NAMLLG, begins with blank
C  out:               4 = error, bad input file name (ex. all slashes)
C  out:               5 = error, bad unit number, IUN, given
C  out: ISTAT .... output status of routine: - INT
C  out:               0 = no error
C  out:               1 = error
C
C  rqd: GET_APPS_DEFAULTS,UPEXIS
C
C  cmt: Limit of 128 on lenth of interal strings.
C  =====================================================================
      SUBROUTINE MXL_PATH(LLRUN,NAMLLG,PTHLLG,PTHHRP,IERR,ISTAT)

      INTRINSIC      LEN
      INTEGER        LEN

      CHARACTER*(*)  NAMLLG,PTHLLG,PTHHRP
      CHARACTER*128  PTHTMP,PTHPAR,NAMFIL
      CHARACTER*7    SUFL
      CHARACTER*5    SUFH
      CHARACTER*8    DIRL
      CHARACTER*9    DIRH
      CHARACTER*1    KHR
      INTEGER        LLRUN,IERR,IUN,ISTAT
      INTEGER        JERR,JJ,KK,MAXLLG,MAXHRP,ISKIP
      INTEGER        LENNAM,LENTMP,LENPAR,LENSLA,LENSUB,LENFIL
      INTEGER        LNSUFL,LNSUFH,LNDIRL,LNDIRH
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxl_path.f,v $
     . $',                                                             '
     .$Id: mxl_path.f,v 1.2 2002/02/15 20:08:13 dws Exp $
     . $' /
C    ===================================================================
C

      DATA    SUFL / '.latlon'  /
      DATA    SUFH / '.hrap' /
      DATA    DIRL / 'll_pairs' /
      DATA    DIRH / 'line_segs' /

      IERR = 0
      IUN   = 3

      LNSUFL = LEN(SUFL)
      LNSUFH = LEN(SUFH)
      LNDIRL = LEN(DIRL)
      LNDIRH = LEN(DIRH)

      PTHLLG = ' '
      PTHHRP = ' '

      MAXLLG = LEN(PTHLLG)
      MAXHRP = LEN(PTHHRP)
      IF (MAXLLG .GT. 128) MAXLLG = 128
      IF (MAXHRP .GT. 128) MAXHRP = 128

      IF (IUN.LT.1 .OR. IUN.GT.99) THEN
        IERR = 5
        GOTO 999
      ENDIF

      JJ     = LEN(NAMLLG)
      LENNAM = 0
  140 IF (JJ .LE. LENNAM) GOTO 150
        KHR = NAMLLG(JJ:JJ)
        IF (KHR.NE.' ' .AND. KHR.NE.'/') LENNAM = JJ
        JJ = JJ-1
        GOTO 140
  150 CONTINUE
      KK = 0
      JJ = LENNAM
  160 IF (JJ .LE. KK) GOTO 170
        KHR = NAMLLG(JJ:JJ)
        IF (KHR.NE.' ' .AND. KHR.NE.'/' .AND. KHR.NE.'.') KK = JJ
        JJ = JJ-1
        GOTO 160
  170 CONTINUE
      IF (KK .EQ. 0) THEN
        IERR = 4
        GOTO 999
      ENDIF

      IF (NAMLLG(1:1) .EQ. ' ') THEN
        IERR = 3
        GOTO 999
      ENDIF

      PTHTMP = NAMLLG(1:LENNAM)
      LENTMP = LENNAM
      CALL UPEXIS(IUN,PTHTMP(1:LENTMP),JERR)
      IF (JERR .EQ. 0) GOTO 190
      IF (LENTMP+4 .LE. MAXLLG) THEN
        ISKIP = 0
        IF (LENTMP-LNSUFL+1 .GT. 0) THEN
          IF (PTHTMP(LENTMP-LNSUFL+1:LENTMP) .EQ. SUFL) THEN
            ISKIP = 1
          ENDIF
        ENDIF
        IF (ISKIP.EQ.0 .AND. LENTMP-LNSUFH+1.GT.0) THEN
          IF (PTHTMP(LENTMP-LNSUFH+1:LENTMP) .EQ. SUFH) THEN
            ISKIP = 1
          ENDIF
        ENDIF
        IF (ISKIP .EQ. 0) THEN
            PTHTMP = PTHTMP(1:LENTMP) // SUFL
            LENTMP = LENTMP+LNSUFL
            CALL UPEXIS(IUN,PTHTMP(1:LENTMP),JERR)
            IF (JERR .EQ. 0) GOTO 190
        ENDIF
      ENDIF

      IF (NAMLLG(1:1) .NE. '/') THEN
        CALL GET_APPS_DEFAULTS('calb_param',10,PTHPAR,LENPAR)
        IF (LENPAR.GT.0 .AND. LENPAR+LNDIRL+1.LT.128) THEN
          PTHPAR = PTHPAR(1:LENPAR) // '/' // DIRL
          LENPAR = LENPAR+LNDIRL+1
        ENDIF
        IF (LENPAR .GT. 0) THEN
          IF (LENPAR+LENNAM .GT. MAXLLG) GOTO 995
          PTHTMP = PTHPAR(1:LENPAR) // '/' // NAMLLG(1:LENNAM)
          LENTMP = LENPAR+LENNAM+1
          CALL UPEXIS(IUN,PTHTMP(1:LENTMP),JERR)
          IF (JERR .EQ. 0) GOTO 190
          IF (LENTMP+LNSUFL .LE. MAXLLG) THEN
            ISKIP = 0
            IF (LENTMP-LNSUFL+1 .GT. 0) THEN
              IF (PTHTMP(LENTMP-LNSUFL+1:LENTMP) .EQ. SUFL) THEN
                ISKIP = 1
              ENDIF
            ENDIF
            IF (ISKIP.EQ.0 .AND. LENTMP-LNSUFH+1.GT.0) THEN
              IF (PTHTMP(LENTMP-LNSUFH+1:LENTMP) .EQ. SUFH) THEN
                ISKIP = 1
              ENDIF
            ENDIF
            IF (ISKIP .EQ. 0) THEN
                PTHTMP = PTHTMP(1:LENTMP) // SUFL
                LENTMP = LENTMP+LNSUFL
                CALL UPEXIS(IUN,PTHTMP(1:LENTMP),JERR)
                IF (JERR .EQ. 0) GOTO 190
            ENDIF
          ENDIF
        ENDIF
      ENDIF

      IERR = -3

  190 CONTINUE
      PTHLLG = PTHTMP(1:LENTMP)

C ===

      JJ = LENTMP
      KK = 0
  210 IF (JJ .LE. KK) GOTO 220
        IF (PTHTMP(JJ:JJ) .EQ. '.') KK = JJ
        JJ = JJ-1
        GOTO 210
  220 CONTINUE
      IF (KK .GT. 1) LENTMP = KK-1

      JJ = LENTMP
      KK = 0
  230 IF (JJ .LE. KK) GOTO 240
        IF (PTHTMP(JJ:JJ) .EQ. '/') KK = JJ
        JJ = JJ-1
        GOTO 230
  240 CONTINUE
      LENSLA = 0
      IF (KK .GT. 0) LENSLA = KK

      LENFIL = LENTMP-LENSLA+5
      IF (LENFIL .GT. MAXHRP) GOTO 995
      ISKIP = 0
      IF (LENTMP-LNSUFL+1 .GT. 0) THEN
        IF (PTHTMP(LENTMP-LNSUFL+1:LENTMP) .EQ. SUFL) THEN
          ISKIP = 1
        ENDIF
      ENDIF
      IF (ISKIP.EQ.0 .AND. LENTMP-LNSUFH+1.GT.0) THEN
        IF (PTHTMP(LENTMP-LNSUFH+1:LENTMP) .EQ. SUFH) THEN
          ISKIP = 1
        ENDIF
      ENDIF
      IF (ISKIP .EQ. 0) THEN
          NAMFIL = PTHTMP(LENSLA+1:LENTMP) // SUFH

          IF (LENSLA .EQ. 0) THEN
            PTHTMP = NAMFIL
            LENTMP = LENFIL
            GOTO 290
          ELSE
CC get next dir and see if it is ll_pairs, if so then sub with line_segs
            CALL UPEXIS(IUN,NAMFIL(1:LENFIL),JERR)
            IF (JERR .EQ. 0) THEN
              PTHHRP = NAMFIL(1:LENFIL)
              IF (IERR .EQ. 0) THEN
                IERR = -2
              ELSE
                IERR = -3
              ENDIF
              GOTO 999
            ELSE
              LENSUB = LENSLA-LNDIRL
              IF (LENSUB .GT. 0) THEN
                IF (PTHTMP(LENSUB:LENSLA) .EQ. DIRL // '/') THEN
                  IF (LENSUB+LNDIRH-1+LENFIL .GT. MAXHRP) GOTO 995
                  PTHTMP = PTHTMP(1:LENSUB-1) // DIRH // '/'
                  PTHTMP = PTHTMP(1:LENSUB+LNDIRH) // NAMFIL(1:LENFIL)
                  LENTMP = LENSUB+LNDIRH+LENFIL
                  GOTO 290
                ELSE
                  IF (LENSLA+LENFIL .GT. MAXHRP) GOTO 995
                  PTHTMP = PTHTMP(1:LENSLA) // NAMFIL(1:LENFIL)
                  LENTMP = LENSLA+LENFIL
                  GOTO 290
                ENDIF
              ELSE
                IF (LENSLA+LENFIL .GT. MAXHRP) GOTO 995
                PTHTMP = PTHTMP(1:LENSLA) // NAMFIL(1:LENFIL)
                LENTMP = LENSLA+LENFIL
                GOTO 290
              ENDIF
            ENDIF
          ENDIF
      ENDIF

  290 CALL UPEXIS(IUN,PTHTMP(1:LENTMP),JERR)
      IF (JERR .NE. 0) THEN
        IF (IERR .EQ. -3) THEN
          IF (LLRUN.GE.4 .AND. LLRUN.LE.8) THEN
            IERR = -4
          ELSE
            IERR = 1
          ENDIF
        ENDIF
      ELSE
        IF (IERR .EQ. 0) THEN
          IERR = -2
        ELSE
          IERR = -1
        ENDIF
      ENDIF
      PTHHRP = PTHTMP(1:LENTMP)
      GOTO 999

  995 IERR = 2

  999 CONTINUE
      ISTAT = 0
      IF (IERR .GT. 0) ISTAT = 1

      RETURN
      END
