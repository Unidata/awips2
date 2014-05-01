C MODULE MXX_PATH
C  =====================================================================
C  pgm: MXX_PATH .. Make pathnames for mapx input and output files
C  =====================================================================
      SUBROUTINE MXX_PATH(GRD_DIR,FLMISS,DIR_MAP,XTIME,ISTAT)

      INTRINSIC       LEN
      INTEGER         LEN

      CHARACTER*(*)   GRD_DIR
      CHARACTER*(*)   FLMISS,DIR_MAP
      CHARACTER*256   TMPDIR
      CHARACTER*200   LIN
      CHARACTER*8     XTIME
      CHARACTER*3     KHSTR1
      CHARACTER*2     KHSTR2,KHSTR3
      CHARACTER*45    MSG2
      CHARACTER*10    LOGNAM
      INTEGER         ISTAT,JSTAT,LENDM,MAXMIS,LMIS
      INTEGER         KJ1,KY1,KH1,KN1,KS1
      INTEGER         IUN,KKLAST,JE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxx_path.f,v $
     . $',                                                             '
     .$Id: mxx_path.f,v 1.1 2001/06/13 09:26:29 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA  MSG2 / '          date extension added to filename:  ' /
      DATA  LOGNAM / 'MISSED.log' /

        ISTAT = 0

C  GET PATH TO NEXRAD GRID FILES & GENERATE FULL PATH

        CALL MXX_SDIR(GRD_DIR,'calb_griddb',TMPDIR,JSTAT)
        CALL MXX_SDER(GRD_DIR,'calb_griddb',JSTAT)
        GRD_DIR = TMPDIR
        IF (JSTAT .GE. 0) ISTAT = 1

C  GET DEFAULT PATH TO MAPX OUTPUT FILES & GENERATE FULL PATH

        CALL MXX_SDIR(DIR_MAP,'calb_area_ts_dir',TMPDIR,JSTAT)
        CALL MXX_SDER(DIR_MAP,'calb_area_ts_dir',JSTAT)
        DIR_MAP = TMPDIR
        IF (JSTAT .GE. 0) ISTAT = 2

C  GET CURRENT TIME TO INCLUDE IN MISSED/MAPX FILE PASS

        FLMISS = ' '
        IF (ISTAT .EQ. 0) THEN
          LENDM  = KKLAST(0,DIR_MAP)
          MAXMIS = LEN(FLMISS)
          IF (LENDM.GT.0 .AND. LENDM+11.LE.MAXMIS) THEN
            FLMISS = DIR_MAP(1:LENDM) // '/' // LOGNAM

            IUN = 3
            CALL UPEXIS(IUN,FLMISS,JSTAT)
            IF (JSTAT .EQ. 0) THEN
              LENDM = LENDM+11
              IF (LENDM+9 .LE. MAXMIS) THEN
                FLMISS = FLMISS(1:LENDM) // '_' // XTIME
                LMIS = KKLAST(1,FLMISS)
                CALL MXX_WCEX( LOGNAM, XTIME, FLMISS(1:LMIS) )
              ENDIF
            ENDIF
          ELSE
            ISTAT = 3
CC          IF (MAXMIS .GE. 11) FLMISS = LOGNAM
          ENDIF
        ENDIF
       
      RETURN
      END
