C  =====================================================================
      SUBROUTINE MXL_READ(IUN,BASINS,NBASN,NXBAS,
     $                    MX,FLAT,FLON,NUMLL,AREA,UNIT,IERR)

      INTEGER        IUN,NBASN,NUMLL,IERR,MX
      CHARACTER*8    BASINS(*),NXBAS
      CHARACTER*4    UNIT
      CHARACTER*14   WLAT,WLON
      REAL           FLAT(MX),FLON(MX),FNLAT,FNLON,AREA
      INTEGER        IELAT,IELON

      CHARACTER*200  LINE
      CHARACTER*80   WORD
      CHARACTER*1    KH,KFOU
      CHARACTER      D1*2, D2*1, D3*3
      INTEGER        LERR,IEND,INEX,IFOU,LWORD,LWLAT,LWLON

      SAVE           LINE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxl_read.f,v $
     . $',                                                             '
     .$Id: mxl_read.f,v 1.1 2001/06/13 09:22:40 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA    D1,D2,D3 / '''"',  ' ',  ',()' /

        NUMLL = 0
        AREA  = 0.0
        UNIT  = ' '

        LERR = 0
        IERR = 0
        IFOU = 0
   20   IF (LERR.NE.0 .OR. IFOU.NE.0) GOTO 50

C                   Look for "BASN" word to start next basin

          WORD = ' '
          KFOU = ' '
   30     IF (LERR.NE.0 .OR. (WORD.EQ.'BASN' .AND. KFOU.EQ.'(')) GOTO 40
            CALL KKNXR(IUN,LINE,WORD,LWORD,KFOU,LERR,D1,D2,D3)
            GOTO 30
   40     CONTINUE

          IF (LERR .EQ. 0) THEN
            NXBAS = ' '

C                   Get units type, METR or ENGL

            CALL KKNXR(IUN,LINE,UNIT,LWORD,KFOU,LERR,D1,D2,D3)
            IF (UNIT .EQ. 'XXXX') UNIT = 'ENGL'
            IF (UNIT.NE.'ENGL' .AND. UNIT.NE.'METR') IERR = 6

C                   Get basin name

            IF (IERR .EQ. 0) THEN
              CALL KKNXR(IUN,LINE,NXBAS,LWORD,KFOU,LERR,D1,D2,D3)
              IF (NXBAS.EQ.' ' .OR. LWORD.GT.8) IERR = 5
            ENDIF

C                   Check if this basin is wanted

            IF (NXBAS .NE. ' ') THEN
              IF (NBASN .LE. 0) THEN
                IFOU = 999
              ELSE
                INEX = NBASN
                IFOU = 0
   42           IF (INEX .LE. IFOU) GOTO 44
                  IF (NXBAS .EQ. BASINS(INEX)) IFOU = INEX
                  INEX = INEX-1
                  GOTO 42
   44           CONTINUE
              ENDIF
            ENDIF

          ENDIF
          GOTO 20
   50   CONTINUE

C                   Have either LERR=-1 for no basin, LERR=pos for err,
C                     or LERR=0 and IFOU=1 for basin

        IF (IFOU .NE. 0) THEN

C                   Pass through description

          CALL KKNXR(IUN,LINE,WORD,LWORD,KFOU,LERR,D1,D2,D3)
          IF (LWORD .LT. 0) IERR = 4

C                   If all is well, start looping through lat/lon pairs

          IF (IERR .EQ. 0) THEN
            NUMLL = 0
            IEND  = 0
            WORD  = ' '
            LWORD = 0
  130       IF (IERR.NE.0 .OR. IEND.NE.0) GOTO 140

              CALL KKNXR(IUN,LINE,WLAT,LWLAT,KFOU,LERR,D1,D2,D3)
              IF (LWLAT .GT. 0) THEN
                KH = WLAT(1:1)
                IF (KH.NE.'-' .AND. (KH.LT.'0' .OR. KH.GT.'9')) THEN
                  WORD  = WLAT
                  LWORD = LWLAT
                  IEND  = 1
                  LWLAT = -1
                ENDIF
              ENDIF

              IF (LWLAT .GT. 0) THEN
                CALL KKNXR(IUN,LINE,WLON,LWLON,KFOU,LERR,D1,D2,D3)
                IF (LWLON .GT. 0) THEN
                  KH = WLON(1:1)
                  IF (KH.NE.'-' .AND. (KH.LT.'0' .OR. KH.GT.'9')) THEN
                    WORD  = WLON
                    LWORD = LWLON
                    IEND  = 1
                    LWLON = -1
                  ENDIF
                ENDIF
              ENDIF

              IF (LWLAT.GT.0 .AND. LWLON.GT.0 .AND. NUMLL.LT.MX) THEN
                CALL KKA2F(WLAT,LWLAT,FNLAT,IELAT)
                CALL KKA2F(WLON,LWLON,FNLON,IELON)
                IF (IELAT.EQ.0 .AND. IELON.EQ.0) THEN
                  NUMLL = NUMLL+1
                  FLAT(NUMLL) = FNLAT
                  FLON(NUMLL) = FNLON
                ELSE
                  IERR = 2
                ENDIF
                IF (KFOU .EQ. ')') IEND = -1
CCCC          ELSE
CCCC            IERR = 7
              ENDIF
              GOTO 130
  140       CONTINUE
          ENDIF

          AREA = 0.0
          IF (LWORD .LE. 0) THEN
            CALL KKNXR(IUN,LINE,WORD,LWORD,KFOU,LERR,D1,D2,D3)
          ENDIF
          IF (LWORD .GT. 0) THEN
            IF (WORD .EQ. 'ELEV') THEN
              CALL KKNXR(IUN,LINE,WORD,LWORD,KFOU,LERR,D1,D2,D3)
              IF (WORD.NE.'BASN' .AND. WORD.NE.'AREA') THEN
                CALL KKNXR(IUN,LINE,WORD,LWORD,KFOU,LERR,D1,D2,D3)
              ENDIF
            ENDIF
            IF (WORD .EQ. 'AREA') THEN
              CALL KKNXR(IUN,LINE,WORD,LWORD,KFOU,LERR,D1,D2,D3)
              IF (LWORD .GT. 0) THEN
                CALL KKA2F(WORD,LWORD,AREA,IEAREA)
                IF (IEAREA .NE. 0) IERR = 8
              ENDIF
            ENDIF
          ENDIF

        ENDIF

        IF (LERR .GT. 0) IERR = 3
        IF (IERR.EQ.0 .AND. LERR.LT.0) IERR = -1

      RETURN
      END
