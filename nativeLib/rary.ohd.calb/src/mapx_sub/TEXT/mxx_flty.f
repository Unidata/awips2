C MODULE MXX_FLTY
C  ====================================================================
C  pgm: MXX_FLTY .. Check file type (text,.Z,.gz) and maybe uncompress
C  ====================================================================
      SUBROUTINE MXX_FLTY(DIRGRD,FILGRD,FILTMP,FILOUT,ISTAT)

      INTRINSIC      LEN
      INTEGER        LEN

      EXTERNAL       KKLAST
      INTEGER        KKLAST

      CHARACTER*(*)  DIRGRD,FILGRD,FILTMP,FILOUT
      CHARACTER*300  CMND
      INTEGER        LND,LNF,LNT,LNO,LNC,LENTH
      INTEGER        ISTAT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxx_flty.f,v $
     . $',                                                             '
     .$Id: mxx_flty.f,v 1.1 2001/06/13 09:26:03 mgm Exp $
     . $' /
C    ===================================================================
C

        LND = KKLAST(0,DIRGRD)
        LNF = KKLAST(0,FILGRD)
        LNT = KKLAST(0,FILTMP)
        LNO = LEN(FILOUT)
        LNC = LEN(CMND)

        FILOUT = ' '
        ISTAT = 1
        IF (LND.GT.0 .AND. LNO.GT.0 .AND. LNT.GT.0) THEN

          ISTAT = 2
          IF (LNF.GT.0 .AND. FILGRD(LNF:LNF).EQ.'z') THEN
            LENTH = LND+LNF+1
            IF (LENTH .LE. LNO) THEN
              FILOUT = DIRGRD(1:LND) // '/' // FILGRD(1:LNF)
              ISTAT = 0
            ENDIF

          ELSEIF (LNF.GT.1 .AND. FILGRD(LNF-1:LNF).EQ.'.Z') THEN
            LENTH = LND+LNF+LNT+LNT+LNT+40
            IF (LENTH .LE. LNC) THEN
              CMND = 'test -f ' // FILTMP(1:LNT) // ' && ' //
     $               'rm -f ' // FILTMP(1:LNT)
              CALL SYSTEM(CMND)
              CMND = 'cp ' //
     $               DIRGRD(1:LND) // '/' // FILGRD(1:LNF) // ' ' //
     $               FILTMP(1:LNT) // '.Z' // ' && ' //
     $               'chmod 777 ' // FILTMP(1:LNT) // '.Z' // ' && ' //
     $               'uncompress ' // FILTMP(1:LNT) // '.Z'
              CALL SYSTEM(CMND)
              IF (LNO .GE. LNT+2) THEN
                FILOUT = FILTMP(1:LNT)
                ISTAT = -1
              ENDIF
            ENDIF

          ELSEIF (LNF.GT.2 .AND. FILGRD(LNF-2:LNF).EQ.'.gz') THEN
            LENTH = LND+LNF+LNT+LNT+LNT+40
            IF (LENTH .LE. LNC) THEN
              CMND = 'test -f ' // FILTMP(1:LNT) // ' && ' //
     $               'rm -f ' // FILTMP(1:LNT)
              CALL SYSTEM(CMND)
              CMND = 'cp ' //
     $               DIRGRD(1:LND) // '/' // FILGRD(1:LNF) // ' ' //
     $               FILTMP(1:LNT) // '.gz' // ' && ' //
     $               'chmod 777 ' // FILTMP(1:LNT) // '.gz' // ' && ' //
     $               'gunzip ' // FILTMP(1:LNT) // '.gz'
              CALL SYSTEM(CMND)
              IF (LNO .GE. LNT+2) THEN
                FILOUT = FILTMP(1:LNT)
                ISTAT = -2
              ENDIF
            ENDIF

          ELSE
            ISTAT = 3

          ENDIF

        ENDIF

      RETURN
      END
