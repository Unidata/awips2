C MODULE MXX_EXTN
C  =====================================================================
C  pgm: MXX_EXTN .. Add date extension _jjj_hhnn to name if file exists
C
C  use:     CALL MXX_EXTN(BASNM,FILNM,XTIME,ISTAT)
C
C   in: BASNM ... name of basin involved for this pathname - CHAR*(*)
C  i/o: FILNM ... pathname of file that may get extension if
C   in:           the named file already exists - CHAR*(*)
C   in: XTIME ... extension date as a string as jjj_hhnn - INT
C   in            (note, the extension is added after an underscore)
C  out: ISTAT ... status: - INT
C  out:             -1 .. no error, file does NOT exist, no extension
C  out:              0 .. no error, extension added to filename
C  out:              1 .. warning, no input filename or extension
C  out:              2 .. error, cannot access file
C  out:              3 .. error, resulting name too long
C
C  rqd: KKLAST,MXX_WCEX,UPEXIS
C  =====================================================================
      SUBROUTINE MXX_EXTN(BASNM,FILNM,XTIME,ISTAT)

      INTRINSIC      LEN
      INTEGER        LEN

      EXTERNAL       KKLAST,MXX_WCEX,UPEXIS
      INTEGER        KKLAST

      CHARACTER*(*)  BASNM,FILNM,XTIME
      INTEGER        ISTAT,JE,IUN3,MXF,LNF,LNX,LNB,LNFNEW
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxx_extn.f,v $
     . $',                                                             '
     .$Id: mxx_extn.f,v 1.1 2001/06/13 09:25:56 mgm Exp $
     . $' /
C    ===================================================================
C

        ISTAT = 1

        MXF = LEN(FILNM)
        LNF = KKLAST(0,FILNM)
        LNX = KKLAST(0,XTIME)

        IF (LNF.GT.0 .AND. LNX.GT.0) THEN
          IUN3 = 3
          CALL UPEXIS(IUN3,FILNM,JE)
          IF (JE .GT. 0) THEN
            ISTAT = 2
          ELSEIF (JE .LT. 0) THEN
            ISTAT = -1
          ELSE
            LNFNEW = LNF+LNX+1
            IF (LNFNEW .GT. MXF) THEN
              ISTAT = 3
            ELSE
              ISTAT = 0
              FILNM = FILNM(1:LNF) // '_' // XTIME(1:LNX)

              LNB = KKLAST(1,BASNM)
              CALL MXX_WCEX(BASNM(1:LNB),XTIME(1:LNX),FILNM(1:LNFNEW))
            ENDIF
          ENDIF
        ENDIF

      RETURN
      END
