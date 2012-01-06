C MODULE MXO_XHRP
C  =====================================================================
C  pgm: MXO_XHRP .. Create the latlon file from "ofs" files
C  =====================================================================
      SUBROUTINE MXO_XHRP(LARR,KARR,IARR,RARR,IY,IXB,IXE,NUMLL,
     $                    FLHRAP,BASINS,NBASN,ISTAT)

      EXTERNAL        KKLAST
      INTEGER         KKLAST

      CHARACTER*(*)   KARR
      INTEGER         IARR(*)
      REAL            RARR(*)
      INTEGER         LARR

      REAL            AREA
      INTEGER         IY(*),IXB(*),IXE(*)
      CHARACTER*8     BASINS(*),NXBAS
      CHARACTER*4     UNIT
      CHARACTER*18    MSG3
      INTEGER         ISTAT,NBASN,NUMLL,IPTR
      CHARACTER*(*)   FLHRAP
      CHARACTER*200   LIN

      INTEGER         USEGM,ISERR,NUMBA,IERR,LSEGM,JE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxo_xhrp.f,v $
     . $',                                                             '
     .$Id: mxo_xhrp.f,v 1.1 2001/06/13 09:24:41 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA  MSG3 / 'cannot open file: ' /

        ISTAT = -1
        USEGM = 82

        CALL UPOPEN(USEGM,FLHRAP,0,'F',ISERR)
        IF (ISERR .NE. 0) THEN
          LSEGM = KKLAST(1,FLHRAP)
          LIN = ' '
          WRITE(LIN,'(A,A)',IOSTAT=JE) MSG3,FLHRAP(1:LSEGM)
          IF(JE.EQ.0) CALL WLIN('E',LIN)
          ISTAT = 1
        ENDIF

        IF (ISERR .EQ. 0) THEN
          CALL MXO_DBOP('O',JE)
          IF (JE .NE. 0) ISTAT = 3
        ENDIF

        IF (ISTAT.LE.0 .AND. ISERR.EQ.0) THEN
          NUMBA = 0
          ISTAT = 0
          IPTR = 0
          IERR = 0
  120     IF (IERR .NE. 0) GOTO 130
            IF (NBASN .GT. 0) THEN
              IF (NUMBA .LT. NBASN) THEN
                NUMBA = NUMBA+1
              ELSE
                IERR = -1
              ENDIF
            ENDIF

            IF (IERR .EQ. 0) THEN
              CALL MXO_RHRP(LARR,KARR,IARR,RARR,BASINS,NBASN,NXBAS,
     $                      NUMBA,IY,IXB,IXE,NUMLL,AREA,UNIT,IPTR,IERR)

              IF (NUMLL.GT.0 .AND. IERR.EQ.0) THEN
                CALL MXO_WHRP(USEGM,NXBAS,IY,IXB,IXE,NUMLL,AREA,UNIT,
     $                        NUMBA,IERR)
              ENDIF
            ENDIF
            GOTO 120
  130     CONTINUE
          IF (IERR .GT. 0) ISTAT = 2
        ENDIF

        IF (ISERR .EQ. 0) CALL UPCLOS(USEGM,' ',IERR)

      RETURN
      END
