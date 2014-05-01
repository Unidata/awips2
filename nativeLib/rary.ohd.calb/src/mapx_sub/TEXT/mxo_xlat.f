C MODULE MXO_XLAT
C  =====================================================================
C  pgm: MXO_XLAT .. Create the latlon file from "ofs" files
C  =====================================================================
      SUBROUTINE MXO_XLAT(LARR,KARR,IARR,RARR,FLAT,FLON,NUMLL,
     $                    FLLATL,BASINS,NBASN,ISTAT)

      EXTERNAL        KKLAST
      INTEGER         KKLAST

      CHARACTER*(*)   KARR
      INTEGER         IARR(*)
      REAL            RARR(*)
      INTEGER         LARR

      REAL            FLAT(*),FLON(*),AREA
      CHARACTER*8     BASINS(*),NXBAS
      CHARACTER*4     UNIT
      CHARACTER*20    DESC
      CHARACTER*18    MSG3
      INTEGER         ISTAT,NBASN,NUMLL,IPTR
      CHARACTER*(*)   FLLATL
      CHARACTER*200   LIN

      INTEGER         UBOUN,IBERR,NUMBA,IERR,LBOUN,JE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxo_xlat.f,v $
     . $',                                                             '
     .$Id: mxo_xlat.f,v 1.1 2001/06/13 09:24:53 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA  MSG3 / 'cannot open file: ' /

        ISTAT = -1
        UBOUN = 83

        CALL UPOPEN(UBOUN,FLLATL,0,'F',IBERR)
        IF (IBERR .NE. 0) THEN
          LBOUN = KKLAST(1,FLLATL)
          LIN = ' '
          WRITE(LIN,'(A,A)',IOSTAT=JE) MSG3,FLLATL(1:LBOUN)
          IF(JE.EQ.0) CALL WLIN('E',LIN)
          ISTAT = 1
        ENDIF

        IF (IBERR .EQ. 0) THEN
          CALL MXO_DBOP('O',JE)
          IF (JE .NE. 0) ISTAT = 3
        ENDIF

        IF (ISTAT.LE.0 .AND. IBERR.EQ.0) THEN
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
              CALL MXO_RLAT(LARR,KARR,IARR,RARR,BASINS,NBASN,NXBAS,
     $                      NUMBA,FLAT,FLON,NUMLL,
     $                      AREA,UNIT,DESC,IPTR,IERR)

              IF (NUMLL.GT.0 .AND. IERR.EQ.0) THEN
                CALL MXO_WLAT(UBOUN,NXBAS,FLAT,FLON,NUMLL,AREA,
     $                        UNIT,DESC,IERR)
              ENDIF
            ENDIF
            GOTO 120
  130     CONTINUE
          IF (IERR .GT. 0) ISTAT = 2
        ENDIF

        IF (IBERR .EQ. 0) CALL UPCLOS(UBOUN,' ',IERR)

        IF (ISTAT .LT. 0) ISTAT = 0

      RETURN
      END
