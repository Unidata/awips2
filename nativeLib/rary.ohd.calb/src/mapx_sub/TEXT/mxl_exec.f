C MODULE MXL_EXEC
C  =====================================================================
C  pgm: MXL_EXEC .. Compute hrap file from latlon file
C
C  rqd: WLIN,UPOPEN,UPCLOS,MXL_SFLI,MXL_SFER,MXL_SFOT,SFBDRV,MXL_READ
C  rqd: KKLAST
C  =====================================================================
      SUBROUTINE MXL_EXEC(MX,X,Y,FLAT,FLON,IDIM,IY,IXB,IXE,
     $                    FLLATL,FLHRAP,NAREA,BASINS,ISTAT)

      EXTERNAL        WLIN,UPOPEN,UPCLOS,MXL_SFLI,MXL_SFER,MXL_SFOT
      EXTERNAL        SFBDRV,MXL_READ,KKLAST
      INTEGER         KKLAST

      CHARACTER*(*)   FLLATL,FLHRAP
      CHARACTER*8     BASINS(*),NXBAS
      CHARACTER*4     UNIT
      CHARACTER*200   LIN

      REAL            X(*),Y(*),FLAT(*),FLON(*)
      REAL            AREA,UAREA,CAREA,XC,YC
      INTEGER         IY(*),IXB(*),IXE(*)
      CHARACTER*18    MSG3
      INTEGER         USEGM,UBOUN,NAREA,ISTAT,IERR,JE,IBERR,ISERR
      INTEGER         LSEGM,MX,NUMLL,NUM_BASN,LBOUN,IDIM,NUMHH
      INTEGER         LFACTR,JSTAT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxl_exec.f,v $
     . $',                                                             '
     .$Id: mxl_exec.f,v 1.1 2001/06/13 09:22:01 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA  MSG3 / 'cannot open file: ' /

C  GENERATE PATH TO LL_GRID INPUT FILE, AND CALL LL_GRID

        CALL WLIN('B',' ')
        CALL WLIN('M','  ========>  BEGIN PROCESSING LAT/LON FILE')

        ISTAT = 0
        USEGM = 82
        UBOUN = 83

        CALL UPOPEN(UBOUN,FLLATL,0,'F',IBERR)
        IF (IBERR .NE. 0) THEN
          LBOUN = KKLAST(1,FLLATL)
          LIN = ' '
          WRITE(LIN,'(A,A)',IOSTAT=JE) MSG3,FLLATL(1:LBOUN)
          IF(JE.EQ.0) CALL WLIN('E',LIN)
          ISTAT = 1
        ENDIF

        ISERR = -1
        IF (IBERR .EQ. 0) THEN
          CALL UPOPEN(USEGM,FLHRAP,0,'F',ISERR)
          IF (ISERR .NE. 0) THEN
            LSEGM = KKLAST(1,FLHRAP)
            LIN = ' '
            WRITE(LIN,'(A,A)',IOSTAT=JE) MSG3,FLHRAP(1:LSEGM)
            IF(JE.EQ.0) CALL WLIN('E',LIN)
            ISTAT = 1
          ENDIF
        ENDIF

        IF (IBERR.EQ.0 .AND. ISERR.EQ.0) THEN
          NUM_BASN = 0
          IERR = 0
  120     IF (IERR .NE. 0) GOTO 130
            CALL MXL_READ(UBOUN,BASINS,NAREA,NXBAS,
     $                    MX,FLAT,FLON,NUMLL,AREA,UNIT,IERR)

            IF (NUMLL.GT.0 .AND. IERR.EQ.0) THEN
              NUM_BASN = NUM_BASN+1

              CALL SFBDRV(X,Y,FLAT,FLON,IY,IXB,IXE,IDIM,NUMLL,LFACTR,
     $                    AREA,UAREA,CAREA,XC,YC,UNIT,NUMHH,JSTAT)
              CALL MXL_SFOT(USEGM,NXBAS,NUM_BASN,NUMHH,AREA,
     $                      IY,IXB,IXE,JSTAT)
              CALL MXL_SFER(NXBAS,JSTAT)
              CALL MXL_SFLI(NXBAS,NUM_BASN,NUMHH,AREA,UNIT,IY,IXB,IXE)

            ENDIF
            GOTO 120
  130     CONTINUE
        ENDIF

        IF (ISERR .EQ. 0) CALL UPCLOS(USEGM,' ',IERR)
        IF (IBERR .EQ. 0) CALL UPCLOS(UBOUN,' ',IERR)

        CALL WLIN('M','  ========>  END PROCESSING LAT/LON FILE')
        CALL WLIN('B',' ')

      RETURN
      END
