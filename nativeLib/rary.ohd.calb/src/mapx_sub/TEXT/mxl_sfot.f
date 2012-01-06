C  =====================================================================
C  pgm: MXL_SFOT .. Output results from rtn sfbdrv to hrap file
C
C  out: ISTAT ...... routine status: - INT
C  out:                -1 .... no action because there was no data
C  out:                 0 .... no errors, output completed
C  out:                 1 .... bad unit number, no output tried
C  out:                 2 .... bad write to given unit number
C
C  rqd: KKLAST
C  =====================================================================
      SUBROUTINE MXL_SFOT(IUN,ID,NUMBAS,K,AREA,IY,IXB,IXE,ISTAT)

      EXTERNAL        KKLAST
      INTEGER         KKLAST

      INTEGER         IUN,NUMBAS,K,ISTAT
      INTEGER         IY(*),IXB(*),IXE(*)
      REAL            AREA
      CHARACTER*(*)   ID

      INTEGER         II,JE,LENID
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxl_sfot.f,v $
     . $',                                                             '
     .$Id: mxl_sfot.f,v 1.1 2001/06/13 09:23:49 mgm Exp $
     . $' /
C    ===================================================================
C

        ISTAT = -1
        IF (ID.NE.' ' .AND. NUMBAS.GT.0 .AND. K.GT.0) THEN
          IF (IUN.LT.1 .OR. IUN.GT.99) THEN
            ISTAT = 1
          ELSE
            LENID = KKLAST(1,ID)
            WRITE(IUN,'(3X,A)',IOSTAT=JE) ID(1:LENID)
            IF (JE .EQ. 0) THEN
              WRITE(IUN,'(I6,I7,F13.4)',IOSTAT=JE) NUMBAS,K,AREA
            ENDIF

            II = 1
  100       IF (II.GT.K .OR. JE.NE.0) GOTO 120
              WRITE(IUN,'(3X,4I4)',IOSTAT=JE) II,IY(II),IXB(II),IXE(II)
              II = II+1
              GOTO 100
  120       CONTINUE

            ISTAT = 0
            IF (JE .NE. 0) ISTAT = 2
          ENDIF
        ENDIF

      RETURN
      END
