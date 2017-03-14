C MODULE MXO_WHRP
C  =====================================================================
C  pgm: MXO_WHRP .. Output lat-lon info to ".latlon" file
C  =====================================================================
      SUBROUTINE MXO_WHRP(IUN,NXBAS,IY,IXB,IXE,NUMLL,AREA,UNIT,
     $                    NUMBA,IERR)

      INTEGER         IUN,NUMLL,NUMBA,IERR
      REAL            AREA
      INTEGER         IY(*),IXB(*),IXE(*)
      CHARACTER*4     UNIT
      CHARACTER*8     NXBAS
      INTEGER         II,JE
      INTEGER         LINLIM

      SAVE            LINLIM
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxo_whrp.f,v $
     . $',                                                             '
     .$Id: mxo_whrp.f,v 1.1 2001/06/13 09:24:25 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA            LINLIM / 0 /

        IF (NUMLL.GT.0 .AND. IUN.GT.0 .AND. IUN.LE.99) THEN

          WRITE(IUN,'(3X,A)',IOSTAT=JE) NXBAS
          WRITE(IUN,'(I6,I7,F13.4)',IOSTAT=JE) NUMBA,NUMLL,AREA

          DO 34 II=1,NUMLL
            WRITE(IUN,'(I7,3I4)',IOSTAT=JE) II,IY(II),IXB(II),IXE(II)
            LINLIM = LINLIM+1
            IF (LINLIM .GT. 200000) STOP 111
   34     CONTINUE

          IF (JE .NE. 0) IERR = 6
        ELSE
          IERR = -2
        ENDIF

      RETURN
      END
