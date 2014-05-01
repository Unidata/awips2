C MODULE MXO_WLAT
C  =====================================================================
C  pgm: MXO_WLAT .. Output lat-lon info to ".latlon" file
C  =====================================================================
      SUBROUTINE MXO_WLAT(IUN,NXBAS,FLAT,FLON,NUMLL,AREA,UNIT,DESC,IERR)

      INTEGER         IUN,NUMLL,IERR
      REAL            FLAT(*),FLON(*),AREA
      CHARACTER*4     UNIT
      CHARACTER*8     NXBAS
      CHARACTER*20    DESC
      CHARACTER*1     QU
      INTEGER         II,JE
      INTEGER         LINLIM

      SAVE            LINLIM
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxo_wlat.f,v $
     . $',                                                             '
     .$Id: mxo_wlat.f,v 1.1 2001/06/13 09:24:33 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA            LINLIM / 0 /
      DATA            QU     / '''' /

        IF (NUMLL.GT.0 .AND. IUN.GT.0 .AND. IUN.LE.99) THEN

CC        WRITE(IUN,'(A),IOSTAT=JE) '@DEFINE BASIN'
          WRITE(IUN,'(''BASN('',A,'') '',A8,2X,A1,A,A1)',IOSTAT=JE)
     $          UNIT,NXBAS,QU,DESC,QU
          WRITE(IUN,'(''('')',IOSTAT=JE)

          DO 30 II=1,NUMLL
            WRITE(IUN,'(F10.4,'','',F8.4)',IOSTAT=JE) FLAT(II),FLON(II)
            LINLIM = LINLIM+1
            IF (LINLIM .GT. 200000) STOP 111
   30     CONTINUE

          WRITE(IUN,'('')'')',IOSTAT=JE)
          WRITE(IUN,'('' AREA('',F12.4,'')'')',IOSTAT=JE) AREA

          IF (JE .NE. 0) IERR = 6
        ELSE
          IERR = -2
        ENDIF

      RETURN
      END
