C MODULE MXL_SFLI
C  =====================================================================
C  pgm: MXL_SFLI .. Output results from rtn sfbdrv to log file
C
C  rqd: WLIN
C  =====================================================================
      SUBROUTINE MXL_SFLI(ID,NUMBAS,K,AREA,UNIT,IY,IXB,IXE)

      EXTERNAL        WLIN

      INTEGER         NUMBAS,K
      INTEGER         IY(*),IXB(*),IXE(*)
      REAL            AREA
      CHARACTER*(*)   ID,UNIT

      CHARACTER*200   LIN
      CHARACTER*61    FMA
      CHARACTER*41    FMA1
      CHARACTER*20    FMA2
      CHARACTER*3     UNITNM
      INTEGER         II,JE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxl_sfli.f,v $
     . $',                                                             '
     .$Id: mxl_sfli.f,v 1.1 2001/06/13 09:23:38 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA   FMA1 / '(''     I='',I3,''   IY(I)='',I4,''   IXB(I)=''' /
      DATA   FMA2 / ',I4,''   IXE(I)='',I4)' /

        UNITNM = '???'
        IF (UNIT .EQ. 'ENGL') UNITNM = 'MI2'
        IF (UNIT .EQ. 'METR') UNITNM = 'KM2'

        LIN = '   basin name ............ '
        WRITE(LIN(28:),'(2X,A)',IOSTAT=JE) ID
        IF (JE.EQ.0) CALL WLIN('M',LIN)
        LIN = '   no of this basin ...... '
        WRITE(LIN(28:),'(I10)',IOSTAT=JE) NUMBAS
        IF (JE.EQ.0) CALL WLIN('M',LIN)
        LIN = '   no of line segments ... '
        WRITE(LIN(28:),'(I10)',IOSTAT=JE) K
        IF (JE.EQ.0) CALL WLIN('M',LIN)
        LIN = '   computed area ......... '
        WRITE(LIN(28:),'(F10.2,2X,A3)',IOSTAT=JE) AREA,UNITNM
        IF (JE.EQ.0) CALL WLIN('M',LIN)

        FMA = FMA1 // FMA2
        DO 245 II=1,K
          IF (II.LT.3 .OR. II.GT.K-2) THEN
            LIN = ' '
            WRITE(LIN,FMA,IOSTAT=JE) II,IY(II),IXB(II),IXE(II)
            IF (JE.EQ.0) CALL WLIN('M',LIN)
          ENDIF
  245   CONTINUE

        CALL WLIN('M','   normal end')

      RETURN
      END
