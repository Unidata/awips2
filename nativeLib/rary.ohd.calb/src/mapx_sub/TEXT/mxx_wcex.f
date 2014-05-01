C MODULE MXX_WCEX
C  =====================================================================
C  pgm: MXX_WCEX .. Output change of filename extension
C  =====================================================================
      SUBROUTINE MXX_WCEX(BASINS,XTIME,FLMAP)

      CHARACTER*(*)  BASINS,XTIME,FLMAP
      CHARACTER*200  LIN
      CHARACTER*17   MSG1,MSG2,MSG3
      INTEGER        JE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxx_wcex.f,v $
     . $',                                                             '
     .$Id: mxx_wcex.f,v 1.1 2001/06/13 09:28:56 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA   MSG1 / 'file exists:     ' /
      DATA   MSG2 / 'extension added: ' /
      DATA   MSG3 / 'new file name:   ' /

        CALL WLIN('B',' ')

        LIN = ' '
        WRITE(LIN,'(A,A)',IOSTAT=JE) MSG1,BASINS
        IF (JE.EQ.0) CALL WLIN('C',LIN)

        LIN = ' '
        WRITE(LIN,'(A,A)',IOSTAT=JE) MSG2,XTIME
        IF (JE.EQ.0) CALL WLIN('S',LIN)

        LIN = ' '
        WRITE(LIN,'(A,A)',IOSTAT=JE) MSG3,FLMAP
        IF (JE.EQ.0) CALL WLIN('S',LIN)

        CALL WLIN('B',' ')

      RETURN
      END
