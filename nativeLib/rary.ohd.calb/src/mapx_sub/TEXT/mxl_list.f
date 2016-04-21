C MODULE MXL_LIST
C  =====================================================================
C  pgm: MXL_LIST .. Outputll_grid and hrap pathnames
C
C  rqd: WLIN
C  =====================================================================
      SUBROUTINE MXL_LIST(FLLATL,FLHRAP)

      EXTERNAL        WLIN

      CHARACTER*(*)   FLLATL,FLHRAP
      CHARACTER*200   LIN
      INTEGER         JE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxl_list.f,v $
     . $',                                                             '
     .$Id: mxl_list.f,v 1.1 2001/06/13 09:22:09 mgm Exp $
     . $' /
C    ===================================================================
C

        CALL WLIN('B',' ')

        LIN = ' '
        WRITE(LIN,'(A,A)',IOSTAT=JE) ' LAT/LON PAIRS FILE: ',FLLATL
        IF(JE.EQ.0) CALL WLIN('M',LIN)

        LIN = ' '
        WRITE(LIN,'(A,A)',IOSTAT=JE) ' LINE SEG HRAP FILE: ',FLHRAP
        IF(JE.EQ.0) CALL WLIN('M',LIN)

        CALL WLIN('B',' ')

      RETURN
      END
