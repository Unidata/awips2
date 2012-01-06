C MODULE MXC_WBKH
C  =====================================================================
C  pgm: MXC_WBKH .. Output error message for a bad char in an int field
C  =====================================================================
      SUBROUTINE MXC_WBKH(JSTAT,LIN,MSG)

      CHARACTER*(*)   LIN,MSG
      CHARACTER*35    MSG0
      INTEGER         JSTAT,JE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxc_wbkh.f,v $
     . $',                                                             '
     .$Id: mxc_wbkh.f,v 1.1 2001/06/13 09:21:45 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA   MSG0 / 'bad character in integer value for ' /

        JSTAT = JSTAT+1
        LIN = ' '
        WRITE(LIN,'(A,A)',IOSTAT=JE) MSG0,MSG
        IF(JE.EQ.0) CALL WLIN('E',LIN)

      RETURN
      END
