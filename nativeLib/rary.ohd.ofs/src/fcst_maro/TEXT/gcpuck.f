C MEMBER GCPUCK
C  (from old member PPGCPUCK)
C
      SUBROUTINE GCPUCK(CALRTN)
C
C.....THIS SUBROUTINE DOES A CPU TIME CHECK. THE CPU TIME GIVEN IS THE
C.....TIME ELAPSED SINCE THE LAST CPU TIME CHECK.
C
C.....JERRY M. NUNN       WGRFC FT. WORTH       JANUARY 5, 1988
C
C.....HERE ARE THE ARGUMENTS:
C
C.....CALRTN - THE CALLING ROUTINE.
C
      DIMENSION CALRTN(2)
C
      COMMON /HCPUCK/ ITOTCP, IBGSEC, ILASEC
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/gcpuck.f,v $
     . $',                                                             '
     .$Id: gcpuck.f,v 1.1 1995/09/17 19:01:11 dws Exp $
     . $' /
C    ===================================================================
C
C
  900 FORMAT(1H0, '*** SUBROUTINE GCPUCK CALLED BY SUBROUTINE ', 2A4,
     * ' ***', /, 1H0, '    CPU TIME ELAPSED SINCE LAST CPU TIME CHECK =
     * ', F7.2, ' SEC.')
C
C.....GET THE CPU TIME.
C
      CALL URTIMR(LAPSE, ITOTCP)
      ELAPSE = LAPSE/100.
      WRITE(IOPDBG,900) CALRTN, ELAPSE
C
      RETURN
      END
