C MODULE XSPACE
C-----------------------------------------------------------------------
C
      SUBROUTINE XSPACE(MX,K,TYPE)
C
C  PRINTS MESSAGE WHEN MAIN MAP DATA ARRAY IS EXCEEDED.
C
C  WRITTEN BY - ERIC ANDERSON  HRL 12/1983
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_map/RCS/xspace.f,v $
     . $',                                                             '
     .$Id: xspace.f,v 1.2 2002/02/11 20:39:40 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF( IPTRCE.GE.3) WRITE (IOPDBG,*) 'ENTER XSPACE'
C
      WRITE (IPR,901) TYPE,MX+K,MX
  901 FORMAT ('0**ERROR** NUMBER OF WORDS NEEDED TO PROCESS ',A4,
     *  ' DATA (',I6,') EXCEEDS SPACE AVAILABLE (',I6,').')
      CALL ERROR
C
      IF(IPTRCE.GE.3) WRITE (IOPDBG,*) 'EXIT XSPACE'
C
      RETURN
C
      END
