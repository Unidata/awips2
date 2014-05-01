      SUBROUTINE  HEADER()
 
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_stubs/RCS/header.f,v $
     . $',                                                             '
     .$Id: header.f,v 1.1 1995/09/17 19:07:54 dws Exp $
     . $' /
C    ===================================================================
C

        WRITE(IPR,100)
  100   FORMAT(' >>>>> SUBROUTINE  HEADER  ENTERED - ',
     $         'CURRENTLY STUBBED OFF')
 
      RETURN
      END
