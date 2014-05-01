      SUBROUTINE GRIB2PACKER_TEST_MAIN

      logical j
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2packer/RCS/test.f,v $
     . $',                                                             '
     .$Id: test.f,v 1.1 2004/09/16 16:52:29 dsa Exp $
     . $' /
C    ===================================================================
C
      call pk_endian(j)
      write(*,*) j
      end
