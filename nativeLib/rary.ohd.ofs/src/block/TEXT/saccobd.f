c  ==============================================================
C  pgm: BDSACCO .. Block data for initialising storage variables
c                   for SACCO mod display
C  ==============================================================
      BLOCK DATA BDSACCO

      common/sacco1/ opernames(10), UZTWC_1(10),UZFWC_1(10),LZTWC_1(10),
     1                LZFSC_1(10),LZFPC_1(10),ADIMC_1(10), fgco_1(10)

      real UZTWC_1,UZFWC_1,LZTWC_1,LZFSC_1,LZFPC_1,ADIMC_1,fgco_1
      character*8 opernames
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSBDSACCO     / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/saccobd.f,v $
     . $',                                                             '
     .$Id: saccobd.f,v 1.1 2001/06/13 10:23:35 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA      opernames/ "","","","","","","","","",""/
      DATA      UZFWC_1  /  -999.,-999.,-999.,-999.,-999.,-999.,
     1                      -999.,-999.,-999.,-999  /
      DATA      UZTWC_1  /  -999.,-999.,-999.,-999.,-999.,-999.,
     1                      -999.,-999.,-999.,-999  /
      DATA      LZTWC_1  /  -999.,-999.,-999.,-999.,-999.,-999.,
     1                      -999.,-999.,-999.,-999  /
      DATA      LZFSC_1  /  -999.,-999.,-999.,-999.,-999.,-999.,
     1                      -999.,-999.,-999.,-999  /
      DATA      LZFPC_1  /  -999.,-999.,-999.,-999.,-999.,-999.,
     1                      -999.,-999.,-999.,-999  /
      DATA      ADIMC_1  /  -999.,-999.,-999.,-999.,-999.,-999.,
     1                      -999.,-999.,-999.,-999  /
      DATA      fgco_1   /  -999.,-999.,-999.,-999.,-999.,-999.,
     1                      -999.,-999.,-999.,-999  /      

      END
