C MODULE UINCLDBD
C-----------------------------------------------------------------------
C
      BLOCK DATA UINCLDBD
C
      INCLUDE 'uinclx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSUINCLDBD      / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/util/src/block/RCS/uincldbd.f,v $
     . $',                                                             '
     .$Id: uincldbd.f,v 1.3 1998/07/02 19:54:22 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA MINCL/99/
      DATA MPINCL/99/
      DATA ICINCL/1/
      DATA NINCL/0/
      DATA PATHINCL/99*' '/
      DATA NRINCL/99*0/
      DATA DIRINCL/' '/
C
      END
