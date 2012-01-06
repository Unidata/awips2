C     Module PRCO31
C
      SUBROUTINE PRCO31(PXI,TA,MELT,WATER,HEAT,EXCESS,
     2 GMRO,PACKRO,RM,IOUNIT)
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     This subroutine prints carryover and other variables
C     for debugging.
C
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        Jay Day - RTi., JULY 1988 for use in the SNOW-43 operation
C.......................................
C    Arguments:
C
C    Argument       I/O         Description
C      PXI           I          Precipitation
C      TA            I          Air Temperature
C      MELT          I          Melt from Snow Pack
C      WATER         I          Water due to melt and precip.
C      HEAT          I          Heat gained by the pack
C      EXCESS        I          Excess Water in snow pack
C      GMRO          I          Melt at Ground Snow Interface
C      PACKRO        I          Total runoff from the snow pack
C      RM            I          Rain + melt
C      IOUNIT        I          Unit number where all the data 
C                               has to be written
C.......................................
C
      implicit none
C----- D E C L A R A T I O N S --------------------------
C     --- F U N C T I O N  A R G U M E N T S ---
      real     pxi, ta, melt, water,
     1         heat, excess, gmro, packro, rm
      integer  iounit
C     --- L O C A L ---
      integer  i
C
C----- C O M M O N  B L O C K S -------------------------
      include 'snow43/cntl31'
      include 'snow43/snco31'
      include 'snow43/cupdt31'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/prco31.f,v $
     . $',                                                             '
     .$Id: prco31.f,v 1.1 1996/05/07 11:05:04 page Exp $
     . $' /
C    ===================================================================
C
C.....................................
C
      write(iounit,610) 
  610 format(1x,'DETAILED INFORMATION ON CO & SNOWMELT COMPUTATIONS')
C
      WRITE(IOUNIT,611) KMO31,KDA31,KYR31,KHR31,
     1 WE,NEGHS,LIQW,TINDEX,AESC,ACCMAX,
     2 SB,SBAESC,SBWS,STORGE,
     3 PXI,TA,MELT,WATER,HEAT,
     4 EXCESS,GMRO,PACKRO,RM,
     5 (EXLAG(I),I=1,NEXLAG)
C
  611 FORMAT(1X,i2,1h/,i2,1h/,i2,6H HOUR ,i2,/1x,
     1 12h     WE     ,1x,6hNEGHS ,2x,6hLIQW  ,2x,6HTINDEX,2x,7hAESC   ,
     2 1x,7hACCMAX ,1x,7hSB     ,1x,7hSBAESC ,1x,7hSBWS   ,1x,7hSTORGE ,
     3 /1x, 10(f7.1, 1x),/1x,
     4 12h     PXI    ,1x,6hTA    ,2x,6hMELT  ,2x,6hWATER ,2x,7hHEAT   ,
     5 1x,7hEXCESS ,1x,7hGMRO   ,1x,7hPACKRO ,1x,7hRM     ,1x,7hEXLAG  ,
     6 /2x, 9(f7.2, 1x),2x,7(f5.2, 1x))
C
      RETURN
      END
