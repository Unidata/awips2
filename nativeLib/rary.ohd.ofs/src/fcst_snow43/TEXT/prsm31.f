C     Module PRSM31
C
      SUBROUTINE PRSM31
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     This routine is used to print the snow model summations
C         SPX, SSFALL, SRM, SMELT, SMELTR, SROBG,
C         DSFALL, DRAIN, DQNET
C.......................................
C     Subroutine initially written by ...
C        Jay Day - RTi., JULY 1988 for use in the SNOW-43 operation
C.......................................
C     No Arguments:
C.......................................
C
      implicit none
C----- D E C L A R A T I O N S --------------------------
C     --- C O M M O N  B L O C K  V A R S ---
      integer iodbug, itrace, idball, ndebug, idebug
C
C----- C O M M O N  B L O C K S -------------------------
      include 'snow43/sums31'
      include 'snow43/cntl31'
      COMMON/FDBUG/IODBUG, ITRACE, IDBALL, NDEBUG, IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/prsm31.f,v $
     . $',                                                             '
     .$Id: prsm31.f,v 1.1 1996/05/07 11:05:18 page Exp $
     . $' /
C    ===================================================================
C
C
      write(iodbug, 610) 
  610 format(1x,29hCONTENTS OF THE SUMS31 BLOCK )
C
      WRITE(iodbug,611) KMO31, KDA31, KYR31, KHR31,
     1               SPX, SSFALL, SRM, SMELT,
     2               SMELTR, SROBG, DSFALL,
     3               DRAIN, DQNET, DRSL, NDRSP
C
  611 FORMAT(1X,I2,1h/,i2,1h/,i2,6h HOUR ,i2,/,
     162h       SPX       SSFALL    SRM       SMELT     SMELTR    SROBG,
     262h     DSFALL    DRAIN     DQNET     DRSL      NDRSP            ,
     3/,10f10.2,i10)
C
      RETURN
      END
