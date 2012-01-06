C MODULE FUNBLK
C-----------------------------------------------------------------------
C
      BLOCK DATA FUNBLK
C
C  INITIALIZE COMMON BLOCKS FOR FCST PROGRAM FUNCTIONS
C
C
C  DEBUG COMMON BLOCK
C
      INCLUDE 'common/pudbug'
C
      DATA IOPDBG/6/
      DATA IPTRCE/0/
      DATA NDBUG/0/
      DATA PDBUG/20*4H    /
      DATA IPALL/0/
C
C
C  CPUCHECK FUNCTION
C
      COMMON/FUN5CB/IFIRST,ICPUF5
C
      DATA IFIRST/0/
      DATA ICPUF5/0/
C
C
C  MARO FUNCTION
C
      INCLUDE 'gcommon/gsize'
      INCLUDE 'gcommon/gmdr'
      INCLUDE 'gcommon/gdispl'
      INCLUDE 'gcommon/gdate'
      INCLUDE 'gcommon/gmropa'
      INCLUDE 'gcommon/gversn'
      INCLUDE 'gcommon/gopt'
      INCLUDE 'gcommon/gboxot'
      INCLUDE 'gcommon/boxtrc'
C
      DATA NGRID, NMARO, NWORK, NGMDR /10000, 800, 100, 700/
      DATA IRUN, NDIST, IPMAX, N6HR /0, 1500, 3000, 200/
      DATA NSQUAR, NBXDMP, NRFRO, NGRBOX /10, 20, 20, 98/
      DATA LRFRO, MRFULL, NPAGE, NWKMAX /16, 384, 30, 200/
      DATA NSTEP1, NSTEP2, NUMPD1, NUMPD2, LGENL /6, 24, 4, 1, 100/ 
C
C
C  MAT FUNCTION
C
      INCLUDE 'common/tary'
      INCLUDE 'common/tscrat'
C
      DATA MARY/5000/
      DATA ISTDAT/218/
      DATA KTSCR/70/
      DATA LRECL/800/
      DATA MAXREC/0/
C
C
C  MAPE FUNCTION
C
      COMMON /VOPT /JAPTN,JPTN,JMAPE,LSTDY
      COMMON /VSTUP/ PTYPE,DTYPE,MET,CPETIM(2),UTYPE,BTYPE,EST(3)
      COMMON /VMODAS/ LSTDAM(13)
      COMMON /VUNTS/ ISTEP,IUNITS,ICALL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSFUNBLK        / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/funblk.f,v $
     . $',                                                             '
     .$Id: funblk.f,v 1.4 1999/07/06 16:28:08 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA PTYPE/4HPE  /
      DATA DTYPE/4HEA24/
      DATA CPETIM/4HPETI,4HMS  /
      DATA UTYPE/4HUSER/
      DATA BTYPE/4HMAPE/
      DATA EST/1H ,1HB,1HM/
      DATA LSTDAM/31,28,31,30,31,30,31,31,30,31,30,31,29/
      DATA ISTEP/24/
      DATA IUNITS/4HMM  /
      DATA ICALL/0/
C
C
      END
