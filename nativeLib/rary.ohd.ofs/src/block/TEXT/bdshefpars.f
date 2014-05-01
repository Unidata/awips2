C MODULE BDSHEFPARS
C  =====================================================================
C  pgm: BDSHEFPARS .. Block data for program name, version, date
C
C  cmt: Created by user "ihfsuser" on 040628 14:12
C  =====================================================================
      BLOCK DATA BDSHEFPARS
      common /CMSHEFPARS/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68             RCSKW1,RCSKW2
      COMMON / RCSBDSHEFPARS / RCSKW1,RCSKW2
      DATA                     RCSKW1,RCSKW2 /                         '
     .$Source: /fs/hseb/ob81/ohd/ofs/src/block/RCS/bdshefpars.f,v $
     . $',                                                             '
     .$Id: bdshefpars.f,v 1.20 2007/03/20 17:32:36 dsa Exp $
     . $' /
C    ===================================================================
C

      DATA      PGMNAM  /  'SHEFPARS'    /
      DATA      PGMVRN  /  'ob8.1     '  /
      DATA      PGMVRD  /  '03/20/07'    /
      DATA      PGMSYS  /  'RFS5'        /
      DATA      PGMCMP  /  'F90 '        /
      DATA      MPGMRG  /   0            /

      END
