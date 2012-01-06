C MODULE BDWBASMAPX
C  =====================================================================
C  pgm: BDWBASMAPX .. Block data for program name, version, date
C
C  cmt: Created by user "ihfsuser" on 040628 14:12
C  =====================================================================
      BLOCK DATA BDWBASMAPX
      common /CMWBASMAPX/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68             RCSKW1,RCSKW2
      COMMON / RCSBDWBASMAPX / RCSKW1,RCSKW2
      DATA                     RCSKW1,RCSKW2 /                         '
     .$Source: /fs/hseb/ob81/ohd/ofs/src/block/RCS/bdwbasmapx.f,v $
     . $',                                                             '
     .$Id: bdwbasmapx.f,v 1.20 2007/03/20 17:39:45 dsa Exp $
     . $' /
C    ===================================================================
C

      DATA      PGMNAM  /  'WBASMAPX'    /
      DATA      PGMVRN  /  'ob8.1     '  /
      DATA      PGMVRD  /  '03/20/07'    /
      DATA      PGMSYS  /  'RFS5'        /
      DATA      PGMCMP  /  'F90 '        /
      DATA      MPGMRG  /   0            /

      END
