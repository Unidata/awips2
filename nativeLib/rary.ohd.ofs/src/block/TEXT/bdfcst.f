C MODULE BDFCST
C  =====================================================================
C  pgm: BDFCST .. Block data for program name, version, date
C
C  cmt: Created by user "ihfsuser" on 040628 14:11
C  =====================================================================
      BLOCK DATA BDFCST
      common /CMFCST/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68             RCSKW1,RCSKW2
      COMMON / RCSBDFCST     / RCSKW1,RCSKW2
      DATA                     RCSKW1,RCSKW2 /                         '
     .$Source: /fs/hseb/ob81/ohd/ofs/src/block/RCS/bdfcst.f,v $
     . $',                                                             '
     .$Id: bdfcst.f,v 1.53 2007/03/20 18:01:14 dsa Exp $
     . $' /
C    ===================================================================
C

      DATA      PGMNAM  /  'FCST    '    /
      DATA      PGMVRN  /  'ob8.3     '  /
      DATA      PGMVRD  /  '10/24/07'    /
      DATA      PGMSYS  /  'RFS5'        /
      DATA      PGMCMP  /  'F90 '        /
      DATA      MPGMRG  /   0            /

      END
