C MODULE BDPPINIT
C  =====================================================================
C  pgm: BDPPINIT .. Block data for program name, version, date
C
C  cmt: Created by user "ihfsuser" on 040628 14:23
C  =====================================================================
      BLOCK DATA BDPPINIT
      common /CMPPINIT/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68             RCSKW1,RCSKW2
      COMMON / RCSBDPPINIT   / RCSKW1,RCSKW2
      DATA                     RCSKW1,RCSKW2 /                         '
     .$Source: /fs/hseb/ob81/ohd/ofs/src/block/RCS/bdppinit.f,v $
     . $',                                                             '
     .$Id: bdppinit.f,v 1.23 2007/03/20 17:15:22 dsa Exp $
     . $' /
C    ===================================================================
C

      DATA      PGMNAM  /  'PPINIT  '    /
      DATA      PGMVRN  /  'ob8.1     '  /
      DATA      PGMVRD  /  '03/20/07'    /
      DATA      PGMSYS  /  'RFS5'        /
      DATA      PGMCMP  /  'F90 '        /
      DATA      MPGMRG  /   0            /

      END
