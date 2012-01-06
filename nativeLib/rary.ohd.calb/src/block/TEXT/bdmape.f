C MODULE BDMAPE
C  =====================================================================
C  pgm: BDMAPE .. Block data for program name, version, date
C
C  cmt: Created by user "ihfsuser" on 040628 14:12
C  =====================================================================
      BLOCK DATA BDMAPE

C      INCLUDE 'upvrsx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68             RCSKW1,RCSKW2
      COMMON / RCSBDMAPE     / RCSKW1,RCSKW2
      DATA                     RCSKW1,RCSKW2 /                         '
     .$Source: /fs/hseb/ob81/ohd/calb/src/block/RCS/bdmape.f,v $
     . $',                                                             '
     .$Id: bdmape.f,v 1.17 2007/03/20 17:39:21 dsa Exp $
     . $' /
C    ===================================================================
C

      DATA      PGMNAM  /  'MAPE    '    /
      DATA      PGMVRN  /  'ob8.1     '  /
      DATA      PGMVRD  /  '03/20/07'    /
      DATA      PGMSYS  /  'CALB'        /
      DATA      PGMCMP  /  'F90 '        /
      DATA      MPGMRG  /   0            /

      END
