C MODULE BDOPT3
C  =====================================================================
C  pgm: BDOPT3 .. Block data for program name, version, date
C
C  cmt: Created by user "ihfsuser" on 040628 14:12
C  =====================================================================
      BLOCK DATA BDOPT3

C      INCLUDE 'upvrsx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68             RCSKW1,RCSKW2
      COMMON / RCSBDOPT3     / RCSKW1,RCSKW2
      DATA                     RCSKW1,RCSKW2 /                         '
     .$Source: /fs/hseb/ob81/ohd/calb/src/block/RCS/bdopt3.f,v $
     . $',                                                             '
     .$Id: bdopt3.f,v 1.42 2007/03/20 16:44:18 dsa Exp $
     . $' /
C    ===================================================================
C

      DATA      PGMNAM  /  'OPT3    '    /
      DATA      PGMVRN  /  'ob8.3     '  /
      DATA      PGMVRD  /  '10/24/07'    /
      DATA      PGMSYS  /  'CALB'        /
      DATA      PGMCMP  /  'F90 '        /
      DATA      MPGMRG  /   0            /

      END
