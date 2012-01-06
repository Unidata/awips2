C MODULE BDMAP
C  =====================================================================
C  pgm: BDMAP .. Block data for program name, version, date
C
C  cmt: Created by user "ihfsuser" on 040628 14:12
C  =====================================================================
      BLOCK DATA BDMAP

      INCLUDE 'upvrsx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68             RCSKW1,RCSKW2
      COMMON / RCSBDMAP      / RCSKW1,RCSKW2
      DATA                     RCSKW1,RCSKW2 /                         '
     .$Source: /fs/hseb/ob81/ohd/calb/src/block/RCS/bdmap.f,v $
     . $',                                                             '
     .$Id: bdmap.f,v 1.18 2007/03/20 17:37:54 dsa Exp $
     . $' /
C    ===================================================================
C

      DATA      PGMNAM  /  'MAP     '    /
      DATA      PGMVRN  /  'ob8.1     '  /
      DATA      PGMVRD  /  '03/20/07'    /
      DATA      PGMSYS  /  'CALB'        /
      DATA      PGMCMP  /  'F90 '        /
      DATA      MPGMRG  /   0            /

      END
