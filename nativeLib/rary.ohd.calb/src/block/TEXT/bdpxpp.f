C MODULE BDPXPP
C  =====================================================================
C  pgm: BDPXPP .. Block data for program name, version, date
C
C  cmt: Created by user "ihfsuser" on 040628 14:12
C  =====================================================================
      BLOCK DATA BDPXPP

C      INCLUDE 'upvrsx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68             RCSKW1,RCSKW2
      COMMON / RCSBDPXPP     / RCSKW1,RCSKW2
      DATA                     RCSKW1,RCSKW2 /                         '
     .$Source: /fs/hseb/ob81/ohd/calb/src/block/RCS/bdpxpp.f,v $
     . $',                                                             '
     .$Id: bdpxpp.f,v 1.19 2007/03/20 17:22:39 dsa Exp $
     . $' /
C    ===================================================================
C

      DATA      PGMNAM  /  'PXPP    '    /
      DATA      PGMVRN  /  'ob8.1     '  /
      DATA      PGMVRD  /  '03/20/07'    /
      DATA      PGMSYS  /  'CALB'        /
      DATA      PGMCMP  /  'F90 '        /
      DATA      MPGMRG  /   0            /

      END
