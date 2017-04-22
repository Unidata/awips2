C MODULE BDZGRID
C  =====================================================================
C  pgm: BDZGRID .. Block data for program name, version, date
C
C  cmt: Created by user "ihfsuser" on 040628 14:13
C  =====================================================================
      BLOCK DATA BDZGRID

      COMMON /CMZGRID/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68             RCSKW1,RCSKW2
      COMMON / RCSBDZGRID    / RCSKW1,RCSKW2
      DATA                     RCSKW1,RCSKW2 /                         '
     .$Source: /fs/hseb/ob81/ohd/ffg/src/block/RCS/bdzgrid.f,v $
     . $',                                                             '
     .$Id: bdzgrid.f,v 1.18 2007/03/20 18:00:49 dsa Exp $
     . $' /
C    ===================================================================
C

      DATA      PGMNAM  /  'ZGRID   '    /
      DATA      PGMVRN  /  'ob8.1     '  /
      DATA      PGMVRD  /  '03/20/07'    /
      DATA      PGMSYS  /  'FFG '        /
      DATA      PGMCMP  /  'F90 '        /
      DATA      MPGMRG  /   0            /

      END
