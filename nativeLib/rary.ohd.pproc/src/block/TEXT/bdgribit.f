C MODULE BDGRIBIT
C  =====================================================================
C  pgm: BDGRIBIT .. Block data for program name, version, date
C
C  cmt: Created by user "ihfsuser" on 040628 14:12
C  =====================================================================
      BLOCK DATA BDGRIBIT

      INCLUDE 'upvrsx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68             RCSKW1,RCSKW2
      COMMON / RCSBDGRIBIT   / RCSKW1,RCSKW2
      DATA                     RCSKW1,RCSKW2 /                         '
     .$Source: /fs/hseb/ob83/ohd/pproc/src/block/RCS/bdgribit.f,v $
     . $',                                                             '
     .$Id: bdgribit.f,v 1.1 2006/10/19 16:06:04 dsa Exp millerd $
     . $' /
C    ===================================================================
C

      DATA      PGMNAM  /  'GRIBIT  '    /
      DATA      PGMVRN  /  'ob8.3     '  /
      DATA      PGMVRD  /  '11/14/07'    /
      DATA      PGMSYS  /  'GRIB'        /
      DATA      PGMCMP  /  'F90 '        /
      DATA      MPGMRG  /   0            /

      END
