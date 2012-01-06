C MODULE BDLOOKNSET
C  =====================================================================
C  pgm: BDLOOKNSET .. Block data for program name, version, date
C
C  cmt: Created by user "ihfsuser" on 040628 14:12
C  =====================================================================
      BLOCK DATA BDLOOKNSET

      INCLUDE 'upvrsx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68             RCSKW1,RCSKW2
      COMMON / RCSBDLOOKNSET / RCSKW1,RCSKW2
      DATA                     RCSKW1,RCSKW2 /                         '
     .$Source: /fs/hseb/ob81/ohd/util/src/block/RCS/bdlooknset.f,v $
     . $',                                                             '
     .$Id: bdlooknset.f,v 1.15 2007/03/20 17:36:29 dsa Exp $
     . $' /
C    ===================================================================
C

      DATA      PGMNAM  /  'LOOKNSET'    /
      DATA      PGMVRN  /  'ob8.1     '  /
      DATA      PGMVRD  /  '03/20/07'    /
      DATA      PGMSYS  /  'UTIL'        /
      DATA      PGMCMP  /  'F90 '        /
      DATA      MPGMRG  /   0            /

      END
