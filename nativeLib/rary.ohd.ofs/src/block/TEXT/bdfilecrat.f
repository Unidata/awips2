C MODULE BDFILECRAT
C  =====================================================================
C  pgm: BDFILECRAT .. Block data for program name, version, date
C
C  cmt: Created by user "ihfsuser" on 040628 14:11
C  =====================================================================
      BLOCK DATA BDFILECRAT
      common /CMFILECRAT/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68             RCSKW1,RCSKW2
      COMMON / RCSBDFILECRAT / RCSKW1,RCSKW2
      DATA                     RCSKW1,RCSKW2 /                         '
     .$Source: /fs/hseb/ob81/ohd/ofs/src/block/RCS/bdfilecrat.f,v $
     . $',                                                             '
     .$Id: bdfilecrat.f,v 1.20 2007/03/20 16:48:57 dsa Exp $
     . $' /
C    ===================================================================
C

      DATA      PGMNAM  /  'FILECRAT'    /
      DATA      PGMVRN  /  'ob8.3     '  /
      DATA      PGMVRD  /  '01/02/08'    /
      DATA      PGMSYS  /  'RFS5'        /
      DATA      PGMCMP  /  'F90 '        /
      DATA      MPGMRG  /   0            /

      END
