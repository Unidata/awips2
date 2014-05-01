C MODULE BDREORDER
C  =====================================================================
C  pgm: BDREORDER .. Block data for program name, version, date
C
C  cmt: Created by user "ihfsuser" on 040628 14:11
C  =====================================================================
      BLOCK DATA BDREORDER
      common /CMREORDER/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68             RCSKW1,RCSKW2
      COMMON / RCSBDREORDER  / RCSKW1,RCSKW2
      DATA                     RCSKW1,RCSKW2 /                         '
     .$Source: /fs/hseb/ob81/ohd/ofs/src/block/RCS/bdreorder.f,v $
     . $',                                                             '
     .$Id: bdreorder.f,v 1.24 2007/03/20 17:28:32 dsa Exp $
     . $' /
C    ===================================================================
C

      DATA      PGMNAM  /  'REORDER '    /
      DATA      PGMVRN  /  'ob8.3     '  /
      DATA      PGMVRD  /  '01/02/08'    /
      DATA      PGMSYS  /  'RFS5'        /
      DATA      PGMCMP  /  'F90 '        /
      DATA      MPGMRG  /   0            /

      END
