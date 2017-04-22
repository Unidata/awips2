C MODULE BDSHEFPOST
C  =====================================================================
C  pgm: BDSHEFPOST .. Block data for program name, version, date
C
C  cmt: Created by user "ihfsuser" on 040628 14:12
C  =====================================================================
      BLOCK DATA BDSHEFPOST
      common /CMSHEFPOST/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68             RCSKW1,RCSKW2
      COMMON / RCSBDSHEFPOST / RCSKW1,RCSKW2
      DATA                     RCSKW1,RCSKW2 /                         '
     .$Source: /fs/hseb/ob81/ohd/ofs/src/block/RCS/bdshefpost.f,v $
     . $',                                                             '
     .$Id: bdshefpost.f,v 1.20 2007/03/20 17:33:38 dsa Exp $
     . $' /
C    ===================================================================
C

      DATA      PGMNAM  /  'SHEFPOST'    /
      DATA      PGMVRN  /  'ob8.1     '  /
      DATA      PGMVRD  /  '03/20/07'    /
      DATA      PGMSYS  /  'RFS5'        /
      DATA      PGMCMP  /  'F90 '        /
      DATA      MPGMRG  /   0            /

      END
