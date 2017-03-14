C MODULE BDFILESIZE
C  =====================================================================
C  pgm: BDFILESIZE .. Block data for program name, version, date
C
C  cmt: Created by user "ihfsuser" on 040628 14:11
C  =====================================================================
      BLOCK DATA BDFILESIZE
      common /CMFILESIZE/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68             RCSKW1,RCSKW2
      COMMON / RCSBDFILESIZE / RCSKW1,RCSKW2
      DATA                     RCSKW1,RCSKW2 /                         '
     .$Source: /fs/hseb/ob81/ohd/ofs/src/block/RCS/bdfilesize.f,v $
     . $',                                                             '
     .$Id: bdfilesize.f,v 1.20 2007/03/20 17:11:48 dsa Exp $
     . $' /
C    ===================================================================
C

      DATA      PGMNAM  /  'FILESIZE'    /
      DATA      PGMVRN  /  'ob8.1     '  /
      DATA      PGMVRD  /  '03/20/07'    /
      DATA      PGMSYS  /  'RFS5'        /
      DATA      PGMCMP  /  'F90 '        /
      DATA      MPGMRG  /   0            /

      END
