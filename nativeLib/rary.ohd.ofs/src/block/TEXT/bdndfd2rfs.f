C MODULE BDNDFD2RFS
C  =====================================================================
C  pgm: BDNDFD2RFS .. Block data for program name, version, date
C
C  cmt: Created by user "dsa" on 040916 14:12
C  =====================================================================
      BLOCK DATA BDNDFD2RFS
      common /CMNDFD2RFS/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68             RCSKW1,RCSKW2
      COMMON / RCSBDNDFD2RFS / RCSKW1,RCSKW2
      DATA                     RCSKW1,RCSKW2 /                         '
     .$Source: /fs/hseb/ob81/ohd/ofs/src/block/RCS/bdndfd2rfs.f,v $
     . $',                                                             '
     .$Id: bdndfd2rfs.f,v 1.18 2007/03/20 18:00:25 dsa Exp $
     . $' /
C    ===================================================================
C

      DATA      PGMNAM  /  'NDFD2RFS'    /
      DATA      PGMVRN  /  'ob8.3     '  /
      DATA      PGMVRD  /  '01/02/08'    /
      DATA      PGMSYS  /  'RFS5'        /
      DATA      PGMCMP  /  'F90 '        /
      DATA      MPGMRG  /   0            /

      END
