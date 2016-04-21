C MODULE BDFFGUID
C  =====================================================================
C  pgm: BDFFGUID .. Block data for program name, version, date
C
C  cmt: Created by user "ihfsuser" on 040628 14:12
C  =====================================================================
      BLOCK DATA BDFFGUID

      COMMON /CMFFGUID/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68             RCSKW1,RCSKW2
      COMMON / RCSBDFFGUID   / RCSKW1,RCSKW2
      DATA                     RCSKW1,RCSKW2 /                         '
     .$Source: /fs/hseb/ob81/ohd/ffg/src/block/RCS/bdffguid.f,v $
     . $',                                                             '
     .$Id: bdffguid.f,v 1.20 2007/03/20 16:35:28 dsa Exp $
     . $' /
C    ===================================================================
C

      DATA      PGMNAM  /  'FFGUID  '    /
      DATA      PGMVRN  /  'ob8.1     '  /
      DATA      PGMVRD  /  '03/20/07'    /
      DATA      PGMSYS  /  'FFG '        /
      DATA      PGMCMP  /  'F90 '        /
      DATA      MPGMRG  /   0            /

      END
