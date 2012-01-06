C MODULE BDPRODGEN
C  =====================================================================
C  pgm: BDPRODGEN .. Block data for program name, version, date
C
C  cmt: Created by user "ihfsuser" on 040628 14:13
C  =====================================================================
      BLOCK DATA BDPRODGEN

      COMMON /CMPRODGEN/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68             RCSKW1,RCSKW2
      COMMON / RCSBDPRODGEN  / RCSKW1,RCSKW2
      DATA                     RCSKW1,RCSKW2 /                         '
     .$Source: /fs/hseb/ob81/ohd/ffg/src/block/RCS/bdprodgen.f,v $
     . $',                                                             '
     .$Id: bdprodgen.f,v 1.20 2007/03/20 17:17:41 dsa Exp $
     . $' /
C    ===================================================================
C

      DATA      PGMNAM  /  'PRODGEN '    /
      DATA      PGMVRN  /  'ob8.3     '  /
      DATA      PGMVRD  /  '01/02/08'    /
      DATA      PGMSYS  /  'FFG '        /
      DATA      PGMCMP  /  'F90 '        /
      DATA      MPGMRG  /   0            /

      END
