C MODULE FCBLOCKA
C-----------------------------------------------------------------------
C
      BLOCK DATA FCABLK
C
C  BLOCK DATA TO SET DIMENSION OF FORECAST COMPONENT ARRAYS
C
      INCLUDE 'common/fc'
      INCLUDE 'common/fd'
      INCLUDE 'common/fp'
      INCLUDE 'common/ft'
      INCLUDE 'common/fts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSFCBLOCKA      / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/fcblocka.f,v $
     . $',                                                             '
     .$Id: fcblocka.f,v 1.8 2001/06/13 09:37:31 mgm Exp $
     . $' /
C    ===================================================================
C
C
      DATA MC/3000/,   C/3000*0.0/
      DATA MD/200000/, D/200000*0.0/
      DATA MP/500000/, P/500000*0.0/
      DATA MT/2000/,   T/2000*0.0/
      DATA MTS/10000/,  TS/10000*0.0/
C
      END
