C MODULE FCBLOCKO
C-----------------------------------------------------------------------
C
      BLOCK DATA FCOBLK
C
C  BLOCK DATA TO SET DIMENSION OF FORECAST COMPONENT ARRAYS USED
C  TO HOLD OLD SEGMENT DEFINITION WHEN REDEFINING A SEGMENT IN FCINIT
C
      INCLUDE 'common/oldc'
      INCLUDE 'common/oldp'
      INCLUDE 'common/oldt'
      INCLUDE 'common/oldts'
      INCLUDE 'common/resetc'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSFCBLOCKO      / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/fcblocko.f,v $
     . $',                                                             '
     .$Id: fcblocko.f,v 1.7 2002/02/11 13:58:36 michaelo Exp $
     . $' /
C    ===================================================================
C
C
      DATA MOLDC/3000/,   OLDC/3000*0.0/
      DATA MOLDP/500000/, OLDP/500000*0.0/
      DATA MOLDT/2000/,   OLDT/2000*0.0/
      DATA MOLDTS/10000/,  OLDTS/10000*0.0/
      DATA MRSTC/3000/,   RESETC/3000*0.0/
C
      END
