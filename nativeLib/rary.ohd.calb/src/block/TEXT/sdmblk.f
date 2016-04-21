C MEMBER SDMBLK
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      BLOCK DATA SDMBLK
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSSDMBLK        / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/calb/src/block/RCS/sdmblk.f,v $
     . $',                                                             '
     .$Id: sdmblk.f,v 1.1 1997/01/23 15:14:26 dws Exp $
     . $' /
C    ===================================================================
C
      DATA IOSDBG/6/,ISTRCE/0/
      END
