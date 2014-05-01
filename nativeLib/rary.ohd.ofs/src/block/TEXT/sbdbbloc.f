C MEMBER SBDBBLOC
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 03/24/95.11:20:50 BY $WC20SV
C
C @PROCESS LVL(77)
C
      BLOCK DATA SBDBLK
C
C
C  INITIALIZATION ROUTINE FOR COMMON PPINIT DEBUG COMMON BLOCK
C
C
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSSBDBBLOC      / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/sbdbbloc.f,v $
     . $',                                                             '
     .$Id: sbdbbloc.f,v 1.1 1995/09/17 18:41:23 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C  COMMON BLOCK  /SUDBGX/
      DATA IOSDBG/6/,ISTRCE/0/,NSDBUG/0/,ISALL/0/
      DATA ISDBUG/0/,ISDBGL/0/
      DATA SDBUG/20*' '/,SDBUG2/20*' '/
      DATA ISLTRC/20*0/,ISLDBG/20*0/
      DATA SDRTN2/20*' '/
      DATA ISDRTN/1/
C
C
      END
