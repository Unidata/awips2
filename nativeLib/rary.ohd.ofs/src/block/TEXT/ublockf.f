C MODULE UBLOCKF
C-----------------------------------------------------------------------
C
      BLOCK DATA UBLKF
C
C  INITIALIZE FORECAST SYSTEM UTILITY COMMON BLOCKS
C
      INCLUDE 'udsi'
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'udatas'
      INCLUDE 'uunits'
      INCLUDE 'ucommon/uordrx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSUBLOCKF       / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/ublockf.f,v $
     . $',                                                             '
     .$Id: ublockf.f,v 1.3 2001/06/12 18:52:00 dws Exp $
     . $' /
C    ===================================================================
C
C
C  COMMON BLOCK UDSI
      DATA NOBUG/0/
C
C  COMMON BLOCK UIO
      DATA LP/6/
      DATA ICD/5/
      DATA LPD/6/
      DATA LPE/6/
      DATA ICDPUN/7/
C
C  COMMON BLOCK UDATAS
      DATA IBLNK/4H    /
      DATA ZAPR/-999./
      DATA IDSI/4HDSI /
      DATA IDOLR/4H$   /
      DATA IZAP/-999/
      DATA IASTR/4H*   /
      DATA COMMA/4H,   /
      DATA IEQUAL/4H=   /
      DATA IPOUND/4H#   /
      DATA IPRCNT/4H%   /
C
C  COMMON BLOCK UUNITS
      DATA KDTYPE/3/
      DATA KUPARM/4/
      DATA KDCONV/0/
      DATA KDUMUU/3*0/
C
C  COMMON BLOCK UDEBUG
      DATA IOGDB/6/
      DATA IUTLTR/0/,IUTLDB/0/
      DATA IDETR/0/,IDEDB/0/
      DATA IHCLTR/0/,IHCLDB/0/
      DATA IPDTR/0/,IPDDB/0/
      DATA IPPTR/0/,IPPDB/0/
      DATA IPRTR/0/,IPRDB/0/
      DATA IDBDUM/4*0/
C
C  COMMON BLOCK UORDRX
      DATA IAMORD/0/
      DATA IORDER/0/
C
C
      END
C
C-----------------------------------------------------------------------
C
      INCLUDE 'uduntb'
