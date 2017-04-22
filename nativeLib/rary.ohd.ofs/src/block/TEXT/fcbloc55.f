C MODULE FCBLOC55
C-----------------------------------------------------------------------
C
      BLOCK DATA FCBLOC55
C
C  BLOCK DATA FOR OPERATION 55 - FLDWAV
C
      INCLUDE 'common/opfil55'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
cfan multiple definition of 'rcsfcblocka'
c     COMMON / RCSFCBLOCKA      / RCSKW1,RCSKW2
      COMMON / RCSFCBLOCK55     / RCSKW1,RCSKW2
cfan
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/fcbloc55.f,v $
     . $',                                                             '
     .$Id: fcbloc55.f,v 1.3 2004/02/04 19:54:17 dsa Exp $
     . $' /
C    ===================================================================
C
C
      DATA IOPFIL55/0/
C  following added by jls 1/30/04      
      DATA IOPFMP55/0/      
C
      END
