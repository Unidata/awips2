C MODULE UBLOCKC
C-----------------------------------------------------------------------
C
C  INITIALIZE CALIBRATION SYSTEM COMMON BLOCKS
C
      BLOCK DATA UBLKC
C
      INCLUDE 'uio'
      INCLUDE 'clbcommon/cdbugx'
      INCLUDE 'common/unitno'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSUBLOCKC       / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/calb/src/block/RCS/ublockc.f,v $
     . $',                                                             '
     .$Id: ublockc.f,v 1.5 2001/06/12 15:21:29 aivo Exp $
     . $' /
C    ===================================================================
C
C
C  COMMON BLOCK UIO
      DATA LP/6/,ICD/1/,LPD/6/,LPE/9/,ICDPUN/7/
C
C  COMMON BLOCK CDBUGX
      DATA IOCDBG/6/,ICRWTR/0/,ICRWDB/0/,ICUTTR/0/,ICUTDB/0/
      DATA ICTRCE/0/,ICDBUG/0/
C
C  COMMON BLOCK UNITIO
      DATA fileiotype/' '/,fileiotypes/256*' '/
      DATA igziord / -1 /
      DATA igziowt / -1 /
      DATA iascii  /  1 /
C
      END
