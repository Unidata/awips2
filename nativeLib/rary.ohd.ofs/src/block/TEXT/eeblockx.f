C MODULE EEBLOCKX
C-----------------------------------------------------------------------
C
      BLOCK DATA EEXBLK
C
C      INCLUDE 'common/eunit'
      INCLUDE 'common/elimit'
      INCLUDE 'common/ep'
      INCLUDE 'common/ets'
      INCLUDE 'common/esp'
      INCLUDE 'common/ea'
      INCLUDE 'common/efdate'
      INCLUDE 'common/eperm'
      INCLUDE 'common/escrat'
      INCLUDE 'common/evar'
      INCLUDE 'common/eoutpt'
      INCLUDE 'common/efutp'
      INCLUDE 'common/etsdat'
      INCLUDE 'common/eyrwt'
      INCLUDE 'common/etsunt'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSEEBLOCKX      / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/eeblockx.f,v $
     . $',                                                             '
     .$Id: eeblockx.f,v 1.5 2003/03/14 17:44:34 dws Exp $
     . $' /
C    ===================================================================
C
C
C      DATA KEPARM,KESCRA,KEPERM,KESTBL/96,90,0,0,0,0,0,97/
      DATA MAXWIN,MAXVAR,MAXDSP/5,10,5/
      DATA MPESP/1000/
      DATA MTSESP/10000/
      DATA MSPESP/1/
      DATA MA/5000/
      DATA IFDATE/0,0,0,0,0/
      DATA IPERM,MXLRCP,MXNRCP/0,124,124,124,124,124,520,520,520,520,
     1 520/
      DATA MXINDX,NUSE,NXINDX,MXNRCS,MXLRCS/1000,0,1,20800,124/
      DATA NVPV/2,2,1,1,2,2,1,2,0,0/
      DATA IWS,IFPLT,NCALL/0,1,0/
      DATA IFUTP/0/
      DATA LTSDAT/124/
      DATA MAXYR/50/
      DATA MAXETS/100/
      DATA NEPTS/0/
      data indirlen/0/
      data ioutdirlen/0/
C
      END
