C MEMBER DABLOCK
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 01/21/94.11:56:14 BY $WC20SV
C
C @PROCESS LVL(77)
C
      BLOCK DATA DABLK
C
C
C  INITIALIZE ASAP COMMON BLOCKS
C
C
      INCLUDE 'dacommon/daious'
      INCLUDE 'dacommon/dadbgx'
      INCLUDE 'dacommon/daprdc'
      INCLUDE 'dacommon/daupct'
      INCLUDE 'dacommon/daurec'
      INCLUDE 'dacommon/darpts'
      INCLUDE 'dacommon/dausrs'
      INCLUDE 'dacommon/dausrc'
      INCLUDE 'dacommon/daasap'
      INCLUDE 'dacommon/xfunts'
      INCLUDE 'dacommon/usratr'
      INCLUDE 'dacommon/wrtout'
      INCLUDE 'dacommon/nmcinf'
      INCLUDE 'dacommon/produx'
      INCLUDE 'dacommon/xfdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSDABLOCK       / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/dablock.f,v $
     . $',                                                             '
     .$Id: dablock.f,v 1.1 1995/09/17 18:41:05 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C  COMMON /DAIOUS/
      DATA RAWCTL/23/
      DATA ASPUNT/03/
      DATA ASPCTL/25/
C
C  COMMON /DADBGX/
      DATA IODADB/6/,IDATR/0/,IDADB/0/
      DATA MDBUSR/15/,MDBFLG/7/,NDBUSR/0/
C
C  COMMON /DAUPCT/
      DATA IUPDAT/0/,NUDEF/0/,NURDEF/0/,NUDEL/0/,NPDEF/0/,NPRDEF/0/
      DATA NPDEL/0/,NUCNT/0/,NUADD/0/,NDISAB/0/,NENABL/0/,MAXCNT/500/
C
C  COMMON /DARPTS/
      DATA NRPTS/99/
C
C  COMMON /DAPRDC/
      DATA MXPROD/1557/
C
C  COMMON /DAUREC/
      DATA MXUREC/60/
C
C  COMMON /DAUSRS/
      DATA MXUSRS/15/
      DATA NMUSRS/0/
C
C  COMMON /DAUSRC/
      DATA MXUSRC/25/
      DATA NMUSRC/0/
C
C  COMMON /DAASAP/
      DATA MXASAP/15/
      DATA NMASAP/0/
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  COMMON /XFUNTS/
      DATA INMCH/0/
      DATA INMCC/0/
      DATA IRFCC/23/
      DATA IRFCP/0/
      DATA IMSGL/0/
      DATA IAFS/26/
      DATA IDBG/6/
C
C  COMMON /USRATR/
      DATA MAXRFC/15/
C
C  COMMON /WRTOUT/
      DATA LRECP/0/
      DATA NRECP/0/
C
C  COMMON /NMCINF/
      DATA NMCNAM/'HYSCTL  '/
      DATA ITYPE/38,08/
      DATA NUMTYP/2/
      DATA ICRAW/18/
C
C  COMMON /PRODUX/
      DATA MAXPRD/500/
      DATA NPROD/0/
      DATA MAXDST/5/
C
C  COMMON /XFDBUG/
      DATA IXFTR/0/
      DATA IXFDB/0/
C
      END
