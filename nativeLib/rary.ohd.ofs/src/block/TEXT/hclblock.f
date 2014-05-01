C MEMBER HCLBLOCK
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 01/24/95.08:43:56 BY $WC20SV
C
C @PROCESS LVL(77)
C
      BLOCK DATA HCLBLK
C
C
C  INITIALIZE HYDROLOGIC COMMAND LANGUAGE COMMON BLOCKS
C
C
      INCLUDE 'hclcommon/hprflg'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hdatas'
      INCLUDE 'hclcommon/hwords'
      INCLUDE 'hclcommon/hword2'
      INCLUDE 'hclcommon/hword3'
      INCLUDE 'hclcommon/hgtech'
      INCLUDE 'hclcommon/htechn'
      INCLUDE 'hclcommon/hgargm'
      INCLUDE 'hclcommon/hargmn'
      INCLUDE 'hclcommon/hptray'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSHCLBLOCK      / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/hclblock.f,v $
     . $',                                                             '
     .$Id: hclblock.f,v 1.1 1995/09/17 18:41:18 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C  COMMON /HPRFLG/
      DATA IPRFLG/1/
C
C  COMMON /HUNITS/
      DATA KINDXL/12/,KINDXG/10/,KDEFNL/13/,KDEFNG/11/,KHCARD/48/
      DATA KHDFLT/49/,KTMPSV/88/,KHCLPR/00/,KLDFGD/14/,KPROCC/89/
C
C  COMMON /HDATAS/
      DATA INDX/4HINDX/,DEFN/4HDEFN/,DFLT/4HDFLT/,ROPT/4HROPT/
      DATA LRECLI/4/,LRECLH/16/
C
C  COMMON /HWORDS/
      DATA LACOLN/4HA:  /,LARGCL/4HARG:/,LFUN/4HFUN /,LETN/4HN   /
      DATA LNAME /4HNAME/,LPASS /4HPASS/,LSUB/4HSUB /,LTECH/4HTECH/
      DATA LETY  /4HY   /,LLOCAL(1)/4HLOCA/,LLOCAL(2)/4HL   /
      DATA LGLOBL(1)/4HGLOB/,LGLOBL(2)/4HAL  /,LLPARN/4H(   /
      DATA LUNIV(1)/4HUNIV/,LUNIV(2)/4HERSA/,LSUBN/4HSUBN/,LETA/4HA   /
      DATA LRPARN/4H)   /
C
C  COMMON /HWORD2/
      DATA LEND/4HEND /,LINCLD(1)/4HINCL/,LINCLD(2)/4HUDE /
      DATA LALL/4HALL /,LNONE/4HNONE/,LYES/4HYES /,LNO/4HNO  /
      DATA LETI/4HI   /,LETR/4HR   /,LETL/4HL   /,LETD/4HD   /
C
C  COMMON /HWORD3/
      DATA LTRUE(1)/4H.TRU/,LTRUE(2)/4HE.  /,LFALSE(1)/4H.FAL/
      DATA LFALSE(2)/4HSE. /,LSTAR/4H*   /
C
C  COMMON /HGTECH/
      DATA MGTECH/200/
C
C  COMMON /HTECHN/
      DATA MTECH/50/
C
C  COMMON /HGARGM/
      DATA MGARG/1500/
C
C  COMMON /HARGMN/
      DATA MARG/500/
C
C  COMMON /HPTRAY/
      DATA MPTRAY/400/
      DATA NPTRAY/0/
C
C
      END
