C MODULE UXPPBLOC
C-----------------------------------------------------------------------
C
       BLOCK DATA PPIBLK
C
C  INITIALIZATION ROUTINE FOR PREPROCESSOR PARAMETRIC DATA BASE
C  COMMON BLOCKS
C
      INCLUDE 'pppcommon/ppxctl'
      INCLUDE 'pppcommon/ppdtdr'
      INCLUDE 'pppcommon/ppmctl'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSUXPPBLOC      / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/uxppbloc.f,v $
     . $',                                                             '
     .$Id: uxppbloc.f,v 1.3 1999/07/06 16:53:02 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA IPDTDR/4HGENL,1,6*0,      
     *            4HUSER,1,3*0,1,2*0,
     *            4HNTWK,1,3*0,1,2*0, 
     *            4HURRS,1,3*0,1,2*0,
     *            4HSTBN,1,3*0,1,2*0, 
     *            4HORRS,1,3*0,1,2*0,
     *            4HORDR,1,3*0,1,2*0, 
     *            4HOP24,1,3*0,1,2*0,
     *            4HOPVR,1,3*0,1,2*0, 
     *            4HOT24,1,3*0,1,2*0,
     *            4HOE24,1,3*0,1,2*0,
     *            4HGBOX,1,6*0,       
     *            4HGMDR,1,3*0,1,2*0,
     *            4HGP24,1,3*0,1,2*0, 
     *            4HOG24,1,3*0,1,2*0,
     *            4HRFRO,1,6*0,       
     *            4HFFG ,1,6*0,
     *            4HASSM,1,6*0,
     *            4HPCPN,2,6*0,       
     *            4HTEMP,2,6*0,
     *            4HPE  ,2,6*0,       
     *            4HRRS ,2,6*0,
     *            4HMAP ,3,6*0,       
     *            4HMAPS,3,6*0,
     *            4HMAPE,3,6*0,       
     *            4HMAT ,3,6*0,
     *            4HMARO,3,6*0,       
     *            4HMAPX,3,6*0,
     *            4HMPCO,4,6*0,       
     *            4HMPFO,4,6*0,
     *            4HFMPO,4,3*0,1,2*0,
     *            4HMXCO,4,3*0,1,2*0, 
     *            4HXGRD,4,3*0,1,2*0,       
     *            4HBASN,5,6*0,
     *            4HCHAR,5,3*0,1,2*0, 
     *            4HMMMT,5,3*0,1,2*0,
     *            112*0/
C
      DATA IPMCTL/0,1,0,5*0,
     *            0,1,0,5*0,
     *            0,1,0,5*0,
     *            0,1,0,5*0,
     *            0,1,0,5*0,
     *            32*0/
C
      DATA MXPXRC/0/
      DATA MXPTYP/50/
      DATA NMPTYP/36/
      DATA NMPFIL/5/
      DATA IPXRC1/103/
      DATA USERPP/2*4H    /
      DATA IPXDUM/0/
C
      END
