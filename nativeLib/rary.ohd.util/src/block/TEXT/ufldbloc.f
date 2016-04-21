C MODULE UFLDBLOC
C-----------------------------------------------------------------------
C
      BLOCK DATA UFLDBLOC
C
C  INITIALIZE FREE FORMAT FIELD COMMON BLOCK
C
      INCLUDE 'ufreex'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSUFLDBLOC      / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/util/src/block/RCS/ufldbloc.f,v $
     . $',                                                             '
     .$Id: ufldbloc.f,v 1.2 1998/07/02 19:55:02 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA PFCOMA/.TRUE./
      DATA ICDBUF/' '/
      DATA ICDSTR/1/,ICDSTP/72/,IPRCRD/0/,IPRBLN/1/,IPRMPT/0/
      DATA NRDCRD/0/,NPUCRD/0/,IPRTCD/0/,MAXFLD/50/
      DATA ICKDAT/0/,ICDTMP/02/,IOPREP/0/,ICDSPC/1/,ICDQTE/1/
      DATA NFLDNQ/0/
      DATA CMTCHR/'$'/
C
      END
