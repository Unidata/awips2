C MEMBER BLCK31
C-----------------------------------------------------------------------
C
      BLOCK DATA BLCK31
C
C  INITIALIZE COMMON BLOCKS FOR SNOW-43 OPERATION
C
      include 'snow43/cupdt31'
      include 'snow43/fskdata'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSBLCK31        / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/blck31.f,v $
     . $',                                                             '
     .$Id: blck31.f,v 1.1 1996/05/07 11:15:54 page Exp $
     . $' /
C    ===================================================================
C
C
C  COMMON BLOCK  /CUPDT31/
      data llfltr,mmfltr,nnfltr/2,1,5/
C
C  COMMON BLOCK  /FSKDATA/
      data nkdv/0/
      data wesnk/10 * -99.0/
      data vwesnk/10 * -99.0/
      data wegain/10 * -99.0/
C
      END
