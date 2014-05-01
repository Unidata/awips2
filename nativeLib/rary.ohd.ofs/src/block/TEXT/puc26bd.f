C MEMBER PUC26BD
C --------------------------------------------
      BLOCK DATA PUC26BD

      COMMON/FMT26/P(3),EP(2),TS(4),ETS(3),CA(4),ECA(3)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSPUC26BD       / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/puc26bd.f,v $
     . $',                                                             '
     .$Id: puc26bd.f,v 1.1 1995/09/17 18:41:22 dws Exp $
     . $' /
C    ===================================================================
C
      DATA P  /4H(5HP,4HARMS,4H)   /
      DATA EP /4H(4HE,4HNDP)/
      DATA TS /4H(11H,4HTIME,4H-SER,4HIES)/
      DATA ETS/4H(5HE,4HNDTS,4H)   /
      DATA CA /4H(9HC,4HARRY,4HOVER,4H)   /
      DATA ECA/4H(5HE,4HNDCO,4H)   /

      END
