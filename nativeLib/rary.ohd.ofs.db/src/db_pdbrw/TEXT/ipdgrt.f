C MEMBER IPDGRT
C  (from old member PDPDSTAR)
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION IPDGRT (VAL,RRBUF)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/ipdgrt.f,v $
     . $',                                                             '
     .$Id: ipdgrt.f,v 1.1 1995/09/17 18:43:25 dws Exp $
     . $' /
C    ===================================================================
C
C
C  THIS FUNCTION WILL TAKE CARE OF PROBLEMS COMPARING NEGATIVE
C  REAL VALUES IN INTEGER WORDS BY ACTUALLY DOING REAL COMPARISIONS
C
      IF (VAL.GT.RRBUF) IPDGRT=1
      IF (VAL.LT.RRBUF) IPDGRT=-1
      IF (VAL.EQ.RRBUF) IPDGRT=0
C
      RETURN
C
      END
