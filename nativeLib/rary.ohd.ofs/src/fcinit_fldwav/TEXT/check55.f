      SUBROUTINE CHECK55(IUSE,LEFT,NERR)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/check55.f,v $
     . $',                                                             '
     .$Id: check55.f,v 1.1 1999/04/23 18:09:24 dws Exp $
     . $' /
C    ===================================================================
C
C
C        THIS SUBROUTINE CHECKS TO SEE IF THE SIZE OF THE Z ARRAY HAS
C        BEEN EXCEEDED.
C
      NERR=0
      IF(IUSE.GT.LEFT) NERR=1
      RETURN
      END
