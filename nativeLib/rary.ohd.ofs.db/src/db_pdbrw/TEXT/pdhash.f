C MODULE PDHASH
C-----------------------------------------------------------------------
C  THIS ROUTINE WILL COMPUTE A HASH VALUE.
C  THE INPUT IS 2 I*4 WORDS BUT WE WILL USE 4 I*2 WORDS.
C
      SUBROUTINE PDHASH (IHOLD,KEY,IHASH)

      INTRINSIC   MOD

      INTEGER*2 KEY(4)
      INTEGER   IHOLD,IHASH,KHASH
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdhash.f,v $
     . $',                                                             '
     .$Id: pdhash.f,v 1.2 1999/07/06 15:43:38 page Exp $
     . $' /
C    ===================================================================
C

        KHASH = KEY(1)
        KHASH = KHASH + KEY(2)/4  + KEY(2)*4
        KHASH = KHASH + KEY(3)/8  + KEY(3)*8
        KHASH = KHASH + KEY(4)/16 + KEY(4)*16

        IF (KHASH.LT.0) KHASH = -KHASH
        IHASH = MOD(KHASH,IHOLD) + 1

      RETURN
      END
