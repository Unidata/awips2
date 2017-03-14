C MODULE PPHASH
C***********************************************************************
C             VERSION:  1.0.0
C                DATE:  11-18-81
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C***********************************************************************
C          DESCRIPTION:
C
C    HASHING ALGORITHM TO GENERATE A NUMERIC CODE FROM THE KEY.
C    KEY IS TIME SERIES ID AND TYPE; THE CODE IS THE RECORD
C    NUMBER FOR THE TIME SERIES INDEX.
C    THE INPUT IS 3 I*4 WORDS BUT WE WILL USE 6 I*2 WORDS.
C***********************************************************************
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IHOLD      I     I     1    SIZE OF THE HASH AREA
C       KEY        I*2   I     6    CONTAINS THE TS ID AND TYPE
C       IHASH      I     O     1    HASHED CODE FOR INDEX RECORD #
C***********************************************************************
      SUBROUTINE PPHASH (IHOLD,KEY,IHASH)

      INTRINSIC   MOD

      INTEGER*2 KEY(6)
      INTEGER   IHOLD,IHASH,KHASH
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/pphash.f,v $
     . $',                                                             '
     .$Id: pphash.f,v 1.2 1999/07/06 15:12:44 page Exp $
     . $' /
C    ===================================================================
C

        KHASH = KEY(1)
        KHASH = KHASH + KEY(2)/4  + KEY(2)*4
        KHASH = KHASH + KEY(3)/8  + KEY(3)*8
        KHASH = KHASH + KEY(4)/16 + KEY(4)*16
        KHASH = KHASH + KEY(5)/32 + KEY(5)*32
        KHASH = KHASH + KEY(6)/64 + KEY(6)*64

        IF (KHASH.LT.0) KHASH = -KHASH
        IHASH = MOD(KHASH,IHOLD) + 1

      RETURN
      END
