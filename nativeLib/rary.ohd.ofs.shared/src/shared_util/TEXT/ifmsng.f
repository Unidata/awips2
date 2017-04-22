C MEMBER IFMSNG
C  (from old member FCIFMSNG)
C
      FUNCTION IFMSNG(VALUE)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/ifmsng.f,v $
     . $',                                                             '
     .$Id: ifmsng.f,v 1.1 1995/09/17 19:24:07 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     INTEGER FUNCTION TO CHECK FOR MISSING DATA VALUES.
C
C     RETURN VALUE =1  IF VALUE IS WITH + OR - 0.01 OF
C                      THE FORECAST COMPONENT MISSING DATA
C                      VALUE OF -999.0 OR THE MISSING TIME
C                      DISTRIBUTION VALUE OF -998.0.
C                  =0  OTHERWISE
C.......................................
C     FUNCTION INITIALLY WRITTEN BY. . .
C        ERIC ANDERSON - HRL   OCT. 1979
C.......................................
      IFMSNG=0
      IF ((VALUE.LT.-998.99).AND.(VALUE.GT.-999.01)) IFMSNG=1
      IF ((VALUE.LT.-997.99).AND.(VALUE.GT.-998.01)) IFMSNG=1
C.......................................
      RETURN
      END
