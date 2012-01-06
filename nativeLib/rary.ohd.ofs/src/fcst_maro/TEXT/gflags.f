C MEMBER GFLAGS
C  (from old member PPGFLAGS)
C
      SUBROUTINE GFLAGS(JRAIN, JFLAG)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/gflags.f,v $
     . $',                                                             '
     .$Id: gflags.f,v 1.1 1995/09/17 19:01:38 dws Exp $
     . $' /
C    ===================================================================
C
C
C.....THIS SUBROUTINE ASSOCIATES AN OBSERVATION FLAG WITH A
C.....PRECIPITATION AMOUNT. THE FLAG IS BLANK IF THE PRECIPITATION IS
C.....OBSERVED, AND IS AN ASTERISK (*) IF THE PRECIPITATION IS AN
C.....ESTIMATE.
C
C.....ARGUMENTS ARE:
C
C.....JRAIN  - THE PRECIPITATION AMOUNT.
C.....JFLAG  - THE FLAG.
C
C.....ORIGINALLY WRITTEN BY:
C
C.....JERRY M. NUNN       WGRFC FT. WORTH, TEXAS       DECEMBER 1986
C
C
      DATA NBLNK, IASTR /1h , 1h*/
C
      JFLAG = NBLNK
      IF(JRAIN .LT. 0) JFLAG = IASTR
      RETURN
      END
