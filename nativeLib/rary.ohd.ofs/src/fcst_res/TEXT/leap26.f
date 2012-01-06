C MEMBER LEAP26
C  (from old member FCLEAP26)
C
C DESC FUNCTION TO SEE IF JULIAN DATE AND HOUR FALL WITHIN LEAP YEAR
C--------------------------------------------------------------------
      FUNCTION LEAP26(IDAYL,IHRL)
C--------------------------------------------------------------------
C  FUNCTION TO DETERMINE IF A JULIAN DATE AND HOUR FALL WITHIN A LEAP
C  YEAR.  LEAP26 = 0, NOT A LEAP YEAR
C         LEAP26 = 1, IS A LEAP YEAR.
C--------------------------------------------------------------------
C  WRITTEN BY - JOE OSTROWSKI - HRL - JULY 1983
C--------------------------------------------------------------------
C
      INCLUDE 'common/fctime'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/leap26.f,v $
     . $',                                                             '
     .$Id: leap26.f,v 1.1 1995/09/17 19:05:47 dws Exp $
     . $' /
C    ===================================================================
C
C
C  GET YEAR FROM THE JULIAN DATE
C
      CALL MDYH1(IDAYL,IHRL,IMO,IDAY,IYR,IHRM,NLSTZ,NOUTDS,TZC)
C
C  YEAR IS A LEAP YEAR IF IT IS A MULTIPLE OF FOUR, BUT NOT A CENTURY
C  YEAR UNLESS THE YEAR IS A MULTIPLE OF 400.
C
      LEAP26 = 0
      IF (MOD(IYR,4).NE.0) GO TO 99
      IF (MOD(IYR,100).EQ.0.AND.MOD(IYR,400).NE.0) GO TO 99
C
C  ALL TESTS PASSED, IT IS A LEAP YEAR.
C
      LEAP26 = 1
C
   99 CONTINUE
      RETURN
      END
