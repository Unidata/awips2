C MEMBER MTWT26
C  (from old member FCMTWT26)
C
C DESC DETERMINE NUMERICAL DAY OF WEEK FROM JULIAN DAY AND HOUR
C-------------------------------------------------------------------
      FUNCTION MTWT26(IDAYW,IHRW)
C------------------------------------------------------------------
C  FUNCTION TO RETURN THE DAY OF WEEK GIVEN THE JULIAN DAY AND INTERNAL
C  HOUR. DAY OF WEEK IS NUMERICAL, RANGING FROM 1 TO 7, WITH SUNDAY = 1,
C  THROUGH SATURDAY = 7.
C------------------------------------------------------------------
C  WRITTEN BY JOE OSTROWSKI - HRL - JULY 1983
C------------------------------------------------------------------
C
      INCLUDE 'common/fctime'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/mtwt26.f,v $
     . $',                                                             '
     .$Id: mtwt26.f,v 1.1 1995/09/17 19:05:49 dws Exp $
     . $' /
C    ===================================================================
C
C
C  THE JULIAN DAY MUST BE INCREASED BY ONE IF THE LOCAL HOUR EQUIVALENT
C  TO THE INTERNAL HOUR IS GREATER THAN 24.
C
      LOCLHR = IHRW + LOCAL + NOUTDS
      KDR = IDAYW
      IF (LOCLHR.GT.24) KDR = KDR + 1
C
C  JANUARY 1, 1900 (JULIAN DAY 1) WAS A MONDAY
C   WE NEED TO ADD ONE TO THE MODULO REMAINDER TO GET THE PROPER DAY OF
C   WEEK.
C
      MTWT26 = MOD(KDR,7) + 1
C
      RETURN
      END
