C MEMBER PCKDLT
C  (from old member PRDCHCK1)
C*******************************************************************
C
C MEMBER PRDCHCK1
C
C********************************************************************
C
C PROCESSED DATA BASE CARD CHECK ROUTINES
C
C******************************************************************
      SUBROUTINE PCKDLT(ITSTEP,ISMALL,ISTAT)
C
C THIS ROUTINE WILL CHECK FOR A VALID DELTA T ALSO
C IT MUST BE GREATER THAN OR EQUAL TO ISMALL
C
      DIMENSION IDLTAT(8)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/pckdlt.f,v $
     . $',                                                             '
     .$Id: pckdlt.f,v 1.1 1995/09/17 18:45:31 dws Exp $
     . $' /
C    ===================================================================
C
      DATA IDLTAT/1,2,3,4,6,8,12,24/
C
      ISTAT=0
      DO 100 I=1,8
      IF(ITSTEP.EQ.IDLTAT(I)) GO TO 200
100   CONTINUE
      GO TO 300
200   IF(ITSTEP.GE.ISMALL) GO TO 999
300   ISTAT=1
999   RETURN
      END
