C MODULE IFBUG
C-----------------------------------------------------------------------
C
C  FUNCTION TO CHECK IF SYSTEM DEBUG CODE SPECIFIED.
C
C  FUNCTION IS SET TO 1 IF CODE SPECIFIED, 0 IF NOT.
C
      FUNCTION IFBUG (NAME)
C
      INCLUDE 'common/sysbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/ifbug.f,v $
     . $',                                                             '
     .$Id: ifbug.f,v 1.3 2001/06/12 15:39:02 aivo Exp $
     . $' /
C    ===================================================================
C
C
      IF (IALL.EQ.1) GO TO 20
C
      IFBUG=0
C
      IF (NDEBGS.EQ.0) GO TO 30
C
      DO 10 I=1,NDEBGS
         IF (NAME.EQ.IDEBGS(I)) GO TO 20
10       CONTINUE
      GO TO 30
C
20    IFBUG=1
C
30    RETURN
C
      END
