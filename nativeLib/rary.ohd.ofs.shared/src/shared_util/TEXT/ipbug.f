C MODULE IPBUG
C-----------------------------------------------------------------------
C
      FUNCTION IPBUG (DCODE)
C
C  FUNCTION TO CHECK IF PREPROCESSOR COMPONENT DEBUG CODE SPECIFIED.
C
C  FUNCTION IS SET TO 1 IF CODE SPECIFIED, 0 IF NOT.
C
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/ipbug.f,v $
     . $',                                                             '
     .$Id: ipbug.f,v 1.2 2001/06/13 13:39:25 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPALL.EQ.1) GO TO 20
C
      IPBUG=0
C
      IF (NDBUG.EQ.0) GO TO 30
C
      DO 10 I=1,NDBUG
         IF (DCODE.EQ.PDBUG(I)) GO TO 20
10       CONTINUE
      GO TO 30
C
20    IPBUG=1
C
30    RETURN
C
      END
