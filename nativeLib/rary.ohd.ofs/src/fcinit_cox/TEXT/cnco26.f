C MEMBER CNCO26
C  (from old member FCCOX26)
C
C***********************************************************************
C
      SUBROUTINE CNCO26(CNEW,X,NCNEW,NCOLD,LCN)
C
C***********************************************************************
C
C     GENERATE NEW CARRYOVER DATA FROM OLD CARRYOVER DATA WITH SAME
C     OPERATION TIME INTERVAL
C
      DIMENSION CNEW(1),X(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cnco26.f,v $
     . $',                                                             '
     .$Id: cnco26.f,v 1.1 1995/09/17 18:47:06 dws Exp $
     . $' /
C    ===================================================================
C
      IF (NCNEW-NCOLD) 1212,1212,1232
 1212 CONTINUE
      IST = LCN
      IEND = LCN+NCNEW-1
      DO 1214 I=IST,IEND
      J = NCOLD-NCNEW+I-IST+1
 1214 CNEW(I) = X(J)
      GO TO 1250
 1232 CONTINUE
      J = 0
      IST = LCN+NCNEW-NCOLD
      IEND = LCN+NCNEW-1
      DO 1234 I=IST,IEND
      J = J+1
 1234 CNEW(I) = X(J)
      IST = LCN
      IEND = LCN+NCNEW-NCOLD-1
      DO 1235 I=IST,IEND
 1235 CNEW(I) = CNEW(IEND+1)
 1250 CONTINUE
      RETURN
      END
