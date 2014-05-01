C MEMBER IKEY26
C  (from old member FCIKEY26)
C
C DESC FUNCTION TO LOOK FOR MATCH IN ARRAY OF VALUES
C
      FUNCTION IKEY26(STRING,NWORD,CWORDS,LCWRDS,NCWRDS,ND1ST)
C
C ARGUMENT LIST:
C  STRING - ARRAY HOLDING VALUES TO BE MATCHED
C  NWORD  - NUMBER OF WORDS TO MATCH
C  CWORDS - ARRAY HOLDING VALUES TO COMPARE AGAINST
C  LCWRDS - LENGTH OF EACH WORD IN CWORDS
C  NCWRDS - NUMBER OF VALUES IN CWORDS
C  ND1ST  - FIRST DIMENSION IN CWORDS
C
C........................................................
C
C  JTOSTROWSKI - HRL - MARCH 1983
C................................................................
      DIMENSION STRING(1),CWORDS(1,1),LCWRDS(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/ikey26.f,v $
     . $',                                                             '
     .$Id: ikey26.f,v 1.1 1995/09/17 18:51:47 dws Exp $
     . $' /
C    ===================================================================
C
C
      IKEY = 0
C
      DO 10 I=1,NCWRDS
      IF (NWORD.NE.LCWRDS(I)) GO TO 10
      K = (I -1 ) * ND1ST + 1
      IF (IUSAME(STRING(1),CWORDS(1,K),NWORD).EQ.0) GO TO 10
      IKEY = I
      GO TO 9999
C
   10 CONTINUE
C
 9999 IKEY26 = IKEY
      RETURN
      END
