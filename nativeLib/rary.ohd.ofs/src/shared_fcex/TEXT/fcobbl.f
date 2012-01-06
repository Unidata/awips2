C MEMBER FCOBBL
C  (from old member FCFCOBBL)
C
C.......................................................................
C
C  SUBROUTINE FCOBBL IS USED PRIMARILY FOR THE FINAL SORT OF THE
C  CARRYOVER SAVE DATES IN SUBROUTINE FNITCO. DATES ARE ARRANGED IN
C  ASCENDING ORDER.
C
C.......................................................................
C
C  FCOBBL USES A 'BUBBLE SORT' TO ARRANGE THE DATES.
C
C......................................................................
C
      SUBROUTINE FCOBBL(IDAY,ITIM,NBUB)
C
C......................................................................
C
C  ARGUMENT LIST --
C
C     IDAY - JULIAN CARRYOVER DATE ARRAY
C     ITIM - INTERNAL CLOCK TIME ASSOCIATED WITH IDAY
C     NBUB - NUMBER OF ELEMENTS IN IDAY AND ITIM ARRAYS
C
C.....................................................................
C
C  SUBROUTINE ORIGINALLY WRITTEN BY --
C      JOE OSTROWSKI -- HRL -- 791120
C
C.......................................................................
C
C
      DIMENSION IDAY(1),ITIM(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_fcex/RCS/fcobbl.f,v $
     . $',                                                             '
     .$Id: fcobbl.f,v 1.2 1995/09/29 14:28:52 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (NBUB.LE.1) RETURN
      LAST=NBUB
      L=LAST-1
      M=LAST
      DO 200 K=1,L
      IND=0
      M=M-1
      DO 190 N=1,M
      IF(IDAY(N).LE.IDAY(N+1)) GO TO 190
      ITEMP=IDAY(N)
      IDAY(N)=IDAY(N+1)
      IDAY(N+1)=ITEMP
      ITEMP=ITIM(N)
      ITIM(N)=ITIM(N+1)
      ITIM(N+1)=ITEMP
      IND=1
  190 CONTINUE
      IF(IND.EQ.0)  GO TO 210
  200 CONTINUE
  210 CONTINUE
C
C
      M=LAST
      DO 240 K=1,L
      IND=0
      M=M-1
      DO 230 N=1,M
      IF(IDAY(N).NE.IDAY(N+1)) GO TO 230
      IF(ITIM(N).LE.ITIM(N+1)) GO TO 230
      ITEMP=ITIM(N)
      ITIM(N)=ITIM(N+1)
      ITIM(N+1)=ITEMP
      IND=1
  230 CONTINUE
      IF(IND.EQ.0) GO TO 250
  240 CONTINUE
  250 CONTINUE
      RETURN
      END
