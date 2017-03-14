C MEMBER FCSORT
C  (from old member FCFCSORT)
C.......................................................................
C
C   SUBROUTINE FCSORT SORTS THE SLOTS MARKED AS VOLATILE OR INCOMPLETE
C     AND PROTECTED BY AGE.
C   THE SORTING ALLOWS THE OLDEST TO BE ALLOCATED FIRST.
C.......................................................................
C
C  FCSORT USES A 'BUBBLE SORT' TO ARRANGE THE SLOTS BY AGE.
C......................................................................
C
      SUBROUTINE FCSORT(IARRAY,IDAY,ITIM,NIN)
C
C.....................................................................
C
C   ARGUMENT LIST --
C
C     IARRAY - THE ARRAY TO BE SORTED
C     IDAY   - DAY ASSOCIATED WITH ELEMENT IN IARRAY
C     ITIM   - TIME OF DAY ASSOCIATED WITH ELEMENT IN IARRAY
C     NIN    - NUMBER OF ELEMENTS IN IARRAY
C
C.....................................................................
C
C   SUBROUTINE ORIGINALLY PROGRAMMED BY --
C      JOE OSTROWSKI -- HRL -- 791022
C
C.......................................................................
C
      DIMENSION IARRAY(1),IDAY(1),ITIM(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/fcsort.f,v $
     . $',                                                             '
     .$Id: fcsort.f,v 1.1 1995/09/17 19:08:11 dws Exp $
     . $' /
C    ===================================================================
C
C
      LAST=NIN
      L=LAST-1
      M=LAST
C
      DO 10 K=1,L
      IND=0
      M=M-1
      DO 20 N=1,M
      NN=IARRAY(N)
      NM=IARRAY(N+1)
C     WRITE(6,601) NN,NM,IDAY(NN),IDAY(NM)
  601 FORMAT(16I5)
      IF(IDAY(NN).LE.IDAY(NM)) GO TO 20
C
      ITEMP=IARRAY(N)
      IARRAY(N)=IARRAY(N+1)
      IARRAY(N+1)=ITEMP
      IND=1
   20 CONTINUE
      IF(IND.EQ.0) GO TO 30
   10 CONTINUE
C
   30 M=LAST
C
      DO 40 K=1,L
      IND=0
      M=M-1
      DO 50 N=1,M
      NN=IARRAY(N)
      NM=IARRAY(N+1)
      IF(IDAY(NN).NE.IDAY(NM)) GO TO 50
      IF(ITIM(NN).LE.ITIM(NM)) GO TO 50
C
      ITEMP=IARRAY(N)
      IARRAY(N)=IARRAY(N+1)
      IARRAY(N+1)=ITEMP
      IND=1
   50 CONTINUE
      IF(IND.EQ.0) GO TO 60
   40 CONTINUE
C
   60 CONTINUE
      RETURN
      END
