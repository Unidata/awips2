C-----------------------------------------------------------------------
C      THIS IS ONE OF THE SUBPROGRAMS FOR KALMAN FILTER
C      IT GETS INVERSE MATRIX BY GAUSS ELIMINATION METHOD
C      R1=R(-1)
C-----------------------------------------------------------------------
         SUBROUTINE AVER55(R,R1,NO)
         DIMENSION R(NO,NO),R1(NO,NO)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/aver55.f,v $
     . $',                                                             '
     .$Id: aver55.f,v 1.1 1999/04/23 18:08:21 dws Exp $
     . $' /
C    ===================================================================
C

         DO 100 I=1,NO
         DO 100 J=1,NO
100      R1(I,J)=0.0
         DO 120 K=1,NO
120      R1(K,K)=1.0
C----------------------------  FORWARD ---------------------------------
         DO 300 L=1,NO-1
C-------  SELECTING MAIN ELEMENT AND CHANGE LINE  ----------------------
         XY1=ABS(R(L,L))
         IS=L
         DO 150 I=L+1,NO
           IF (ABS(R(I,L)) .GT. XY1) THEN
           XY1=ABS(R(I,L))
           IS=I
           END IF
150      CONTINUE
         DO 170 J=1,NO
         TEMP=R(IS,J)
         R(IS,J)=R(L,J)
170      R(L,J)=TEMP
         DO 180 J=1,NO
         TEMP=R1(IS,J)
         R1(IS,J)=R1(L,J)
180      R1(L,J)=TEMP
C-----------------  END OF SELECTING MAIN ELEMENT ----------------------
         DO 200 J=1,NO
         R1(L,J)=R1(L,J)/XY1
200      R(L,J)=R(L,J)/XY1
 
         DO 220 I=L+1,NO
         XY1=R(I,L)
         DO 220 J=1,NO
         R1(I,J)=R1(I,J)-R1(L,J)*XY1
220      R(I,J)=R(I,J)-R(L,J)*XY1
300      CONTINUE 
         DO 320 J=1,NO
         R(NO,J)=R(NO,J)/R(NO,NO)
320      R1(NO,J)=R1(NO,J)/R(NO,NO)
C-----------------------  BACK  ----------------------------------------
         DO 500 L=NO,2,-1
         DO 420 I=L-1,1,-1
         XY1=R(I,L)
         DO 420 J=1,NO
         R1(I,J)=R1(I,J)-R1(L,J)*XY1
420      R(I,J)=R(I,J)-R(L,J)*XY1
500      CONTINUE
         RETURN 
         END
