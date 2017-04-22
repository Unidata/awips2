C MODULE USORT1
C-----------------------------------------------------------------------
C
C  ROUTINE TO SORT AN ARRAY INTO ASCENDING ORDER.
C
      SUBROUTINE USORT1 (IDIM1,NUM,IARAY1,IARAY2,ISPTR,ISTAT)
C
C  INPUT VARIABLES:
C
C     IDIM1  - FIRST DIMENSION OF ARRAY TO BE SORTED.
C     NUM    - NUMBER OF ARRAY POSITIONS TO BE SORTED.
C     IARAY1 - ARRAY TO BE SORTED.
C     IARAY2 - ARRAY AFTER SORTING.
C     ISPTR  - IF SET TO 0, RELATIVE POSITIONS NOT RETURNED.
C              IF ISPTR(1) SET TO 1, AN ARRAY OF THE RELATIVE LOCATION
C                 OF THE SORTED ARRAY POSITIONS TO THE UNSORTED ARRAY IS
C                 RETURNED.
C                 FOR EXAMPLE IF BEFORE SORTING:
C                     IARAY1(1)=C
C                     IARAY1(2)=B
C                     IARAY1(3)=A
C                 THEN AFTER SORTING:
C                     IARAY2(1)=A  AND  ISPTR(1)=3
C                     IARAY2(2)=B  AND  ISPTR(2)=2
C                     IARAY2(3)=C  AND  ISPTR(3)=1
C
C  OUTPUT VARIABLES:
C
C     ISTAT  - STATUS CODE
C              0 = SORT SUCCESSFUL
C
C
      CHARACTER*4 IARAY1(IDIM1,*),IARAY2(IDIM1,*),CTEMP
      DIMENSION ISPTR(*)
C
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/usort1.f,v $
     . $',                                                             '
     .$Id: usort1.f,v 1.2 2002/02/11 20:48:53 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,130)
         ENDIF
C
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,140) IDIM1,NUM
         ENDIF
C
      ISTAT=0
      NPASS=0
      NSWTCH=0
C
      IF (NUM.LT.2) GO TO 120
C
      NVAL=NUM-1
C
C  CHECK IF ONLY SORTED POINTERS TO BE RETURNED
      INDPTR=1
      IF (ISPTR(1).LE.0) INDPTR=0
C
      DO 10 J=1,NUM
         IF (INDPTR.EQ.1) ISPTR(J)=J
         DO 10 I=1,IDIM1
            IARAY2(I,J)=IARAY1(I,J)
10          CONTINUE
C
C  SORT ARRAY - SORT BY EACH WORD POSITION IF MULTI-DIMENTIONAL ARRAY.
C  CHECK EACH ARRAY POSITION AGAINST NEXT. IF NOT IN PROPER ORDER,
C  SWITCH LOCATIONS. IF NO SWITCES MADE, WORD POSITION IS PROPERLY
C  SORTED. IF SWITCH MADE, LOCATION CHECKED HAS BEEN CHECKED AGAINST
C  ALL OTHERS SO ONE LESS POSITION NEEDS TO BE CHECKED NEXT TIME.
C
      DO 80 NDIM=1,IDIM1
         IVAL=NVAL
         DO 70 N=1,NVAL
            IND=0
            DO 60 I=1,IVAL
               NPASS=NPASS+1
               IF (IARAY2(NDIM,I).LE.IARAY2(NDIM,I+1)) GO TO 60
               IF (NDIM.EQ.1) GO TO 30
                  NT=NDIM-1
                  DO 20 NN=1,NT
                     IF (IARAY2(NN,I).NE.IARAY2(NN,I+1)) GO TO 60
20                   CONTINUE
30             IF (INDPTR.EQ.0) GO TO 35
                  ITEMP=ISPTR(I+1)
                  ISPTR(I+1)=ISPTR(I)
                  ISPTR(I)=ITEMP
35             DO 40 IPOS=1,IDIM1
                  CTEMP=IARAY2(IPOS,I+1)
                  IARAY2(IPOS,I+1)=IARAY2(IPOS,I)
                  IARAY2(IPOS,I)=CTEMP
40                CONTINUE
50             IND=1
               NSWTCH=NSWTCH+1
60             CONTINUE
               IF (IND.EQ.0) GO TO 80
               IVAL=IVAL-1
70          CONTINUE
80       CONTINUE
C
90    IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,150) NPASS,NSWTCH
         IF (ICMDBG.GT.1) THEN
            CALL ULINE (ICMPRU,2)
            WRITE (ICMPRU,160)
            CALL ULINE (ICMPRU,2)
            WRITE (ICMPRU,170)
            DO 100 I=1,NUM
               CALL ULINE (ICMPRU,1)
               IF (INDPTR.EQ.0) WRITE (ICMPRU,190) I,
     *            (IARAY2(N,I),N=1,IDIM1)
               IF (INDPTR.EQ.1) WRITE (ICMPRU,200) I,ISPTR(I),
     *            (IARAY1(N,ISPTR(I)),N=1,IDIM1)
100            CONTINUE
            CALL ULINE (ICMPRU,2)
            WRITE (ICMPRU,205)
            DO 110 I=1,NUM
               CALL ULINE (ICMPRU,1)
               IF (INDPTR.EQ.0) WRITE (ICMPRU,210) I,
     *            (IARAY2(N,I),N=1,IDIM1)
               IF (INDPTR.EQ.1) WRITE (ICMPRU,220) I,ISPTR(I),
     *            (IARAY1(N,ISPTR(I)),N=1,IDIM1)
110            CONTINUE
            ENDIF
         ENDIF
C
120   IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,230) ISTAT
         ENDIF
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
130   FORMAT (' *** ENTER USORT1')
140   FORMAT (' IDIM1=',I3,3X,'NUM=',I6)
150   FORMAT (' NUMBER OF PASSES=',I12,3X,'NUMBER OF SWITCHES=',I12)
160   FORMAT ('0ARRAY SORTED IN ASCENDING ORDER')
170   FORMAT ('0PRINTED IN 10(1X,I5) FORMAT')
190   FORMAT (' I=',I5,3X,10(1X,I5))
200   FORMAT (' I=',I5,3X,'ISPTR(I)=',I5,3X,10(1X,I5))
205   FORMAT ('0PRINTED IN 10(1X,A4) FORMAT')
210   FORMAT (' I=',I5,3X,10(1X,A4))
220   FORMAT (' I=',I5,3X,'ISPTR(I)=',I5,3X,10(1X,A4))
230   FORMAT (' *** EXIT USORT1 - ISTAT=',I3)
C
      END
