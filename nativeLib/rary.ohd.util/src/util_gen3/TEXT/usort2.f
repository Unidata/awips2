C MODULE USORT2
C-----------------------------------------------------------------------
C
C  ROUTINE TO SORT AN ARRAY INTO ASCENDING ORDER.
C
C  THE ARRAY IS SORTED BY FULL WORDS STARTING AT THE SPECIFIED
C  BEGINNING LOCATION AND ENDING AT THE SPECIFIED ENDING LOCATION.
C
      SUBROUTINE USORT2 (IDIM1,NUM,IWORD1,IWORD2,IARAY1,IARAY2,ISPTR,
     *   ISTAT)
C
C  INPUT VARIABLES:
C     IDIM1  - FIRST DIMENSION OF ARRAY TO BE SORTED.
C     NUM    - NUMBER OF ARRAY POSITIONS TO BE SORTED.
C     IWORD1 - FIRST ARRAY POSITION TO BE CHECKED.
C     IWORD2 - LAST  ARRAY POTISION TO BE CHECKED.
C     IARAY1 - ARRAY TO BE SORTED.
C     IARAY2 - ARRAY AFTER SORTING.
C     ISPTR  - IF SET TO 0, RELATIVE POSITIONS NOT RETURNED.
C              IF ISPTR(1) SET TO 1, AN ARRAY OF THE RELATIVE LOCATION
C                 OF THE SORTED ARRAY POSITIONS TO THE UNSORTED ARRAY
C                 IS RETURNED.
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
C     ISTAT  - STATUS CODE:
C               0=SORT SUCCESSFUL
C
      DIMENSION IARAY1(IDIM1,1),IARAY2(IDIM1,1),ISPTR(1)
C
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/usort2.f,v $
     . $',                                                             '
     .$Id: usort2.f,v 1.2 1998/07/02 14:56:00 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,150)
         ENDIF
C
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,160) IDIM1,NUM,IWORD1,IWORD2
         IF (ICMDBG.GT.1) THEN
            CALL ULINE (ICMPRU,2)
            WRITE (ICMPRU,180) 'BEFORE'
            CALL ULINE (ICMPRU,2)
            WRITE (ICMPRU,190)
            DO 10 I=1,NUM
               CALL ULINE (ICMPRU,1)
               WRITE (ICMPRU,200) I,(IARAY1(N,I),N=1,IDIM1)
10             CONTINUE
            CALL ULINE (ICMPRU,2)
            WRITE (ICMPRU,220)
            DO 20 I=1,NUM
               CALL ULINE (ICMPRU,1)
               WRITE (ICMPRU,230) I,(IARAY1(N,I),N=1,IDIM1)
20             CONTINUE
            ENDIF
         ENDIF
C
      ISTAT=0
      NPASS=0
      NSWTCH=0
C
      IF (NUM.LT.2) GO TO 140
C
      NVAL=NUM-1
C
C  CHECK IF ONLY SORTED POINTERS TO BE RETURNED
      INDPTR=1
      IF (ISPTR(1).LE.0) INDPTR=0
C
      DO 40 J=1,NUM
         IF (INDPTR.EQ.1) ISPTR(J)=J
         DO 30 I=1,IDIM1
            IARAY2(I,J)=IARAY1(I,J)
30          CONTINUE
40       CONTINUE
C
C  SORT ARRAY - SORT BY EACH WORD POSITION IF MULTI-DIMENTIONAL ARRAY.
C  CHECK EACH ARRAY POSITION AGAINST NEXT. IF NOT IN PROPER ORDER,
C  SWITCH LOCATIONS. IF NO SWITCES MADE, WORD POSITION IS PROPERLY
C  SORTED. IF SWITCH MADE, LOCATION CHECKED HAS BEEN CHECKED AGAINST
C  ALL OTHERS SO ONE LESS POSITION NEEDS TO BE CHECKED NEXT TIME.
C
      DO 110 NDIM=IWORD1,IWORD2
         IVAL=NVAL
         DO 100 N=1,NVAL
            IND=0
            DO 90 I=1,IVAL
               NPASS=NPASS+1
               IF (IARAY2(NDIM,I).LE.IARAY2(NDIM,I+1)) GO TO 90
               IF (NDIM.EQ.IWORD1) GO TO 60
                  NT=NDIM-1
                  DO 50 NN=IWORD1,NT
                     IF (IARAY2(NN,I).NE.IARAY2(NN,I+1)) GO TO 90
50                   CONTINUE
60             IF (INDPTR.EQ.0) GO TO 70
                  ITEMP=ISPTR(I+1)
                  ISPTR(I+1)=ISPTR(I)
                  ISPTR(I)=ITEMP
70             DO 80 IPOS=1,IDIM1
                  ITEMP=IARAY2(IPOS,I+1)
                  IARAY2(IPOS,I+1)=IARAY2(IPOS,I)
                  IARAY2(IPOS,I)=ITEMP
80                CONTINUE
               IND=1
               NSWTCH=NSWTCH+1
90             CONTINUE
               IF (IND.EQ.0) GO TO 110
               IVAL=IVAL-1
100         CONTINUE
110      CONTINUE
C
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,170) NPASS,NSWTCH
         IF (ICMDBG.GT.1) THEN
            CALL ULINE (ICMPRU,2)
            WRITE (ICMPRU,180) 'AFTER'
            CALL ULINE (ICMPRU,2)
            WRITE (ICMPRU,190)
            DO 120 I=1,NUM
               CALL ULINE (ICMPRU,1)
               IF (INDPTR.EQ.0) WRITE (ICMPRU,200) I,
     *            (IARAY2(N,I),N=1,IDIM1)
               IF (INDPTR.EQ.1) WRITE (ICMPRU,210) I,ISPTR(I),
     *            (IARAY1(N,ISPTR(I)),N=1,IDIM1)
120            CONTINUE
            CALL ULINE (ICMPRU,2)
            WRITE (ICMPRU,220)
            DO 130 I=1,NUM
               CALL ULINE (ICMPRU,1)
               IF (INDPTR.EQ.0) WRITE (ICMPRU,230) I,
     *            (IARAY2(N,I),N=1,IDIM1)
               IF (INDPTR.EQ.1) WRITE (ICMPRU,240) I,ISPTR(I),
     *            (IARAY1(N,ISPTR(I)),N=1,IDIM1)
130            CONTINUE
            ENDIF
         ENDIF
C
140   IF (ICMTRC.GT.0) THEN
          CALL ULINE (ICMPRU,1)
          WRITE (ICMPRU,250) ISTAT
          ENDIF
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
150   FORMAT (' *** ENTER USORT2')
160   FORMAT (' IDIM1=',I3,3X,'NUM=',I6,3X,'IWORD1=',I3,3X,
     *   'IWORD2=',I3)
170   FORMAT (' NUMBER OF PASSES=',I12,3X,'NUMBER OF SWITCHES=',I12)
180   FORMAT ('0ARRAY ',A,' SORTING')
190   FORMAT ('0PRINTED IN (I4,1X) FORMAT')
200   FORMAT (' I=',I5,3X,10(1X,I5))
210   FORMAT (' I=',I5,3X,'ISPTR(I)=',I5,3X,10(1X,I5))
220   FORMAT ('0PRINTED IN (A4,1X) FORMAT')
230   FORMAT (' I=',I5,3X,10(1X,A4))
240   FORMAT (' I=',I5,3X,'ISPTR(I)=',I5,3X,10(1X,A4))
250   FORMAT (' *** EXIT USORT2 - ISTAT=',I3)
C
      END
