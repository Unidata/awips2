C MEMBER SUSORT
C-----------------------------------------------------------------------
C
C DESC SUBROUTINE SUSORT SORTS AN ARRAY INTO ASCENDING ORDER
C
C
      SUBROUTINE SUSORT (IDIM,NUM,IARAY1,IARAY2,ISPTR,ISTAT)
C
C
C  INPUT VARIABLES:
C
C     IDIM   - FIRST DIMENSION OF ARRAY TO BE SORTED.
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
      DIMENSION IARAY1(IDIM,1),IARAY2(IDIM,1),ISPTR(1)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/susort.f,v $
     . $',                                                             '
     .$Id: susort.f,v 1.1 1995/09/17 19:22:12 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG(4HSYS )
C
      IF (ISTRCE.GT.0) WRITE (LP,130)
      IF (ISTRCE.GT.0) CALL SULINE (LP,1)
C
      IF (LDEBUG.GT.0) WRITE (LP,140) IDIM,NUM
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
      IF (LDEBUG.GT.0) CALL SUTIMR (IOSDBG,ITMELA,ITMTOT)
C
      ISTAT=0
      NPASS=0
      NSWTCH=0
C
      NVAL=NUM-1
      IF (NVAL.LE.0) GO TO 120
C
C  CHECK IF ONLY SORTED POINTERS TO BE RETURNED
      INDPTR=1
      IF (ISPTR(1).LE.0) INDPTR=0
C
      DO 10 J=1,NUM
         IF (INDPTR.EQ.1) ISPTR(J)=J
         DO 10 I=1,IDIM
            IARAY2(I,J)=IARAY1(I,J)
10          CONTINUE
C
C  SORT ARRAY - SORT BY EACH WORD POSITION IF MULTI-DIMENTIONAL ARRAY.
C  CHECK EACH ARRAY POSITION AGAINST NEXT. IF NOT IN PROPER ORDER,
C  SWITCH LOCATIONS. IF NO SWITCES MADE, WORD POSITION IS PROPERLY
C  SORTED. IF SWITCH MADE, LOCATION CHECKED HAS BEEN CHECKED AGAINST
C  ALL OTHERS SO ONE LESS POSITION NEEDS TO BE CHECKED NEXT TIME.
C
      DO 80 NDIM=1,IDIM
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
35             DO 40 IPOS=1,IDIM
                  ITEMP=IARAY2(IPOS,I+1)
                  IARAY2(IPOS,I+1)=IARAY2(IPOS,I)
                  IARAY2(IPOS,I)=ITEMP
40                CONTINUE
50             IND=1
               NSWTCH=NSWTCH+1
60             CONTINUE
               IF (IND.EQ.0) GO TO 80
               IVAL=IVAL-1
70          CONTINUE
80       CONTINUE
C
90    IF (LDEBUG.EQ.0) GO TO 120
         WRITE (IOSDBG,150) NPASS,NSWTCH
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,160)
         CALL SULINE (IOSDBG,2)
         WRITE (IOSDBG,170)
         CALL SULINE (IOSDBG,2)
         DO 100 I=1,NUM
            IF (INDPTR.EQ.0) WRITE (IOSDBG,190) I,(IARAY2(N,I),N=1,IDIM)
            IF (INDPTR.EQ.1) WRITE (IOSDBG,200) I,ISPTR(I),
     *         (IARAY1(N,ISPTR(I)),N=1,IDIM)
100         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,180)
         CALL SULINE (IOSDBG,2)
         DO 110 I=1,NUM
            IF (INDPTR.EQ.0) WRITE (IOSDBG,210) I,(IARAY2(N,I),N=1,IDIM)
            IF (INDPTR.EQ.1) WRITE (IOSDBG,220) I,ISPTR(I),
     *         (IARAY1(N,ISPTR(I)),N=1,IDIM)
            CALL SULINE (IOSDBG,1)
110         CONTINUE
C
120   IF (LDEBUG.GT.0) CALL SUTIMR (IOSDBG,ITMELA,ITMTOT)
C
      IF (ISTRCE.GT.0) WRITE (LP,230) ISTAT
      IF (ISTRCE.GT.0) CALL SULINE (LP,1)
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
130   FORMAT (' *** ENTER SUSORT')
140   FORMAT (' IDIM=',I3,3X,'NUM=',I6)
150   FORMAT (' NUMBER OF PASSES=',I12,3X,'NUMBER OF SWITCHES=',I12)
160   FORMAT ('0ARRAY SORTED IN ASCENDING ORDER')
170   FORMAT ('0PRINTED IN 10(I5,1X) FORMAT')
180   FORMAT ('0PRINTED IN 10(A4,1X) FORMAT')
190   FORMAT (1H ,I5,3X,10(I5,1X))
200   FORMAT (1H ,I5,3X,I5,3X,10(I5,1X))
210   FORMAT (1H ,I5,3X,10(A4,1X))
220   FORMAT (1H ,I5,3X,I5,3X,10(A4,1X))
230   FORMAT (' *** EXIT SUSORT : STATUS CODE=',I3)
C
      END
