C MODULE USCAN
C-----------------------------------------------------------------------
C
C  ROUTINE TO RETURN A WORD FROM A CHARACTER STRING USING THE
C  SPECIFIED DELIMITER CHARACTERS.
C
      SUBROUTINE USCAN (STRNG,MSTRNG,DLIM,MDLIM,NSCAN,WORD,MWORD,LWORD,
     *   ISTAT)
C
C  INPUT VARIABLES :
C     STRNG  - CHARACTER STRING TO BE CHECKED FOR DLIM
C     MSTRNG - MAXIMUM NUMBER IF CHARACTERS IN VARIABLE CONTAINING
C              CHARACTER STRING TO BE CHECKED FOR DLIM
C     DLIM   - CHARACTER STRING CONTAINING DELIMITERS
C     MDLIM  - MAXIMUM NUMBER IF CHARACTERS IN VARIABLE CONTAINING
C              DELIMITERS
C                >0 = TREAT EACH CHARACTER AS A DIFFERENT DELIMITERS
C                <0 = TREAT ALL CHARACTER AS ONE DELIMITER
C     NSCAN  - WORD NUMBER TO BE RETURNED
C     MWORD  - MAXIMUM NUMBER IF CHARACTERS IN VARIABLE TO CONTAIN
C              WORD
C
C  OUTPUT VARIABLES :
C     WORD   - CHARACTER STRING CONTAINING WORD
C     LWORD  - NUMBER OF CHARACTERS IN WORD
C              (WHEN NO MORE FIELDS ARE FOUND, SET TO BLANK)
C     ISTAT  - STATUS CODE
C                0=NORMAL RETURN
C                1=WORD ARRAY TOO SMALL
C
C  NOTES:
C     - LEADING DELIMITERS BEFORE THE FIRST WORD HAVE NO EFFECT
C     - TWO OR MORE CONTIGUOUS DELIMITERS ARE TREATED AS ONE
C     - IF THERE ARE FEWER THEN 'NSCAN' WORDS, 'WORD' IS RETURNED
C       AS BLANK
C
C
      CHARACTER*(*) STRNG,DLIM,WORD
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/uscan.f,v $
     . $',                                                             '
     .$Id: uscan.f,v 1.3 1999/07/06 13:00:11 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'ENTER USCAN'
         ENDIF
C
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*)
     *      ' MSTRNG=',MSTRNG,
     *      ' STRNG=',STRNG(1:LENSTR(STRNG)),
     *      ' '
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*)
     *      ' MDLIM=',MDLIM,
     *      ' DLIM=',DLIM(1:LENSTR(DLIM)),
     *      ' '
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*)
     *      ' NSCAN=',NSCAN,
     *      ' MWORD=',MWORD,
     *      ' '
         ENDIF
C
      ISTAT=0
C
      NFOUND=0
      LWORD=0
C
      IF (MSTRNG.LE.0) THEN
         ISTAT=1
         GO TO 120
         ENDIF
      IF (MDLIM.EQ.0) THEN
         ISTAT=1
         GO TO 120
         ENDIF
      IF (MWORD.LE.0) THEN
         ISTAT=1
         GO TO 120
         ENDIF
C
      WORD=' '
C
C  FIND NUMBER OF CHARACTERS IN CHARACTER STRING
      CALL ULENTH (STRNG,MSTRNG,LSTRNG)
C
C  FIND FIRST NON-BLANK CHARACTER
      CALL UBEGIN (STRNG,LSTRNG,IBEG)
C
      IF (MDLIM.GT.0) THEN
         IDLIM=1
         MDLIM2=MDLIM
         ENDIF
      IF (MDLIM.LT.0) THEN
         IDLIM=-1
         MDLIM2=-MDLIM
         ENDIF
C
C  CHECK IF ONE DELIMITER SPECIFIED
      IF (IDLIM.LT.0) GO TO 60
C      
C  CHECK FOR FIRST NON-DELIMITER CHARACTER
      ISTRT=IBEG
      DO 20 I=ISTRT,LSTRNG
         DO 10 J=1,MDLIM2
            IF (ICMDBG.GT.1) THEN
               CALL ULINE (ICMPRU,1)
               WRITE (ICMPRU,*)
     *            ' I=',I,' STRNG(I:I)=',STRNG(I:I),
     *            ' J=',J,' DLIM(J:J)=',DLIM(J:J),
     *            ' IBEG=',IBEG
               ENDIF
            IF (STRNG(I:I).EQ.DLIM(J:J)) THEN
               IBEG=I+1
               GO TO 20
               ENDIF
10          CONTINUE
         GO TO 30
20       CONTINUE
C
C  CHECK FOR DELIMITER IN STRING
30    IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'IBEG=',IBEG
         ENDIF
      LDLIM=0
      NDLIM=0
      DO 50 I=IBEG,LSTRNG
         DO 40 J=1,MDLIM2
            IF (ICMDBG.GT.1) THEN
               CALL ULINE (ICMPRU,1)
               WRITE (ICMPRU,*)
     *            ' I=',I,' STRNG(I:I)=',STRNG(I:I),
     *            ' J=',J,' DLIM(J:J)=',DLIM(J:J),
     *            ' LDLIM=',LDLIM,' NDLIM=',NDLIM
               ENDIF
            IF (STRNG(I:I).EQ.DLIM(J:J)) THEN
               LDLIM=I
               NDLIM=NDLIM+1
               GO TO 50
               ENDIF
40          CONTINUE
         IF (NDLIM.GT.0) GO TO 100
50       CONTINUE
C
      IF (LDLIM.EQ.0) LDLIM=LSTRNG+1
C
C  CHECK IF MORE THAN ONE DELIMITER SPECIFIED
60    IF (IDLIM.GT.0) GO TO 100
C
C  CHECK FOR FIRST NON-DELIMITER CHARACTER
      ISTRT=IBEG
      DO 70 I=ISTRT,LSTRNG
         IF (ICMDBG.GT.1) THEN
            CALL ULINE (ICMPRU,1)
            WRITE (ICMPRU,*) 
     *         ' I=',I,'STRNG(I:I+MDLIM2-1)=',STRNG(I:I+MDLIM2-1),
     *         ' DLIM=',DLIM,' IBEG=',IBEG
            ENDIF
         IF (STRNG(I:I+MDLIM2-1).EQ.DLIM) THEN
            IBEG=I+1
            GO TO 70
            ENDIF
         GO TO 80
70       CONTINUE
C
C  CHECK FOR DELIMITER IN STRING
80    IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'IBEG=',IBEG
         ENDIF
      LDLIM=0
      NDLIM=0
      DO 90 I=IBEG,LSTRNG
         IF (ICMDBG.GT.1) THEN
            CALL ULINE (ICMPRU,1)
            WRITE (ICMPRU,*)
     *         ' I=',I,' STRNG(I:I+MDLIM2-1)=',STRNG(I:I+MDLIM2-1),
     *         ' DLIM=',DLIM,' LDLIM=',LDLIM,' NDLIM=',NDLIM
            ENDIF
         IF (STRNG(I:I+MDLIM2-1).EQ.DLIM) THEN
            LDLIM=I
            NDLIM=NDLIM+1
            GO TO 90
            ENDIF
         IF (NDLIM.GT.0) GO TO 100
90       CONTINUE
C
      IF (LDLIM.EQ.0) LDLIM=LSTRNG+1
C
100   IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*)
     *      ' LDLIM=',LDLIM,
     *      ' NDLIM=',NDLIM,
     *      ' '
         ENDIF
C
C  CHECK IF SPECIFIED WORD NUMBER FOUND
      NFOUND=NFOUND+1
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'NFOUND=',NFOUND,' NSCAN=',NSCAN
         ENDIF
      IF (NFOUND.LT.NSCAN) THEN
         IBEG=LDLIM+1
         IF (IDLIM.EQ.1) GO TO 30
         IF (IDLIM.EQ.-1) GO TO 80
         ENDIF
C
C  SET WORD
      IF (DLIM.EQ.' '.AND.STRNG(IBEG:IBEG).EQ.' ') IBEG=IBEG+1
      IEND=LDLIM-NDLIM
      IF (NDLIM.EQ.0) IEND=IEND-1
      NSTRNG=IEND-IBEG+1
      IPOS=IBEG
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'IBEG=',IBEG,' IEND=',IEND,' NSTRNG=',NSTRNG
         ENDIF
      IF (NSTRNG.GT.0.AND.IPOS.GT.0) THEN
         IF (NSTRNG.GT.MWORD) THEN
            ISTAT=1
            NSTRNG=MWORD
            ENDIF
         DO 110 I=1,NSTRNG
            WORD(I:I)=STRNG(IPOS:IPOS)
            IPOS=IPOS+1
110         CONTINUE
         ENDIF
C
C  FIND NUMBER OF CHARACTERS IN WORD
      CALL ULENTH (WORD,MWORD,LWORD)
C
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'MWORD=',MWORD,
     *      'WORD(1:LENSTR(WORD))=',WORD(1:LENSTR(WORD))
         ENDIF
C
120   IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'EXIT USCAN : ISTAT=',ISTAT
         ENDIF
C
      RETURN
C
      END
