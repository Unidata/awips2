C MEMBER UINTCH
C-----------------------------------------------------------------------
C
      SUBROUTINE UINTCH (INTEGR,MAXCHR,CHAR,LFILL,ISTAT)
C
C  ROUTINE TO A CONVERT AN INTEGER VALUE TO CHARACTER VALUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     ARGUMENT LIST
C
C    INTEGR - INPUT  - INTEGER VALUE TO BE CONVERTED.
C    MAXCHR - INPUT  - MAXIMUM NUMBER OF CHARACTERS ARRAY CHAR CAN HOLD
C      CHAR - OUTPUT - ARRAY CONTAINING RIGHT JUSTIFIED CHARACTER
C                      REPRESENTATION OF VALUE INTEGR.  LEFTMOST
C                      CHARACETRS WILL BE PADDED WITH BLANKS.
C     LFILL - OUTPUT - NUMBER OF CHARACTERS FILLED IN ARRAY CHAR.  THIS
C                      INCLUDES THE MINUS SIGN IF INTEGR IS NEGATIVE.
C     ISTAT - OUTPUT - STATUS CODE
C                      0=NORMAL RETURN
C                      1=ARRAY CHAR NOT LARGE ENOUGH TO HOLD VALUE
C                        INTEGR. THE RIGHTMOST MAXCHR CHARACTERS OF
C                        VALUE INTEGR ARE PACKED INTO CHAR.
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      LOGICAL*4 NEGA
      CHARACTER*1 CHAR(MAXCHR)
      CHARACTER*10 DIGITS/'0123456789'/
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/uintch.f,v $
     . $',                                                             '
     .$Id: uintch.f,v 1.1 1995/09/17 19:02:13 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.0) WRITE (ICMPRU,10)
C
      IF (ICMDBG.GT.0) WRITE (ICMPRU,20) INTEGR,MAXCHR
C
      ISTAT=0
      LFILL=0
C
C  CHECK SIZE OF ARRAY TO HOLD CHARACTER REPRESENTATION
      IF (MAXCHR.GT.0) GO TO 30
         ISTAT=1
         GO TO 100
C
30    DO 40 I=1,MAXCHR
         CHAR(I)=' '
40       CONTINUE
C
C  CHECK IF INTEGER VALUE IS ZERO
      IF (INTEGR.NE.0) GO TO 50
         CHAR(MAXCHR)=DIGITS(1:1)
         LFILL=1
         GO TO 100
50    ITEMP=INTEGR
      NEGA=.FALSE.
      IF (ITEMP.LT.0) NEGA=.TRUE.
      IF (NEGA) ITEMP=-ITEMP
      DO 70 I=1,11
         LPLACE=MAXCHR-I+1
         IF (I.GT.1) ITEMP=ITEMP/10
         IF (ITEMP.EQ.0) GO TO 80
         IF (LPLACE.GT.0) GO TO 60
            ISTAT=1
            LFILL=MAXCHR
            GO TO 100
60       IDIG=ITEMP-(ITEMP/10)*10+1
         CHAR(LPLACE)=DIGITS(IDIG:IDIG)
70       CONTINUE
80    LFILL=MAXCHR-LPLACE
      IF (.NOT.NEGA) GO TO 100
      IF (LPLACE.GT.0) GO TO 90
         ISTAT=1
         GO TO 100
90    CHAR(LPLACE)='-'
      LFILL=LFILL+1
C
100   IF (ICMDBG.GT.0) WRITE (ICMPRU,110) LFILL,(CHAR(I),I=1,MAXCHR)
C
      IF (ICMTRC.GT.0) WRITE (ICMPRU,120)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT (' *** ENTER UINTCH')
20    FORMAT (' INTEGR=',I9,3X,'MAXCHR=',I5)
110   FORMAT (' LFILL=',I3,3X,'CHAR=',100A1)
120   FORMAT (' *** EXIT UINTCH')
C
      END
