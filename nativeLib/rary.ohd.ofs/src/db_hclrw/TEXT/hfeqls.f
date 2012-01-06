C MEMBER HFEQLS
C  (from old member HCLCUTIL)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/21/94.14:13:32 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HFEQLS (IFIELD,LEFT,IRIGHT,ISTRGT)
C
C 3-3-82 IF RIGHT START WITH QUOTE, DONT INCLUDE IT....IF LEFT ENDS
C        WITH QUOTE, DONT INCLUDE IT.
C
C THIS ROUTINE WILL SEARCH A FIELD IN UFREEI FOR '='.  IT WILL
C PACK THE LEFT CHARS IN LEFT(MAX OF 8), AND THE RIGHT CHARS
C IN IRIGHT (MAX OF 8).  ISTRGT IS SET TO THE STARTING COL OF THE
C RIGHT.  IF THERE IS NO EQUAL, LEFT IS SET TO BLANKS.
C
C      ARG         TYPE    I/O   DIM       DESC
C
C      IFIELD       I       I     1       FIELD TO BE SEARCHED
C      LEFT         I       O     2       WORDS TO GET LEFT CHARS
C      IRIGHT       I       O     2       WORD TO GET RIGHT CHARS
C      ISTRGT       I       O     1       COL IN IBUF AFTER = SIGN
C
      INCLUDE 'udatas'
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
      DIMENSION IRIGHT(2),LEFT(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hfeqls.f,v $
     . $',                                                             '
     .$Id: hfeqls.f,v 1.1 1995/09/17 18:42:14 dws Exp $
     . $' /
C    ===================================================================
C
      DATA LQUOTE /4H'   /
C
      CALL UMEMST (IBLNK,LEFT,2)
      CALL UMEMST (IBLNK,IRIGHT,2)
C
       J=IFSTRT(IFIELD)
      K=IFSTOP(IFIELD)
C
      DO 10 I=J,K
         IF (IBUF(I).EQ.IEQUAL) GO TO 20
10    CONTINUE
C
C NO EQUAL FOUND, PUT IT ALL IN IRIGHT
C
      L=J
      GO TO 40
C
C EQUAL FOUND IN COLUMN I
C
20    CONTINUE
C
C PACK LEFT
C
      L=I-1
      IF (IBUF(L).EQ.LQUOTE) L=L-1
      N=L-J+1
      IF (N.LE.0) GO TO 30
      IF (N.GT.8) N=8
      CALL UPACK1(IBUF(J),LEFT,N)
C
C PACK RIGHT
C
30    L=I+1
      IF (IBUF(L).EQ.LQUOTE) L=L+1
40    N=K-L+1
      IF (N.GT.8) N=8
      CALL UPACK1(IBUF(L),IRIGHT,N)
      ISTRGT=L
C
C ALL DONE
C
      IF (IHCLDB.GT.2) WRITE (IOGDB,50) IFIELD,LEFT,IRIGHT,ISTRGT
50    FORMAT (' EXIT HFEQLS - IFIELD=',I3,' LEFT=',2A4,' IRIGHT=',
     1  2A4,' ISTRGT=',I3)
C
      RETURN
C
      END
