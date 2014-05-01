C MEMBER IOFF26
C  (from old member FCIOFF26)
C
C DESC COMPUTE OFFSET FROM BEGINNING OF COMP. VARIABLE SECTION FOR COMP.
C DESC VARIABLE NO. INUM
C-----------------------------------------------------------------------
      FUNCTION IOFF26(INUM)
C-----------------------------------------------------------------------
C  ARGS:
C   INUM - WHICH ONE IN THE LIST OF TOTAL COMPARISON VARIABLES WE WANT
C          THE OFFSET FOR. OFFSET IS EQUIVALENT TO THE ACTUAL WORD
C          IN THIS SECTION(I.E. - OFFSET = 4, COMPARISON VARIABLE
C          DEFINITION STARTS IN WORD 4.)
C----------------------------------------------------------------------
C
C  JTOSTROWSKI - HRL - MARCH 1983
C----------------------------------------------------------------
      INCLUDE 'common/cmpv26'
C
      DIMENSION LENTH(3)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/ioff26.f,v $
     . $',                                                             '
     .$Id: ioff26.f,v 1.2 1999/04/22 14:44:46 page Exp $
     . $' /
C    ===================================================================
C
C
C  LENTH CORRESPONDS TO THE NUMBER OF WORDS NEEDED TO STORE COMP.
C  VARIABLES 1, 2, AND 3.
C
      DATA LENTH/3,6,4/
C
C  FIRST WORD IS RESERVED FOR THE NUMBER OF COMP. VARIABLES STORED
C
      IOFF26 = 2
C
      IF (NUMCMP.EQ.0) GO TO 99
C
      DO 10 I=1,NUMCMP
      IF (INUM.EQ.I) GO TO 99
      IOFF26 = IOFF26 + LENTH(ICTARY(I))
   10 CONTINUE
C
   99 CONTINUE
      RETURN
      END
