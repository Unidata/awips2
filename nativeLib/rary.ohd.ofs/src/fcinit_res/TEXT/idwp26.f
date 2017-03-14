C MEMBER IDWP26
C  (from old member FCIDWP26)
C
C DESC CHECK FOR VALID S/U ID WITH OR WITHOUT LEVEL SPECIFICATION
C--------------------------------------------------------------------
      SUBROUTINE IDWP26(TNAME,NPACK,INAME,ILEVEL,IERID)
C---------------------------------------------------------------------
C  ROUTINE TO CHECK FOR A VALID S/U ID WITH PARENTHESES FOR MULTIPLE
C  S/U DEFINITION
C---------------------------------------------------------------------
C  RETURN CODES -
C    0 - EVERYTHING IS OK,
C    1 - NOT A VALID S/U ID,
C    2 - INCORRECT INTEGER SPECIFICATION WITHIN PARENTHESES,
C    3 - BASIC UNCORRECTABLE ERROR
C----------------------------------------------------------------
C
C  JTOSTROWSKI - HRL - MARCH 1983
C----------------------------------------------------------------
      INCLUDE 'common/fld26'
      INCLUDE 'common/suid26'
C
C
      DIMENSION TNAME(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/idwp26.f,v $
     . $',                                                             '
     .$Id: idwp26.f,v 1.1 1995/09/17 18:51:47 dws Exp $
     . $' /
C    ===================================================================
C
      DATA BLANK/4H    /
C
C INITIALIZE LOCAL VARIABLES
C
      IERID = 0
      ILEVEL = 1
C
C  FIELD HAS ALREADY BEEN ENTERED IN CALLING ROUTINE
C
C
C LOOK FOR AND REMOVE ANY ()'S TO GET S/U ID
C
      CALL UBEF26(NPACK,TNAME,ISTRT,LLPAR,LEN,IERB)
      IDEST = IERB + 2
      GO TO (30,40,40,30) , IDEST
C
C LEFT PAREN WAS FIRST CHARACTER FOUND, OR NAME COULDN'T
C  FIT INTO 12 CHARACTERS
C
   30 CONTINUE
      CALL STER26(7,1)
      IERID = 3
      GO TO 9999
C
C CHECK TO SEE IF ID IS AN ALLOWED NAME
C
   40 CONTINUE
C
      NPC = NPACK
   41 CONTINUE
      IF (TNAME(NPC).NE.BLANK) GO TO 42
      NPC = NPC-1
      IF (NPC.NE.0) GO TO 41
      GO TO 45
C
   42 CONTINUE
      INAME = IKEY26(TNAME,NPC,SUID,LSUID,NSUID,NDSUID)
      IF (INAME.EQ.19) GO TO 45
      IF (INAME.GT.0) GO TO  50
C
   45 CONTINUE
      IERID = 1
      GO TO 9999
C
   50 CONTINUE
C
C IF A ( IS FOUND IN STRING THEN GET THE SPEC NUMBER OF THE S/U
C
      IF (IDEST.NE.2) GO TO 9999
C
      IEND = LRPAR-1
      IF (LRPAR.EQ.0) IEND = LEN
      IBGN = LLPAR+1
      CALL UFINFX(ILEVEL,ISTRT,IBGN,IEND,IST)
      IF (IST.EQ.0) GO TO 9999
C
      CALL STER26(5,IBGN)
      IERID = 2
C
 9999 CONTINUE
      RETURN
      END
