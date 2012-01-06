C MEMBER USET26
C  (from old member FCUSET26)
C
C DESC CALLED AFTER A 'SET' COMMAND IS FOUND TO DETERMINE THE NAME
C DESC AND VALUE OF 'SET' VARIABLE
C
C....................................................................
C
      SUBROUTINE USET26(IRC)
C
C...................................................................
C  THIS ROUTINE LOOKS FOR A VALID NAME AND VALUE FOR THE 'SET' VARIABLE
C  AFTER A 'SET' COMMAND IS FOUND.
C  VALIDITY IS THIS: THE NAME IS AN ALLOWABLE NAME (I.E. NOT RESTRICTED
C  FOR USE AS A VARIABLE NAME), AN '=' IS FOUND AFTER THE NAME AND
C  THE VALUE PROVIDE IS A VALID REAL NUMBER.
C
C  IF EVERYTHING IS OK, STORE THE SET VARIABLE IN /CMPV26/.
C......................................................................
C  ARGUMENT LIST:
C
C    IRC - RETURN CODE =0, NO ERRORS FOUND
C                      =1, AT LEAST ONE ERROR FOUND AND PROPER ERROR
C                          CODE SET IN /ERR26/
C
C......................................................................
C
C  JTOSTROWSKI - HRL - MARCH 1983
C................................................................
      DIMENSION TNAME(3),TNUM(5)
C
      INCLUDE 'common/fld26'
      INCLUDE 'common/cmpv26'
      INCLUDE 'common/err26'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/uset26.f,v $
     . $',                                                             '
     .$Id: uset26.f,v 1.2 1999/04/22 15:20:10 page Exp $
     . $' /
C    ===================================================================
C
C
      IRC = 0
      IZERO = 0
      NPACK = 3
      NNUM = 5
C
C  GET NEXT FIELD AFTER 'SET'
C
      CALL UFLD26(-2,IRF)
      IF (IRF.GT.0) GO TO 9000
C
C  RETURN SUB STRING PRECEDING THE '='
C
      CALL UBEF26(NPACK,TNAME,ISTRT,LEQUAL,LEN,IERB)
C
      ISTNUM = ISTRT
      IBNUM = LEQUAL + 1
      LENUM = LEN
C
      IDEST = IERB + 2
      GO TO (100,200,200,100), IDEST
C
C  EITHER THE '=' WAS FIRST OR THE PREFIX STRING WAS > 12 CHARS
C
  100 CONTINUE
      CALL STER26(9,1)
      GO TO 9900
C
C  VALID ID WAS FOUND (EITHER W/ OR W/OUT AN '='
C
  200 CONTINUE
C
C C  SEE IF ID ENTERED IS ALLOWABLE
C
      ISET = IDCK26(TNAME,NPACK)
C
      IF (ISET.EQ.0) GO TO 300
C
C  INVALID ID
C
      CALL STER26(9,1)
      GO TO 9900
C
  300 CONTINUE
C
C  IF '=' WAS FOUND IN FIRST STRING JUST GO GET VARIABLE VALUE
C  IF NOT, READ NEXT FIELD TO LOOK FOR '='
C
      IF (IDEST.EQ.2) GO TO 305
C
      CALL UFLD26(-2,IRF)
      IF (IRF.GT.0) GO TO 9000
C
      ISTNUM = ISTRT
      IBNUM = LEQUAL + 1
      LENUM = LEN
C
  305 CONTINUE
      CALL UAFT26(NNUM,TNUM,ISTRT,LEQUAL,LEN,IERA)
C
      IAST = IERA + 2
C
      GO TO (330,340,320,310), IAST
C
C FIELD COULD NOT BE PACKED INTO 5A4
C
  310 CONTINUE
      CALL STER26(20,1)
      GO TO 9900
C
C  '=' WAS EXPECTED, BUT NOT FOUND
C
  320 CONTINUE
      CALL STER26(10,1)
      GO TO 9900
C
C NOTHING FOUND AFTER '='; GO GET IT.
C
  330 CONTINUE
      CALL UFLD26(-2,IRF)
C
      IF (IRF.GT.0) GO TO 9000
      ISTNUM = ISTRT
      IBNUM = 1
      LENUM = LEN
C
C  HAVE VALID VALUE FIELD. NOW DETERMINE NUMERICAL VALUE.
C
  340 CALL UFRLFX(VALUE,ISTNUM,IBNUM,LENUM,IZERO,IER)
C
      IF (IER.EQ.0) GO TO 500
      CALL STER26(4,1)
      GO TO 9900
C
C  STORE SET VARIABLE NAME AND VALUE IN /CMPV26/
C
  500 CONTINUE
      NUMCMP = NUMCMP + 1
      IF (NUMCMP.LE.MAXCMP) GO TO 510
C
      CALL STER26(502,1)
      GO TO 9900
C
  510 CONTINUE
      ICTARY(NUMCMP) = 2
      DO 520 I=1,3
  520 CMNAME(I,NUMCMP) = TNAME(I)
      CMPVAL(NUMCMP) = VALUE
      ICMPTY(NUMCMP) = -1
      GO TO 9999
C
C  SET ADDITIONAL ERRORS HERE
C
 9000 CONTINUE
      IF (IRF.EQ.1) CALL STER26(19,1)
      IF (IRF.EQ.4) CALL STER26(7,1)
C
C  SET ERROR CODE
C
 9900 CONTINUE
      IRC = 1
C
 9999 CONTINUE
      RETURN
      END
