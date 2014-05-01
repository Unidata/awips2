C MEMBER IPDCDW
C  (from old member PDCRSUBS)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/19/95.13:35:33 BY $WC20SV
C
C @PROCESS LVL(77)
C
      FUNCTION IPDCDW (ITYPE)
C
C  ROUTINE TO FIND A DAILY DATA TYPE IN THE DIRECTORY AND RETURN
C  THE SUBSCRIPT OF THE TYPE.
C  RETURNS THE EXACT SUBSCRIPT EVEN IF A WRITE ONLY TYPE.
C
      INTEGER*2 ITYPE(2)
C
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdsifc'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/ipdcdw.f,v $
     . $',                                                             '
     .$Id: ipdcdw.f,v 1.1 1995/09/17 18:43:23 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,10) ITYPE,NMDTYP
10    FORMAT (' *** ENTER IPDCDW - ITYPE=',2A2,' NMDTYP=',I4)
C
      DO 20 I=1,NMDTYP
         IF (IDDTDR(2,I).EQ.ITYPE(1).AND.IDDTDR(3,I).EQ.ITYPE(2))
     *      GO TO 30
20       CONTINUE
C
C  TYPE NOT FOUND
      IPDCDW=0
      GO TO 40
C
C  FOUND
30    IPDCDW=I
C
40    IF (IPDTR.GT.0) WRITE (IOGDB,50) ITYPE,IPDCDW
50    FORMAT (' *** EXIT IPDCDW - ITYPE=',2A2,' INDEX=',I4)
C
      RETURN
C
      END
