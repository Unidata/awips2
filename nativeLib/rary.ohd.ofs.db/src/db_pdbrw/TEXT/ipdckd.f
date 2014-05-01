C MODULE IPDCK
C-----------------------------------------------------------------------
C
      FUNCTION IPDCKD (DTYPE)
C
C  ROUTINE TO FIND A DAILY DATA TYPE IN THE DIRECTORY AND RETURN
C  THE SUBSCRIPT OF THE TYPE IN THE DATA DIRECTORY.
C
C  IF THE TYPE IS PP01, PP03 OR PP06, RETURNS PPVR SUBSCRIPT.
C  IF THE TYPE IS TA01, TA03 OR TA06, RETURNS TAVR SUBSCRIPT.
C  IF THE TYPE IS TFMN OR TFMX, RETURNS TF24 SUBSCRIPT.
C
      CHARACTER*4 DTYPE
      INTEGER*2 ITYPE(2)
C
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'urcommon/ursifc'
      INCLUDE 'urcommon/urpddt'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/ipdckd.f,v $
     . $',                                                             '
     .$Id: ipdckd.f,v 1.2 2002/02/11 19:20:13 dws Exp $
     . $' /
C    ===================================================================
C
C
      IPDCKD=0
C
      CALL UMEMOV (DTYPE,ITYPE,1)
C
      IF (IAMORD.EQ.0) NTYPE=NMDTYP
      IF (IAMORD.EQ.1) NTYPE=NPDTYP
C
      DO 10 I=1,NTYPE
         IF (IAMORD.EQ.0.AND.
     *       IDDTDR(2,I).EQ.ITYPE(1).AND.IDDTDR(3,I).EQ.ITYPE(2))
     *      GO TO 20
         IF (IAMORD.EQ.1.AND.
     *       JDDTDR(2,I).EQ.ITYPE(1).AND.JDDTDR(3,I).EQ.ITYPE(2))
     *      GO TO 20
10       CONTINUE
      GO TO 40
C
C  CHECK IF NEED ADJUSTMENT FOR WRITE ONLY TYPE
20    IF (IAMORD.EQ.0.AND.IDDTDR(4,I).LT.0) GO TO 30
      IF (IAMORD.EQ.1.AND.JDDTDR(4,I).LT.0) GO TO 30
         IPDCKD=I
         GO TO 40
30    IPDCKD=-IDDTDR(4,I)
C
40    IF (IPDTR.GT.0) WRITE (IOGDB,50) ITYPE,IPDCKD
50    FORMAT (' EXIT IPDCKD - ITYPE=',2A2,' IPDCKD=',I4)
C
      RETURN
C
      END
