C MEMBER RPNF26
C  (from old member FCRPN26)
C.......................................................................
C
      SUBROUTINE RPNF26(IOPT,STACK,WORK,IUSEW,LEFTW,NSTACK,NRPN,
     .VAL,LEVEL,MTYING)
C
C......................................................................
C  RPNF26 WILL FILL THE RPN ARRAY IN SLIGHTLY DIFFERENT WAYS DEPENDING
C  ON THE VALUE OF IOPT;
C    IOPT = 0:  UNLOAD THE STACK UNTIL AN ( IS FOUND AND THEN REMOVE THE
C               ( FROM THE STACK.
C
C    IOPT = 1:  CHECK THE PRIORITY OF THE INCOMING OPERATOR AND UNLOAD
C               THE STACK UNTIL THE PRIORITY OF THE INCOMING OPERATOR
C               IS GREATER THAN THAT OF THE TOP OF THE STACK. THEN LOAD
C               THE STACK WITH THE INCOMING OPERATOR.
C
C    IOPT = -1:  INCOMING VALUE IS AN OPERAND AND IS JAMMED INTO THE
C                RPN ARRAY DIRECTLY.
C
C......................................................................
C
      DIMENSION WORK(1),STACK(1)
      LOGICAL MTYING
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/rpnf26.f,v $
     . $',                                                             '
     .$Id: rpnf26.f,v 1.1 1995/09/17 18:52:42 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IOPT.EQ.-1) GO TO 2000
      IF (NSTACK.EQ.0.AND.MTYING) RETURN
C
      LVLSTK = STACK(1)/10
      IF (IOPT.EQ.1) GO TO 1000
C
C..................
C  IF ( IS FOUND, STOP UNLOADING AND REMOVE ( FROM STACK.
C
      IF (LVLSTK.EQ.12) GO TO 15
C
C......................
C  START UNLOADING STACK UNTIL ( IS FOUND
C
   10 NRPN = NRPN+1
      CALL FLWK26(WORK,IUSEW,LEFTW,STACK(1),501)
      CALL POP26(STACK,NSTACK)
      LVLSTK = STACK(1)/10
      IF (LVLSTK.NE.12) GO TO 10
C
C.................
C  A ( HAS BEEN FOUND. STOP UNLOADING AND REMOVE ( FROM STACK
C
   15 CALL POP26(STACK,NSTACK)
      RETURN
C..........................................
C  UNLOAD STACK INTO RPN ARRAY UNTIL PRIORITY OF STACK TOP IS LESS THAN
C  THE INCOMING OPERATOR.
C
 1000 CONTINUE
      IF (LEVEL.GT.LVLSTK) GO TO 1100
      NRPN = NRPN+1
      CALL FLWK26(WORK,IUSEW,LEFTW,STACK(1),501)
      CALL POP26(STACK,NSTACK)
      LVLSTK = STACK(1)/10
      IF (LVLSTK.EQ.0) RETURN
      IF (LVLSTK.EQ.12.AND.LEVEL.EQ.-1) RETURN
      GO TO 1000
C
 1100 CALL PUSH26(STACK,VAL,NSTACK)
      RETURN
 2000 NRPN = NRPN+1
      CALL FLWK26(WORK,IUSEW,LEFTW,VAL,501)
      RETURN
      END
