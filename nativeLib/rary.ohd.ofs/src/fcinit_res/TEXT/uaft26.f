C MODULE UAFT26
C
C DESC RETURNS THE STRING FOLLOWING SPECIFIED CHARACTER IN A4 FORMAT
C
C...............................................................
C
      SUBROUTINE UAFT26(NPACK,PACKED,ISTRT,LSRCH,LENGTH,IERR)
C
C.................................................................
C
C  THIS SUBROUTINE REQUIRES THE USE OF SUBROUTINE UFIELD TO FILL
C  /UFREEX/.
C
C..................................................................
C  ARGUMENT LIST
C
C  NPACK - NUMBER OF WORDS IN ARRAY TO HOLD FOUND STRING
C  PACKED - ARRAY TO HOLD FOUND STRING
C  ISTRT - LOCATION OF FIRST CHARACTER OF SEARCH STRING IN ICDBUF ARRAY
C  LSRCH - LOCATION IN SEARCH STRING OF CHARACTER TO BE SEARCHED FOR
C  LENGTH - LENGTH OF STRING TO BE SEARCHED
C   IERR - RETURN CODE
C          = -1, NOTHING AFTER SEARCH CHARACTER
C          =  0, SPECIFIED CHARACTER FOUND AND VALID STRING FOLLOWS
C          =  1, SPECIFIED CHARACTER NOT FOUND
C          =  2, STRING COULD NOT BE PACKED IN PACKED ARRAY
C
C.................................................................
C
C  JTOSTROWSKI - HRL - MARCH 1983
C................................................................
      DIMENSION PACKED(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/uaft26.f,v $
     . $',                                                             '
     .$Id: uaft26.f,v 1.2 1998/07/02 19:41:34 page Exp $
     . $' /
C    ===================================================================
C
C
      IERR = 0
      LBGN = LSRCH+1
      LEND = LENGTH
      IF (LEND.LT.LBGN) GO TO 999
C
      IF (LSRCH.EQ.0) LBGN = 1
      CALL UFPACK(NPACK,PACKED,ISTRT,LBGN,LEND,IERP)
      IF (LSRCH.EQ.0) IERR = 1
      IF (IERP.GT.0) IERR = 2
      RETURN
C
  999 CONTINUE
      IERR = -1
      RETURN
      END
