C MEMBER PRTASC
C  (from old member LOOKNSET)
C  =====================================================================
C  pgm: PRTASC .. Output character string as good chars and blanks
C
C  use:     PRTASC( unit.no, inpt.str, beg.char, end,char )
C
C   in: unit.no ....... unit number for output ( 10(1X,A4) ) format
C   in: inpt.str ...... input string of characters (packed)
C   in: beg.char ...... location of beginning char of concern in input
C   in: end.char ...... location of last char of concern in input array
C  out: (terminal or file) .. if a printable char exists, it is output
C  =====================================================================
      SUBROUTINE PRTASC(LP,IBUF,IBEG,IEND)
C
C
      INTRINSIC     CHAR,ICHAR
      CHARACTER*1   IBUF(1),KHAR,BLANK,LCHAR
      CHARACTER*50  LINE
      INTEGER    LP,IBEG,IEND,II,J1,J2
      PARAMETER( BLANK=' ' )
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/looknset/RCS/prtasc.f,v $
     . $',                                                             '
     .$Id: prtasc.f,v 1.1 1995/09/17 19:00:10 dws Exp $
     . $' /
C    ===================================================================
C
C
C
c        LCHAR = CHAR( (ICHAR(BLANK)+127) )
c  changed to only look at the 95 characters in the ASCII set
c   gfs - 22 March 1994
        LCHAR = CHAR( (ICHAR(BLANK)+94) )
 
        II = IBEG-1
        J1 = 999
C
  100   IF( II .GE. IEND ) GO TO 140
          II = II+1
          KHAR = IBUF(II)
          IF( KHAR.LT.BLANK .OR. KHAR.GT.LCHAR ) KHAR = BLANK
          IF( J1 .LT. 50 ) GO TO 120
            IF( J1 .EQ. 50 ) WRITE(LP,'(A)') LINE
            J1 = 0
            J2 = 5
  120     IF( J2 .LT. 5 ) GO TO 130
            J1 = J1+1
            LINE(J1:J1) = BLANK
            J2 = 1
  130     J1 = J1+1
          LINE(J1:J1) = KHAR
          J2 = J2+1
            GO TO 100
  140   IF( J1 .GT. 50 ) GO TO 170
  150     IF( J1 .GE. 50 ) GO TO 160
            J1 = J1+1
            LINE(J1:J1) = BLANK
              GO TO 150
  160     WRITE(LP,'(A)') LINE
C
  170   CONTINUE
C
C
      RETURN
      END
