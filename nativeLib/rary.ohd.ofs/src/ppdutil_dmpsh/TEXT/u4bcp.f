C  MEMBER U4BCP
C  =====================================================================
C  PGM: U4BCP(STR1,STR2,ISTAT) .. COMPARE 2 4-BYTE VARS WITH + WILD CHR
C
C   IN: STR1 ..... FIRST 4-BYTE VARIABLE TO BE COMPARED
C   IN: STR2 ..... SECOND 4-BYTE VARIABLE TO BE COMPARED
C  OUT: ISTAT .... ISTAT SET TO 0 IF VARS ARE THE SAME, ELSE SET TO 1
C
C  CMT: EITHER INPUT VARIABLE CAN CONTAIN A + AS A WILD CHR CAUSING THAT
C  CMT:  BYTE TO BE SKIPPED IN THE COMPARISON.
C  =====================================================================
      SUBROUTINE U4BCP(STR1,STR2,ISTAT)

      LOGICAL*1  STR1(4),STR2(4),S1,S2
      INTEGER    IS1,IS2,IPLUS,ISTAT
      EQUIVALENCE( S1,IS1 ),( S2,IS2 )
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil_dmpsh/RCS/u4bcp.f,v $
     . $',                                                             '
     .$Id: u4bcp.f,v 1.1 1995/09/17 19:10:26 dws Exp $
     . $' /
C    ===================================================================
C
      DATA       IPLUS,IS1,IS2 / 4h+   ,4h    ,4h     /

        ISTAT = 1

        S1 = STR1(1)
        S2 = STR2(1)
        IF( IS1 .EQ. IPLUS ) GO TO 110
        IF( IS2 .EQ. IPLUS ) GO TO 110
        IF( IS1 .NE. IS2   ) GO TO 200

  110   S1 = STR1(2)
        S2 = STR2(2)
        IF( IS1 .EQ. IPLUS ) GO TO 120
        IF( IS2 .EQ. IPLUS ) GO TO 120
        IF( IS1 .NE. IS2   ) GO TO 200

  120   S1 = STR1(3)
        S2 = STR2(3)
        IF( IS1 .EQ. IPLUS ) GO TO 130
        IF( IS2 .EQ. IPLUS ) GO TO 130
        IF( IS1 .NE. IS2   ) GO TO 200

  130   S1 = STR1(4)
        S2 = STR2(4)
        IF( IS1 .EQ. IPLUS ) GO TO 140
        IF( IS2 .EQ. IPLUS ) GO TO 140
        IF( IS1 .NE. IS2   ) GO TO 200

  140   ISTAT = 0

  200   CONTINUE

      RETURN
      END

