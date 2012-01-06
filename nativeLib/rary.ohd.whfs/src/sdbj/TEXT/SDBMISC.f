c ---------------------------------------------------------------------
      SUBROUTINE CLSCR

C         THIS SUBROUTINE CLEARS THE SCREEN (CLS COMMAND ON PC)

      CHARACTER*1 CLS(4),HOME(6)
C
      CLS(1)=CHAR(27)
      CLS(2)=CHAR(91)
      CLS(3)=CHAR(50)
      CLS(4)=CHAR(74)

      HOME(1)=CLS(1)
      HOME(2)=CLS(2)
      HOME(3)=CHAR(49)
      HOME(4)=CHAR(59)
      HOME(5)=CHAR(49)
      HOME(6)=CHAR(72)
      PRINT*, CLS
      PRINT*, HOME
      RETURN
      END
c ---------------------------------------------------------------------
      SUBROUTINE CLOS

C         THIS SUBROUTINE CLOSES ALL THE FILES IN THIS PROGRAM
C
      CLOSE(5)
      CLOSE(6)
      CLOSE(7)
      CLOSE(8)
C
      RETURN
      END
c ---------------------------------------------------------------------
      SUBROUTINE WAIT
      CHARACTER KEYY
      PRINT 10
   10 FORMAT(/'                             HIT ENTER TO CONTINUE')
      READ '(A)', KEYY
      RETURN
      END
c ---------------------------------------------------------------------
      SUBROUTINE EXIT
      PRINT *, 'BEFORE STOP IN MISC'
      STOP
      END
