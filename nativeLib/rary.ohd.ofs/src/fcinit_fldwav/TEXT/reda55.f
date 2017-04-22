      SUBROUTINE REDA55(IVAR,N)

C     THIS SUBROUTINE TAKES THE INPUT CARDS AND READS THE ARRAYS AS
C     INTERGER OR REAL.

      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
      DIMENSION IVAR(N)
      DIMENSION SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/reda55.f,v $
     . $',                                                             '
     .$Id: reda55.f,v 1.2 2002/02/11 19:04:19 dws Exp $
     . $' /
C    ===================================================================
C
      DATA SNAME/4HREDA,4H55  /

      CALL FPRBUG(SNAME, 1, 55, IBUG)

      READ(IN,'(A)',END=1000) DESC
      READ(IN,*,ERR=551) IVAR
      GO TO 553
551   WRITE (IPR,552)
552   FORMAT ('0**ERROR** READING VALUES FOR ?.')
553   IF(IBUG.EQ.1) WRITE(IODBUG,10) IVAR
   10 FORMAT(5X,15I5)
      GO TO 50

 1000 IF(IBUG.EQ.1) WRITE(IODBUG,1010)
 1010 FORMAT(/5X,'**ERROR** END OF FILE ENCOUNTERED WHILE READING INPUT
     *.  PROGRAM TERMINATED.'/)
   50 RETURN
      END
