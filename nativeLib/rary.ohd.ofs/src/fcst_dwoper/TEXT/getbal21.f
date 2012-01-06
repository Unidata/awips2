      SUBROUTINE GETBAL21(STN,ITHL,EHL,BAL,EHLO,BALO,LAG)

C  THIS SUBROUTINE COMPUTES THE BALANCE (DIFFERENCE) BETWEEN THE NOS
C  AND OBSERVED TIDE AT THE PEAKS AND VALLEYS
C
      INCLUDE 'common/fdbug'
      DIMENSION STN(*)
      CHARACTER*8  SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_dwoper/RCS/getbal21.f,v $
     . $',                                                             '
     .$Id: getbal21.f,v 1.2 2000/09/27 16:11:41 page Exp $
     . $' /
C    ===================================================================
C

      DATA  SNAME / 'GETBAL21' /
C
C
      CALL FPRBUG(SNAME,1,21,IBUG)

      I=ITHL-LAG
      IF(STN(I).GT.-900.) THEN
        BALO=BAL
        BAL=EHL-STN(I)
      ELSE
        BAL=BALO
        EHL=EHLO
      ENDIF
      IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
 9000 FORMAT(1H0,'** ',A,' EXITED.')
      RETURN
      END
