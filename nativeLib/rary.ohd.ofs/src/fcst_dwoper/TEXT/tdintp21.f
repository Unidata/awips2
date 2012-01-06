      SUBROUTINE TDINTP21(B1,B2,I1,I2,IB,TDNOS,TD)

C  THIS SUBROUTINE INTERPOLATES BETWEEN THE HI & LO BALANCES

      INCLUDE 'common/fdbug'
      CHARACTER*8  SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_dwoper/RCS/tdintp21.f,v $
     . $',                                                             '
     .$Id: tdintp21.f,v 1.2 2000/09/27 16:13:29 page Exp $
     . $' /
C    ===================================================================
C
      DATA  SNAME / 'TDINTP21' /
C
      CALL FPRBUG(SNAME,1,21,IBUG)

      RAT=(IB-I1)/(I2-I1)
      BAL=B1+RAT*(B2-B1)
      TD=TDNOS-BAL

      IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
 9000 FORMAT(1H0,'** ',A,' EXITED.')
      RETURN
      END
