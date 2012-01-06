C MEMBER SYMM22
C***************************************************************
       SUBROUTINE SYMM22 (N,CX)
C
C
C  THIS ROUTINE IS PART OF THE TSFP PROGRAM, VERSION 2
C  CREATED BY KONSTANTINE P. GEORGAKAKOS, MIT, MAY-1982
C
C  ENFORCES SYMMETRY ON A COVARIANCE MATRIX
C
C  EACH OFF-DIAGONAL ELEMENT OBTAINS THE VALUE DEFINED BY THE AVERAGE
C  OF THE INITIAL VALUES THAT IT AND ITS SYMMETRIC HAVE
C
C**********************************************************************
C
C...for debugging:
       COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C**********************************************************************
C
          DIMENSION CX(12,12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sssac/RCS/symm22.f,v $
     . $',                                                             '
     .$Id: symm22.f,v 1.2 2002/05/15 13:50:45 hank Exp $
     . $' /
C    ===================================================================
C
C
        IF (ITRACE .GT. 1) WRITE (IODBUG,990)
990     FORMAT(/10X,'** SYMM22 ENTERED.')
C
          IF(N.NE.1) GOTO 800
              IF (ITRACE .GT. 1) WRITE (IODBUG,991)
991           FORMAT(/10X,'** EXIT SYMM22.')
              RETURN
800       CONTINUE
C
          DO 10 I=2,N
          N2 = I-1
          DO 10 J=1,N2
          TEMP=(CX(I,J)+CX(J,I))/2.
          CX(I,J)=TEMP
          CX(J,I)=TEMP
  10          CONTINUE
C
          IF (ITRACE .GT. 1) WRITE (IODBUG,991)
C
          RETURN
          END
