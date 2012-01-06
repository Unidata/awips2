C MEMBER PUC39
C  (from old member FCPUC39)
C
C    THIS IS THE PUNCH PARAMETER SUBROUTINE FOR THE
C    WEST GULF RFC TABLE LOOKUP OPERATION
C
C    DAVE REED WGRFC  7/87
C
C
      SUBROUTINE PUC39(P)
C
C
C    DIMENSION STMTS
C
      DIMENSION P(1),SNAME(2),PUCDAT(2,3)
C
C     DATA SNAME/4HPUC3,4H9   /
C
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc39.f,v $
     . $',                                                             '
     .$Id: puc39.f,v 1.4 2002/10/10 13:35:58 xfan Exp $
     . $' /
C    ===================================================================
C
C
C
      IF(ITRACE.GE.1)WRITE(IODBUG,900)
  900 FORMAT(1H0,19H** PUC39 ENTERED **)
C
C    PUNCH OUT CARD 1
C
      I3=P(3)
      I4=P(4)
      WRITE(IPU,901)I3,I4
  901 FORMAT(I5,4X,I1)
C
C    PRINT OUT TIME SERIES INFORMATION
C
      I14=P(14)
      I18=P(18)
      WRITE(IPU,902)P(11),P(12),P(13),I14
  902 FORMAT(2A4,2X,A4,4X,I2)
C
      WRITE(IPU,902)P(15),P(16),P(17),I18
C
C    RETRIEVE ENGLISH/METRIC CONVERSION CONSTANTSA IF NEEDED
C
      IF(P(4).GT.0.5)GOTO 140
C
      CALL FDCODE(P(13),RIMET,DIMS,I1,I2,I3,I4,I5)
      CALL FCONVT(RIMET,DIMS,RIENG,RMI,RBI,IER)
C
      CALL FDCODE(P(17),ROMET,DIMS,I1,I2,I3,I4,I5)
      CALL FCONVT(ROMET,DIMS,ROENG,RMO,RBO,IER)
  140 CONTINUE
C
C
C
C    NOW PUNCH CARDS FOR TABLE LOOKUP VALUES
C
      NP=P(3)
      DO 100 I=1,NP,3
C    CARDS HAVE 3 PAIRS OF LOOKUP VALUES
C
      I1=I
      I2=I1+2
      IF(I2.GT.NP)I2=NP
C
C
      NPC=0
C
      DO 110 J=I1,I2
      J1=(J-1)*2+19
C
      PUCDAT(1,J-I1+1)=P(J1)
      PUCDAT(2,J-I1+1)=P(J1+1)
  110 NPC=NPC+1
C
C   PUNCH OUT THE CARD
C
C
C    CONVERT TO ENGLISH UNITS IF NECESSARY
C
      IF(P(4).GT.0.5)GOTO 141
      DO 142 L=1,NPC
      PUCDAT(1,L)=PUCDAT(1,L)*RMI+RBI
C jg added same code as in prp39 to make output compatible     
C  142 PUCDAT(2,L)=PUCDAT(2,L)*RMO+RBO
      PUCDAT(2,L)=PUCDAT(2,L)*RMO+RBO

C.....FIND NUMBER OF SIGNIFICANT DIGITS IN EACH LOOKUP POINT...AND
C.....ROUND OFF TO WHOLE NUMBER (for lookup entry).
C jg this comment no longer true based on MR 1675 - 7/02

C
      DO 150 JP = 1, 11
      KP = JP - 1
      X = 10.**KP
      IF(PUCDAT(1,L) .LE. X) GOTO 160
  150 CONTINUE
C
C.....DO NOT LET NUMBER OF SIGNIFICANT DIGITS EXCEED 5.
C
C jgg  160 IF(KP .GT. 5) KP = 5
C  For both values, let sig. digits = 5 to preserve decimal values -MR 1675-7/02
  160 KP = 5
  
      CALL FSIGFG(PUCDAT(1,L), KP, IER)      

C jgg      KP = 5
      CALL FSIGFG(PUCDAT(2,L), KP, IER)    
      
  142 CONTINUE
C jgg  end of additions

  141 CONTINUE
C
      WRITE(IPU,906)(PUCDAT(1,K),PUCDAT(2,K),K=1,NPC)
  906 FORMAT(6F12.4)
  100 CONTINUE
C
C
      IF(ITRACE.GE.1)WRITE(IODBUG,907)
  907 FORMAT(1H0,18H** PUC39 EXITED **)
      RETURN
      END
