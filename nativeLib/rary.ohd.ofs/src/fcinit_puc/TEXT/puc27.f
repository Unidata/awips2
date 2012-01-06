C MEMBER PUC27
C  (from old member FCPUC27)
C
C    THIS IS THE CARD PUNCH ROUTINE FOR THE WGRFC TABULAR OUTPUT
C    OPERATION
C
C    DAVE REED  WGRFC  7/87
C
C
      SUBROUTINE PUC27(P)
C
C   DIMENSION AND COMMON
C
      DIMENSION P(1),IP(22),SNAME(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc27.f,v $
     . $',                                                             '
     .$Id: puc27.f,v 1.1 1995/09/17 18:50:50 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/4HPUC2,4H7   /
C
C    CHECK TRACE LEVEL
C
      IF(ITRACE.GE.1)WRITE(IODBUG,900)
  900 FORMAT(1H0,19H** PUC27 ENTERED **)
C
C   CHECK FOR DEBUG AND PRINT IF NEEDED
C
      CALL FPRBUG(SNAME,1,27,IBUG)
      IF(IBUG.EQ.0)GOTO 90
      WRITE(IODBUG,1900)(P(J),J=1,30)
 1900 FORMAT(1H0,11HP(  1-  6) ,6F5.0,8H P(7-11),5(1X,A4),/,
     110X,11HP( 12- 30) ,20F5.0)
      NTITL=P(3)
      IPT=30
      DO 1100 I=1,NTITL
      IP1=IPT+1
      IP2=IP1+17
      WRITE(IODBUG,1903)IP1,IP2,(P(J),J=IP1,IP2)
 1903 FORMAT(1H ,2HP(,I3,1H-,I3,2H) ,18A4)
      IPT=IPT+18
 1100 CONTINUE
      IPT=30+NTITL*18
      NTS=P(4)
      DO 1110 I=1,NTS
      IP1=IPT+1
      IP2=IP1+17
      WRITE(IODBUG,1904)IP1,IP2,(P(J),J=IP1,IP2)
 1904 FORMAT(1H ,2HP(,I3,1H-,I3,2H) ,2A4,1X,A4,1X,3F6.0,
     11X,A4,7F6.0,2(1X,A4),2F10.3)
      IPT=IPT+18
 1110 CONTINUE
   90 CONTINUE
C
C    PUNCH CARD NUMBER 1
C
      DO 91 I=1,22
   91 IP(I)=P(I)
      WRITE(IPU,901)(IP(I),I=3,6),(P(J),J=7,11),
     1(IP(J),J=12,22)
C
  901 FORMAT(3X,I2,3X,I2,4X,I1,2X,I3,1X,A4,1X,A4,1X,A4,2X,2A4,
     13X,I2,10I2)
C
C    PRINT OUT TITLE LINES
C
      NTITL=P(3)
      DO 100 I=1,NTITL
C
      I1=(I-1)*18+31
      I2=I1+17
  100 WRITE(IPU,902)(P(J),J=I1,I2)
  902 FORMAT(18A4)
C
C    PUNCH OUT TIME SERIES INFORMATION
C
      NTS=P(4)
      DO 200 I=1,NTS
      IPT=30+NTITL*18+(I-1)*18
      I4=P(IPT+4)
      I5=P(IPT+5)
      I6=P(IPT+6)
      I8=P(IPT+8)
      I9=P(IPT+9)
      I10=P(IPT+10)
      I11=P(IPT+11)
      I12=P(IPT+12)
      WRITE(IPU,903)(P(IPT+J),J=1,3),I4,I5,I6,P(IPT+7),I8,
     1I9,I10,I11,I12
  903 FORMAT(2A4,2X,A4,4X,I2,3X,I2,4X,I1,1X,A4,3X,I2,3X,I2,3X,I2,
     14X,I1,4X,I1)
C
      IF(P(IPT+11).LT.0.5)GOTO 200
C
C    PRINT OUT CRITERIA INPUT
C
      I14=P(IPT+14)
      WRITE(IPU,904)P(IPT+13),I14
  904 FORMAT(F15.5,4X,I1)
  200 CONTINUE
C
C
      IF(ITRACE.GE.1)WRITE(IODBUG,905)
  905 FORMAT(1H0,18H** PUC27 EXITED **)
C
C
      RETURN
      END
