      SUBROUTINE CALXS55(I1,IN1,J,LK,HS,AS,BS,X,FKC,FMC,FKF,FMF,FKO,FMO,
     * BSS,ASS,IFXC,K2,K4,K9)
C  CALIBRATION CROSS SECTION
      COMMON/M3255/IOBS,KTERM,KPL,JNK,TEH
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU

      DIMENSION HS(K9,K2,1),AS(K9,K2,1),BS(K9,K2,1),IFXC(K2,1)
      DIMENSION X(K2,1),FKC(K4,1),FMC(K4,1),FKF(K4,1),FMF(K4,1)
      DIMENSION FKO(K4,1),FMO(K4,1),BSS(K9,K2,1),ASS(K9,K2,1)
C
      CHARACTER*8 SNAME

C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/calxs55.f,v $
     . $',                                                             '
     .$Id: calxs55.f,v 1.2 2004/02/02 20:34:16 jgofus Exp $
     . $' /
C    ===================================================================
C
      DATA SNAME/'CALXS55 '/

      CALL FPRBUG(SNAME, 1, 55, IBUG)

      FKCC=FKC(LK,J)
      FMCC=FMC(LK,J)
      FKFF=FKF(LK,J)
      FMFF=FMF(LK,J)
      AS(1,I1,J)=0.
      HS1=HS(1,I1,J)
      BS(1,I1,J)=0.0
      IF(FMCC.LT.0.0001) BS(1,I1,J)=FKCC
      BS(2,I1,J)=FKCC*(HS(2,I1,J)-HS1)**FMCC
      BS(3,I1,J)=FKCC*(HS(3,I1,J)-HS1)**FMCC
      IF(FKFF.GT.0.0001) GO TO 60
      BS(4,I1,J)=FKCC*(HS(4,I1,J)-HS1)**FMCC
      BS(5,I1,J)=FKCC*(HS(5,I1,J)-HS1)**FMCC
      BS(6,I1,J)=BS(5,I1,J)
      GO TO 65
   60 HS1=HS(3,I1,J)
      BC=BS(3,I1,J)
      BS(4,I1,J)=(FKFF*(HS(4,I1,J)-HS1)**FMFF)+BC
      BS(5,I1,J)=(FKFF*(HS(5,I1,J)-HS1)**FMFF)+BC
      BS(6,I1,J)=BS(5,I1,J)
   65 BSS(1,I1,J)=0.
      BSS(2,I1,J)=0.
      ASS(1,I1,J)=0.
      DO 67 K=2,6
      KS=K-1
      DH=HS(K,I1,J)-HS(KS,I1,J)
      AS(K,I1,J)=AS(KS,I1,J)+0.5*DH*(BS(KS,I1,J)+BS(K,I1,J))
      ASS(K,I1,J)=ASS(KS,I1,J)+0.5*DH*(BSS(KS,I1,J)+BSS(K,I1,J))
   67 CONTINUE
      FK=FKO(LK,J)
      FM=FMO(LK,J)
      IF(FK.LT.0.0001) GO TO 70
      HS1=HS(3,I1,J)
      BSS(3,I1,J)=0.0
      IF(FM.LT.0.0001) BSS(3,I1,J)=FK
      BSS(4,I1,J)=FK*(HS(4,I1,J)-HS1)**FM
      BSS(5,I1,J)=FK*(HS(5,I1,J)-HS1)**FM
      BSS(6,I1,J)=BSS(5,I1,J)
      ASS(1,I1,J)=0.0
      DO 68 K=2,4
      KS=K-1
      DH=HS(K,I1,J)-HS(KS,I1,J)
      ASS(K,I1,J)=ASS(KS,I1,J)+0.5*DH*(BSS(KS,I1,J)+BSS(K,I1,J))
   68 CONTINUE
   70 CONTINUE
      I11=I1+1
      DO 170 I=I11,IN1
      IF(IFXC(I,J).EQ.1) GO TO 170
      DO 165 K=1,6
      BS(K,I,J)=BS(K,I1,J)
      AS(K,I,J)=AS(K,I1,J)
      BSS(K,I,J)=BSS(K,I1,J)
  165 ASS(K,I,J)=ASS(K,I1,J)
  170 CONTINUE
      IF(IBUG.EQ.1) WRITE(IODBUG,90) J,LK
      IF(IBUG.EQ.1) WRITE(IODBUG,98) FKCC,FMCC,FKFF,FMFF,FKO(LK,J),
     .  FMO(LK,J)
      IF(IBUG.EQ.1) WRITE(IODBUG,185)
      IF(IBUG.EQ.1) WRITE(IODBUG,95) (X(I,J),I=I1,IN1)
      IF(IBUG.EQ.1) WRITE(IODBUG,185)
      IF(IBUG.EQ.1) WRITE(IODBUG,190) (IFXC(I,J),I=I1,IN1)
      IF(IBUG.EQ.1) WRITE(IODBUG,185)
      IF(IBUG.EQ.1) WRITE(IODBUG,191) (HS(K,I1,J),K=1,6)
      IF(IBUG.EQ.1) WRITE(IODBUG,192) (BS(K,I1,J),K=1,6)
      IF(IBUG.EQ.1) WRITE(IODBUG,193) (AS(K,I1,J),K=1,6)
      IF(FKO(LK,J).GE.0.001) THEN
        IF(IBUG.EQ.1) WRITE(IODBUG,194) (BSS(K,I1,J),K=1,4)
        IF(IBUG.EQ.1) WRITE(IODBUG,195) (ASS(K,I1,J),K=1,4)
      END IF
   90 FORMAT(/2X,'RIVER NO. ',I2,5X,'MANNING N REACH NO. ',I3)
   95 FORMAT(10X,6HX(I,J),8F10.2)
   98 FORMAT(/10X,4HFKC=,F10.4,5X,4HFMC=,F10.4,5X,4HFKF=,F10.4,5X,
     1  4HFMF=,F10.4,5X,4HFKO=,F10.4,5X,4HFMO=,F10.4)
  185 FORMAT(1X)
  190 FORMAT(10X,'IFXC=',8I10)
  191 FORMAT(12X,'HS=',8F10.2)
  192 FORMAT(12X,'BS=',8F10.2)
  193 FORMAT(12X,'AS=',8F10.0)
  194 FORMAT(11X,'BSS=',8F10.2)
  195 FORMAT(11X,'ASS=',8F10.0)
CC  100 IF(IBUG.EQ.1) WRITE(IODBUG,11111)
11111 FORMAT(1X,'** EXIT CALXS **')
      RETURN
      END
