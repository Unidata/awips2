      SUBROUTINE WYQMET55(METRIC,I1,IN1,VAL,IKS,KVAL)
cc      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
      COMMON/IDOS55/IDOS,IFCST

      INCLUDE 'common/fdbug'

      DIMENSION VAL(KVAL)

      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/wyqmet55.f,v $
     . $',                                                             '
     .$Id: wyqmet55.f,v 1.2 2004/02/02 20:41:50 jgofus Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/'WYQMET55'/

      CALL FPRBUG(SNAME, 1, 55, IBUG)

cc      IF (IFCST.EQ.1) IBUG=1
C
C  IKS=0 FOR FLOW
C      1 FOR WSEL/DEPTH
      IF(METRIC.EQ.0) GO TO 12
      FAC=3.281
      IF(IKS.EQ.0) FAC=35.32
      DO 10 I=I1,IN1
      VAL(I)=VAL(I)/FAC
   10 CONTINUE
   12 IF(IKS.EQ.0 .AND.IBUG.EQ.1) WRITE(IODBUG,15) (VAL(I),I=I1,IN1)
      IF(IKS.EQ.1 .AND.IBUG.EQ.1) WRITE(IODBUG,16) (VAL(I),I=I1,IN1)
      IF(METRIC.EQ.0) GO TO 20
      DO 14 I=I1,IN1
      VAL(I)=VAL(I)*FAC
   14 CONTINUE
   15 FORMAT(8F10.0)
   16 FORMAT(8F10.2)
   20 RETURN
      END
