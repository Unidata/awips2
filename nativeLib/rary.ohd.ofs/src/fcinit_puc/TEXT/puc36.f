C MEMBER PUC36
C  (from old member FCPUC36)
C
      SUBROUTINE PUC36(PL,CL)
C.......................................
C     THIS SUBROUTINE PUNCHES PARAMETERS AND CARRYOVER VALUES FOR
C        THE XINANJIANG RAILFALL-RUNOFF OPERATION IN THE
C        FORMAT REQUIRED BY THE INPUT SUBROUTINE PIN36.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C            QINGPING ZHU -YRCC CHINA JULY 1988   VERSION 1
C.......................................
      DIMENSION PL(1),CL(1)
      DIMENSION TSID(2)
C
C     COMMON BLOCKS
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc36.f,v $
     . $',                                                             '
     .$Id: puc36.f,v 1.1 1995/09/17 18:50:56 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA BLANK,SUMS,PRST/4H    ,4HSUMS,4HPRST/
C.......................................
C     CHECK TRACE LEVEL -- TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF (ITRACE.GE.1) WRITE (IODBUG,900)
  900 FORMAT (1H0,16H** PUC36 ENTERED)
C     NO DEBUG OUTPUT FOR THIS SUBROUTINE.
C.......................................
C     PUNCH CARD NO. 1
      IDT=PL(2)
      NUNIT=PL(3)
      L=PL(9)
      IF(L.EQ.0) GO TO 100
      STORE=SUMS
      GO TO 110
  100 STORE=BLANK
  110 L=PL(10)
      IF(L.EQ.0) GO TO 120
      PROT=PRST
      GO TO 130
  120 PROT=BLANK
  130 WRITE (IPU,901) (PL(I),I=4,8),NUNIT,IDT,PROT,STORE
  901 FORMAT (5A4,3X,I2,3X,I2,10X,A4,1X,A4)
C.......................................
C     PUNCH CARD NO. 2
      J=PL(19)-1
      IOPTET=PL(20)
      WRITE(IPU,902) (PL(I+J),I=1,13),IOPTET
  902 FORMAT(F5.2,F5.3,4F5.0,2F5.2,5F5.3,4X,I1)
C.......................................
C     PUNCH CARD NO. 3
      IET=PL(11)
      IF(IET.EQ.1) GO TO 140
      J=PL(18)-1
      WRITE(IPU,906) (PL(I+J),I=1,12)
  906 FORMAT(20X,12F4.1)
      GO TO 150
  140 TSID(1)=PL(12)
      TSID(2)=PL(13)
      TSTYPE=PL(14)
      J=PL(18)-1
      WRITE(IPU,903) TSID,TSTYPE,(PL(I+J),I=1,12)
  903 FORMAT(2X,2A4,1X,A4,5X,12F4.2)
C.......................................
C     PUNCH CARD NO.4 -NO. 3+NUNIT
  150 J=PL(15)-1
      DO 160 I=1,NUNIT
      WRITE(IPU,904) (PL(M+J),M=1,6)
  904 FORMAT(2X,2A4,1X,A4,5X,2A4,1X,A4)
      J=J+6
  160 CONTINUE
C.......................................
C      PUNCH CL ARRAY
      J=0
      DO 170 I=1,NUNIT
      WRITE(IPU,905) (CL(M+J),M=1,7)
  905 FORMAT(4F5.1,2F5.2,F5.3)
      J=J+7
  170 CONTINUE
      RETURN
      END
