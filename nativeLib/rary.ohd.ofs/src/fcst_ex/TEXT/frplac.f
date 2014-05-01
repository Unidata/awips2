C MEMBER FRPLAC
C  (from old member FCFRPLAC)
C
      SUBROUTINE FRPLAC(QOLD,QNEW,IDT)
C.......................................
C     THIS SUBROUTINE REPLACES THE VALUES IN ONE TIME SERIES(QOLD)
C        BY THOSE IN ANOTHER TIME SERIES (QNEW) WITH THE SAME TIME
C        INTERVAL.  ONLY FOR TIME SERIES WITH ONE VALUE PER TIME
C        INTERVAL.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   NOV. 1979
C.......................................
      DIMENSION QOLD(1),QNEW(1)
C
C     COMMON BLOCKS
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/FCTIME/IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD,NOW(5),
     1   LOCAL,NOUTZ,NOUTDS,NLSTZ,IDA,IHR,LDA,LHR,IDADAT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ex/RCS/frplac.f,v $
     . $',                                                             '
     .$Id: frplac.f,v 1.1 1995/09/17 18:58:09 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL = 1.
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,17H** FRPLAC ENTERED)
C     INITIAL VALUES
C.......................................
      KDA=IDA
      KHR=IHR
C.......................................
C     FIND LOCATION OF DATA
  100 I=(KDA-IDADAT)*24/IDT+KHR/IDT
C.......................................
C     REPLACE OLD VALUES WITH NEW VALUES.
      QOLD(I)=QNEW(I)
C.......................................
C     CHECK FOR END OF EXECUTION PERIOD.
      IF((KDA.EQ.LDA).AND.(KHR.EQ.LHR)) GO TO 190
C
C     INCREMENT TO THE NEXT TIME STEP.
      KHR=KHR+IDT
      IF(KHR.LE.24) GO TO 100
      KHR=IDT
      KDA=KDA+1
      GO TO 100
C     END OF THE COMPUTATIONS.
C.......................................
  190 CONTINUE
      RETURN
      END
