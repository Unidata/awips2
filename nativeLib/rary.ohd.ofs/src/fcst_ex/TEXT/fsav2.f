C MEMBER FSAV2
C  (from old member FCEX2)
C
      SUBROUTINE FSAV2(K,PO,CO,C,RO)
C.......................................................................
C     THIS SUBROUTINE COMPUTES THE RUNOFF TO BE SAVED AS CARRYOVER.
C.......................................................................
C     SUBROUTINE INITIALLY WRITTEN BY
C            LARRY BRAZIL -- HRL    JANUARY 1980     VERSION 1
C.......................................................................
      DIMENSION CO(1),C(1),RO(1),PO(1)
C
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/FCTIME/IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD,
     1NOW(5),LOCAL,NOUTZ,NOUTDS,NLSTZ,IDA,IHR,LDA,LHR,IDADAT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ex/RCS/fsav2.f,v $
     . $',                                                             '
     .$Id: fsav2.f,v 1.1 1995/09/17 18:58:14 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................................................
C     CHECK TRACE LEVEL -- TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,16H** FSAV2 ENTERED)
C.......................................................................
C
C     CHECK TO SEE IF RO'S TO BE SAVED ARE PRIOR TO IDA & IHR
      NRO=PO(21)
      IDTR=PO(16)
      KRO=K-NRO+1
      KIQT=(IDA-IDADAT)*24/IDTR+IHR/IDTR
      IF(KRO.GE.KIQT) GO TO 500
      KDIFF=KIQT-KRO
      DO 400 J=1,KDIFF
      L=NRO+1-J
      I=KDIFF+1-J
      C(I)=CO(L)
  400 CONTINUE
      KDIFF=KDIFF+1
      GO TO 510
  500 KDIFF=1
  510 DO 410 I=KDIFF,NRO
      L=K-NRO+I
      C(I)=RO(L)
  410 CONTINUE
      RETURN
      END
