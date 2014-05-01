C MEMBER PRC20
C  (from old member FCPRC20)
C
      SUBROUTINE PRC20 (PO,CO)
C
C ......................................................................
C
C       THIS IS THE PRINT CARRYOVER SUBROUTINE FOR THE CHANGE TIME
C     INTERVAL OPERATION.  THIS OPERATION ONLY HAS CARRYOVER FOR THE
C     FOLLOWING CASES:
C
C     TIME SERIES 1     TIME SERIES 2     TIME INTERVAL
C     -------------     -------------     -------------
C
C     MEAN              MEAN              INCREASING
C     ACCM              ACCM              INCREASING
C     INST              INST              DECREASING
C     MEAN              INST              DECREASING
C
C ......................................................................
C
C     SUBROUTINE ORIGINALLY WRITTEN BY:
C       ED VANBLARGAN - HRL   MAY, 1981
C ......................................................................
C
      DIMENSION PO(1),CO(1),ISUBN(2)
C
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/FCONIT/IVALUE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc20.f,v $
     . $',                                                             '
     .$Id: prc20.f,v 1.1 1995/09/17 18:49:39 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA ISUBN/4HPRC2,4H0   /
C
C     UTILITY SUBROUTINE TO CHECK DEBUG AND TRACE LEVEL.
C
      CALL FPRBUG(ISUBN,1,20,IBUG)
C
C     CHECK IF CARRYOVER EXISTS FOR THIS OPERATION.
C
      NEEDC=PO(10)
      IF (NEEDC.EQ.0) GO TO 999
C
C     CHECK IF CO IS DEFAULT
C
      IF (IVALUE.NE.1) GO TO 200
      IRDCO=PO(11)
      IF (IRDCO.EQ.0) GO TO 200
C
      WRITE (IPR,100)
100   FORMAT(1H0,10X,39HCARRYOVER SET TO DEFAULT VALUE OF ZERO.)
      GO TO 999
200   ITA=PO(5)
C
      WRITE (IPR,300) (PO(I),I=2,4),ITA,(CO(I),I=1,NEEDC)
300   FORMAT(1H0,10X,48HNEXT IS THE PREVIOUS VALUE OF TIME SERIES (I.D.=
     $,2A4,3X,5HTYPE=,A4,3X,3HDT=,I2,1X,7HHOURS).
     $/ 11X,32HCHANGE TIME INTERVAL CARRYOVER =,2F10.2)
C
999   RETURN
      END
