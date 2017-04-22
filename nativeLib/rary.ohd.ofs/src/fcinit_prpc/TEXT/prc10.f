C MEMBER PRC10
C  (from old member FCPRC10)
C
      SUBROUTINE PRC10(PO,CO)
C.......................................
C     THIS IS THE PRINT CARRYOVER SUBROUTINE FOR THE ADD/SUBTRACT
C        OPERATION.  THIS OPERATION ONLY HAS CARRYOVER WHEN
C        THE TIME INTERVAL OF THE TIME SERIES TO BE ADDED
C        OR SUBTRACTED IS LESS THAN THE TIME INTERVAL OF THE
C        TIME SERIES IT IS ADDED TO OR SUBTRACTED FROM.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C        ERIC ANDERSON - HRL   NOV. 1979
C.......................................
      DIMENSION PO(1),CO(1)
C
C     COMMON BLOCKS.
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/FCONIT/IVALUE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc10.f,v $
     . $',                                                             '
     .$Id: prc10.f,v 1.1 1995/09/17 18:49:35 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL=1, - NO DEBUG OUTPUT.
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,16H** PRC10 ENTERED)
C.......................................
C     CHECK IF CARRYOVER EXISTS FOR THIS OPERATION.
      ICO=PO(11)
      IF(ICO.EQ.0) RETURN
C.......................................
C     PRINT CARRYOVER VALUE.
      IF(IVALUE.NE.1) GO TO 100
      ICO=PO(13)
      IF(ICO.EQ.1) GO TO 100
      WRITE(IPR,901)
  901 FORMAT(1H0,10X,39HCARRYOVER SET TO DEFAULT VALUE OF ZERO.)
      GO TO 101
  100 ITA=PO(10)
      WRITE(IPR,902) CO(1),(PO(I),I=7,9),ITA
  902 FORMAT(1H0,10X,23HADD/SUBTRACT CARRYOVER=,F9.3,/16X,
     148HTHIS IS THE PREVIOUS VALUE OF TIME SERIES (I.D.=,2A4,3X,
     25HTYPE=,A4,3X,3HDT=,I2,1X,7HHOURS).)
  101 CONTINUE
C.......................................
      RETURN
      END
