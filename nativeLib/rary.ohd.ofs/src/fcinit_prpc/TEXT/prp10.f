C MEMBER PRP10
C  (from old member FCPRP10)
C
      SUBROUTINE PRP10(PO)
C.......................................
C     THIS IS THE PRINT PARAMETER SUBROUTINE FOR THE ADD/SUBTRACT
C        TIME SERIES OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C        ERIC ANDERSON - HRL   NOV. 1979
C.......................................
      DIMENSION PO(1)
      DIMENSION ADDSUB(4,2)
C
C     COMMON BLOCKS.
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp10.f,v $
     . $',                                                             '
     .$Id: prp10.f,v 1.1 1995/09/17 18:49:57 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENT.
      DATA ADDSUB/4HSUBT,4HRACT,4HED F,4HROM ,4HADDE,4HD TO,
     14H    ,4H    /
C.......................................
C     TRACE LEVEL =1. - NO DEBUG OUTPUT.
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,16H** PRP10 ENTERED)
C.......................................
C     GET CONTROL VARIABLES.
      IADD=PO(2)
      NEG=PO(12)
      ITB=PO(6)
      ITA=PO(10)
C.......................................
C     PRINT INFORMATION ON TIME SERIES TO BE ADDED OR SUBTRACTED.
      WRITE(IPR,901) (PO(I),I=7,9),ITA,(ADDSUB(I,IADD+1),I=1,4),
     1(PO(I),I=3,5),ITB
  901 FORMAT(1H0,10X,18HTIME SERIES (I.D.=,2A4,3X,5HTYPE=,A4,3X,
     114HTIME INTERVAL=,I2,1X,9HHOURS) IS,1X,4A4,/16X,18HTIME SERIES (I.
     2D.=,2A4,3X,5HTYPE=,A4,3X,14HTIME INTERVAL=,I2,1X,7HHOURS).)
      IF (NEG.EQ.1) GO TO 100
      WRITE(IPR,902)
  902 FORMAT(1H0,15X,37HRESULT IS NOT ALLOWED TO BE NEGATIVE.)
      GO TO 101
  100 WRITE(IPR,903)
  903 FORMAT(1H0,15X,33HRESULT IS ALLOWED TO BE NEGATIVE.)
  101 CONTINUE
C.......................................
      RETURN
      END
