C MEMBER PRP37
C  (from old member FCPRP37)
C
       SUBROUTINE PRP37(PO)
C      TEST PRINT ROUTINE FOR LIST-MSP
C      WRITTEN BY ERIC ANDERSON-HRL DEC 1987
       DIMENSION PO(1),SWOP(4)
       COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
       COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp37.f,v $
     . $',                                                             '
     .$Id: prp37.f,v 1.1 1995/09/17 18:50:16 dws Exp $
     . $' /
C    ===================================================================
C
       DATA FNONE/4HNONE/
       IF(ITRACE.GE.1) WRITE(IODBUG,900)
900    FORMAT(1H0,16H** PRP37 ENTERED)
       WRITE(IPR,901) (PO(I),I=2,7)
901    FORMAT(1H0,10X,26HRUNOFF/SNOW TABULATION FOR,1X,6A4)
       WRITE(IPR,902) (PO(I),I=8,11)
902    FORMAT(1H ,20X,13HAPI OPERATION,5X,2A4,3X,2A4)
       DO 100 I=1,4
100    SWOP(I)= PO(I+12)
       ISNW=PO(12)
       IF(ISNW.EQ.1) GO TO 105
       SWOP(1) = FNONE
105    WRITE(IPR,903) SWOP
903    FORMAT(1H ,20X,14HSNOW OPERATION,4X,2A4,3X,2A4)
       IF(ITRACE.GE.1) WRITE(IODBUG,904)
904    FORMAT(1H0,13H** EXIT PRP37)
       RETURN
       END
