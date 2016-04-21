C MEMBER PUC37
C  (from old member FCPUC37)
C
       SUBROUTINE PUC37(PO)
C      TEST PUNCH ROUTINE FOR LIST-MSP
C      WRITTEN BY ERIC ANDERSON-HRL DEC 1987
       DIMENSION PO(1),SWOP(4)
       COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
       COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc37.f,v $
     . $',                                                             '
     .$Id: puc37.f,v 1.1 1995/09/17 18:50:56 dws Exp $
     . $' /
C    ===================================================================
C
       DATA FNONE/4HNONE/
       IF(ITRACE.GE.1) WRITE(IODBUG,900)
900    FORMAT (1H0,16H** PUC37 ENTERED)
       DO 100 I=1,4
100    SWOP(I)=PO(I+12)
       ISNW=PO(12)
       IF(ISNW.EQ.1) GO TO 105
       SWOP(1)=FNONE
105    WRITE(IPU,901) (PO(I),I=8,11),SWOP
901    FORMAT(2A4,2X,2A4,2X,2A4,2X,2A4)
       IF(ITRACE.GE.1) WRITE(IODBUG,902)
902    FORMAT(1H0,13H** EXIT PUC37)
       RETURN
       END
