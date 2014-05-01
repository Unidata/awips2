C MEMBER PRC38
C  (from old member FCPRC38)
C  VERSION 1
C
      SUBROUTINE PRC38(PO,CO)
C
C#######################################################################
C
C  THIS SUBROUTINE PRINTS CARRYOVER VALUES FROM THE CO ARRAY FOR THE
C  BASEFLOW SIMULATION OPERATION.
C
C#######################################################################
C
C  CONTENTS OF THE CO ARRAY:
C
C    NOTE:  CO ARRAY IS NOT NEEDED IF PO(9) = ZERO.
C
C     WORD    NAME      DESCRIPTION                             UNITS
C   ________  ________  ________________________________________________
C      1      TBFLOW    PREVIOUS TOTAL BASEFLOW                 CMS
C                       (USED IF PO(9) = 1 OR 2)
C      2      BFREC     PREVIOUS BASEFLOW RECESSION COEFF
C                       (USED ONLY IF PO(9) = 2)
C
C#######################################################################
C
C   COMMON:
C
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FENGMT/METRIC
C
C#######################################################################
C
C  DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION PO(1),CO(1),SUBNAM(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc38.f,v $
     . $',                                                             '
     .$Id: prc38.f,v 1.1 1995/09/17 18:49:46 dws Exp $
     . $' /
C    ===================================================================
C
C
C#######################################################################
C
C  DATA:
C
      DATA SUBNAM/4hPRC3,4h8   /,NOP/38/
C
C#######################################################################
C
C  CALL DEBUG CHECK ROUTINE
C
      CALL FPRBUG(SUBNAM,1,NOP,IFDEB)
C
C  PULL IN NEEDED INFORMATION FROM THE PO ARRAY
C
      IVBF=PO(9)
      METENG=PO(2)
C
C  PRINT OUT CARRYOVER DATA (IF ANY)
C
      IF(IVBF)100,100,200
100   WRITE(IPR,1000)
      GO TO 10000
200   WRITE(IPR,1200)
      TBFLOW=CO(1)
      IF(METRIC)210,220,230
210   IF(METENG)220,220,230
220   TBFLOW=TBFLOW/.028317
      WRITE(IPR,1500)TBFLOW
      IF(IVBF-1)10000,10000,300
230   WRITE(IPR,1600)TBFLOW
      IF(IVBF-1)10000,10000,300
300   BFREC=CO(2)
      WRITE(IPR,1700)BFREC
10000 RETURN
C
C#######################################################################
C
C  FORMAT STATEMENTS
C
C#######################################################################
C
1000  FORMAT(////10X,'NO CARRYOVER VALUES ARE STORED FOR THE ',
     1       'BASEFLOW SIMULATION OPERATION.')
1200  FORMAT(////19X,'CARRYOVER DATA FOR THE BASEFLOW SIMULATION ',
     1       'OPERATION:',/)
1500  FORMAT(26X,'PREVIOUS TOTAL BASEFLOW = ',F9.1,' CFS')
1600  FORMAT(26X,'PREVIOUS TOTAL BASEFLOW = ',F9.1,' CMS')
1700  FORMAT(26X,'BASEFLOW RECESSION COEFFICIENT = ',F5.3)
      END
