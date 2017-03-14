C MEMBER PRC41
C  (from old member FCPRC41)
C
      SUBROUTINE PRC41(PO,CO)
C
C
C#######################################################################
C
C  THIS SUBROUTINE PRINTS CARRYOVER VALUES FROM THE C ARRAY FOR THE
C  NEW EVENT-BASED API-HAR2 OPERATION.
C
C#######################################################################
C
C  LIST OF VARIABLES:
C
C     CO(1)   INPUT CARRYOVER DATA FROM C ARRAY.
C     ICOF    CARRYOVER INPUT READ FLAG.  0 = NO READ, 1 = READ.
C             (FROM PO ARRAY ELEMENT 25).
C     NSPER   NUMBER OF PERIODS IN THE NEW STORM WINDOW.
C             (FROM PO ARRAY ELEMENT 22).
C     PO(1)   INPUT PARAMETRIC DATA FROM P ARRAY.
C
C  CONTENTS OF THE CO ARRAY:
C
C
C     WORD    NAME       DESCRIPTION                            UNITS
C  ________   _______    _______________________________________________
C      1      TAPI       12Z API VALUE                          INCHES
C      2      TAEI       12Z AEI VALUE                          INCHES
C      3      TAI        12Z AI VALUE
C      4      SAPI       STORM API VALUE AT 12Z                 INCHES
C      5      SAEI       STORM AEI VALUE AT 12Z                 INCHES
C      6      SAI        STORM AI VALUE AT 12Z
C      7      SRAIM      STORM RAIN/MELT                        INCHES
C      8      SRO        STORM RUNOFF                           INCHES
C      9      DRAIM      24-HOUR RAIN/MELT                      INCHES
C     10      DRO        24-HOUR RUNOFF                         INCHES
C   11 - 34   RNSP       RAIN/MELT IN EACH PERIOD OF THE        INCHES
C                        NEW STORM WINDOW
C
C#######################################################################
C
      DIMENSION PO(1),CO(1),SUBNAM(2),RNSP(24),BNAME(5)
      COMMON /IONUM/ IN,IPR,IPU
      COMMON /FCONIT/ IVALUE
      COMMON /FDBUG/ IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc41.f,v $
     . $',                                                             '
     .$Id: prc41.f,v 1.1 1995/09/17 18:49:46 dws Exp $
     . $' /
C    ===================================================================
C
      DATA SUBNAM /4hPRC4,4h1   /,NOP/41/
C
C  CALL DEBUG CHECK ROUTINE.
C
      CALL FPRBUG(SUBNAM,1,NOP,IFDEB)
C
C  PULL IN NEEDED INFO FROM THE PO ARRAY.
C
      BNAME(1)=PO(4)
      BNAME(2)=PO(5)
      BNAME(3)=PO(6)
      BNAME(4)=PO(7)
      BNAME(5)=PO(8)
      NSPER=PO(28)
      ICOF=PO(31)
C
C  CHECK FOR INITIAL OR CURRENT CARRYOVER.  IF IVALUE = 0,
C  NOT INITIAL VALUES.  IF IVALUE = 1, INITIAL VALUES.
C
      IF(IVALUE)130,130,110
C
C  IF INITIAL CARRYOVER, CHECK FOR DEFAULT VALUES.
C
110   IF(ICOF)120,120,130
120   WRITE(IPR,1120)
      GO TO 9999
C
C  PRINT CARRYOVER VALUES.
C
130   TAPI=CO(1)
      TAEI=CO(2)
      TAI=CO(3)
      SAPI=CO(4)
      SAEI=CO(5)
      SAI=CO(6)
      SRAIM=CO(7)
      SRO=CO(8)
      DRAIM=CO(9)
      DRO=CO(10)
      DO 150 I=1,NSPER
150   RNSP(I)=CO(10+I)
      WRITE(IPR,1151)
      WRITE(IPR,1155)TAPI,TAEI,TAI,SAPI,SAEI,SAI
      WRITE(IPR,1160)SRAIM,SRO,DRAIM,DRO
      WRITE(IPR,1170)(RNSP(I),I=1,NSPER)
1120  FORMAT(//11X,'DEFAULT VALUES WERE USED AS INITIAL VALUES IN ',
     1      'THE CARRYOVER ARRAY.')
1151  FORMAT(/11X,'CARRYOVER VALUES FOR THIS OPERATION :',/)
1155  FORMAT(45X,'STORM   STORM   STORM',/12X,
     1      '12Z API    12Z AEI    12Z AI      API     AEI      AI',
     2      /13X,F5.2,6X,F5.2,6X,F5.2,5X,F5.2,3X,F5.2,3X,F5.2)
1160  FORMAT(/14X,'STORM      STORM       24-HR      24-HR',
     1      /12X,'RAIN/MELT    RUNOFF    RAIN/MELT    RUNOFF',
     2      /14X,F5.2,6X,F5.2,7X,F5.2,6X,F5.2)
1170  FORMAT(/12X,'RAIN/MELT FOR EACH PERIOD WITHIN THE ',
     1      'NEW STORM WINDOW (OLDEST PERIOD IS FIRST) :',
     2      /12X,12(F5.2,3X),/12X,12(F5.2,3X))
9999  RETURN
      END
