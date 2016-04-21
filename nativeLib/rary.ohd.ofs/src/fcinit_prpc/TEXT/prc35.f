C MEMBER PRC35
C  (from old member FCPRC35)
C  VERSION 1
C
      SUBROUTINE PRC35(PO,CO)
C
C
C#######################################################################
C
C  THIS SUBROUTINE PRINTS CARRYOVER VALUES FROM THE C ARRAY FOR THE
C  API - HAR OPERATION.
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
C      3      TFI        12Z FI VALUE
C      4      SAPI       STORM API VALUE AT 12Z                 INCHES
C      5      SAEI       STORM AEI VALUE AT 12Z                 INCHES
C      6      SFI        STORM FI VALUE AT 12Z
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc35.f,v $
     . $',                                                             '
     .$Id: prc35.f,v 1.1 1995/09/17 18:49:44 dws Exp $
     . $' /
C    ===================================================================
C
      DATA SUBNAM /4hPRC3,4h5   /,NOP/35/
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
      NSPER=PO(22)
      ICOF=PO(25)
C
C  PRINT HEADING.
C
      WRITE(IPR,1000)BNAME
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
      TFI=CO(3)
      SAPI=CO(4)
      SAEI=CO(5)
      SFI=CO(6)
      SRAIM=CO(7)
      SRO=CO(8)
      DRAIM=CO(9)
      DRO=CO(10)
      DO 150 I=1,NSPER
150   RNSP(I)=CO(10+I)
      WRITE(IPR,1151)
      WRITE(IPR,1155)TAPI
      WRITE(IPR,1156)TAEI
      WRITE(IPR,1157)TFI
      WRITE(IPR,1158)SAPI
      WRITE(IPR,1159)SAEI
      WRITE(IPR,1160)SFI
      WRITE(IPR,1161)SRAIM
      WRITE(IPR,1162)SRO
      WRITE(IPR,1163)DRAIM
      WRITE(IPR,1164)DRO
      WRITE(IPR,1170)(RNSP(I),I=1,NSPER)
9999  RETURN
1000  FORMAT(//41X,'API - HAR CARRYOVER VALUES FOR ',5A4,/)
1120  FORMAT(//,33X,'DEFAULT VALUES WERE USED AS INITIAL ',
     1      'VALUES IN THE CARRYOVER ARRAY.')
1151  FORMAT(40X,'INTERNAL',/40X,'VARIABLE',/42X,'NAME',7X,
     1      'DESCRIPTION',20X,'CONTENTS',/)
1155  FORMAT(42X,'TAPI  ',4('.'),' 12Z API ',23('.'),1X,F5.2)
1156  FORMAT(42X,'TAEI  ',4('.'),' 12Z AEI ',23('.'),1X,F5.2)
1157  FORMAT(42X,'TFI   ',4('.'),' 12Z FI ',24('.'),1X,F5.2)
1158  FORMAT(42X,'SAPI  ',4('.'),' STORM API AT 12Z ',14('.'),1X,F5.2)
1159  FORMAT(42X,'SAEI  ',4('.'),' STORM AEI AT 12Z ',14('.'),1X,F5.2)
1160  FORMAT(42X,'SFI   ',4('.'),' STORM FI AT 12Z ',15('.'),1X,F5.2)
1161  FORMAT(42X,'SRAIM ',4('.'),' STORM RAIN/MELT AS OF 12Z ',
     1      5('.'),1X,F5.2)
1162  FORMAT(42X,'SRO   ',4('.'),' STORM RUNOFF AS OF 12Z ',
     1      8('.'),1X,F5.2)
1163  FORMAT(42X,'DRAIM ',4('.'),' 24-HOUR RAIN/MELT ENDING 12Z ',
     1      2('.'),1X,F5.2)
1164  FORMAT(42X,'DRO   ',4('.'),' 24-HOUR RUNOFF ENDING 12Z ',
     1      5('.'),1X,F5.2)
1170  FORMAT(//27X,'RAIN/MELT FOR EACH PERIOD WITHIN THE ',
     1      'NEW STORM WINDOW (OLDEST PERIOD IS FIRST):',
     2      24(/63X,F5.2))
      END
