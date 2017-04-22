C     MODULE: FCPRC43     VERSION 1      
c
c  =====================================================================
c  pgm:  prc43 (po,co)
c
c   in: po     .... parameter array
c   in: co     .... carryover array
c  =====================================================================
      SUBROUTINE PRC43(PO,CO)
c
C#######################################################################
C
C  THIS ROUTINE PRINTS CARRYOVER VALUES FROM THE C ARRAY FOR THE
C  API-HFD OPERATION.
C
C#######################################################################
c  Initially written by
c     Ken Mack    NERFC                          August 1995
c     Tim Sweeney  HRL                           October 1995
c#######################################################################
c
C  LIST OF VARIABLES:

C     CO(1)   INPUT CARRYOVER DATA FROM C ARRAY.
C     ICOF    CARRYOVER INPUT READ FLAG.  0 = NO READ, 1 = READ.
C             (FROM PO ARRAY ELEMENT 25).
C     NSPER   NUMBER OF PERIODS IN THE NEW STORM WINDOW.
C             (FROM PO ARRAY ELEMENT 22).
C     PO(1)   INPUT PARAMETRIC DATA FROM P ARRAY.

C  CONTENTS OF THE CO ARRAY:

C     WORD    NAME       DESCRIPTION                            UNITS
C  ________   _______    _______________________________________________
C      1      TAPI       12Z API VALUE                          INCHES
C      2      TATI       12Z ATI VALUE                          INCHES
C      3      TRI        12Z RI VALUE
C      4      SAPI       STORM API VALUE AT 12Z                 INCHES
C      5      SATI       STORM ATI VALUE AT 12Z                 INCHES
C      6      SRI        STORM RI VALUE AT 12Z
C      7      SRAIM      STORM RAIN/MELT                        INCHES
C      8      SRO        STORM RUNOFF                           INCHES
C      9      DRAIM      24-HOUR RAIN/MELT                      INCHES
C     10      DRO        24-HOUR RUNOFF                         INCHES
C   11 - 34   RNSP       RAIN/MELT IN EACH PERIOD OF THE        INCHES
C                        NEW STORM WINDOW

C#######################################################################
C
      include 'common/ionum'
      include 'common/fdbug'
      include 'common/fconit'
c
c      COMMON /IONUM/ IN,IPR,IPU
c      COMMON /FCONIT/ IVALUE
c      COMMON /FDBUG/ IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
c
      DIMENSION PO(*),CO(*),SUBNAM(2),RNSP(24),BNAME(5)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc43.f,v $
     . $',                                                             '
     .$Id: prc43.f,v 1.1 1996/03/21 16:00:57 page Exp $
     . $' /
C    ===================================================================
C
c
      DATA SUBNAM /4HPRC4,4H3   /,NOP/43/

C  CALL DEBUG CHECK ROUTINE.

      CALL FPRBUG(SUBNAM,1,NOP,IFDEB)

C  PULL IN NEEDED INFO FROM THE PO ARRAY.

      BNAME(1) = PO(4)
      BNAME(2) = PO(5)
      BNAME(3) = PO(6)
      BNAME(4) = PO(7)
      BNAME(5) = PO(8)
      NSPER    = PO(26)
      ICOF     = PO(29)

C  PRINT HEADING.

      WRITE(IPR,1000) BNAME

C  CHECK FOR INITIAL OR CURRENT CARRYOVER.  IF IVALUE = 0,
C  NOT INITIAL VALUES.  IF IVALUE = 1, INITIAL VALUES.

      IF(IVALUE.LE.0) GOTO 130

C  IF INITIAL CARRYOVER, CHECK FOR DEFAULT VALUES.

      IF(ICOF.LE.0) THEN
        WRITE(IPR,1120)
        GOTO 9999
      ENDIF

C  PRINT CARRYOVER VALUES.

130   TAPI  = CO(1)
      TATI  = CO(2)
      TRI   = CO(3)
      SAPI  = CO(4)
      SATI  = CO(5)
      SRI   = CO(6)
      SRAIM = CO(7)
      SRO   = CO(8)
      DRAIM = CO(9)
      DRO   = CO(10)
C
      DO 150 I=1,NSPER
150   RNSP(I)=CO(10+I)
      WRITE(IPR,1151)
      WRITE(IPR,1155)TAPI
      WRITE(IPR,1156)TATI
      WRITE(IPR,1157)TRI
      WRITE(IPR,1158)SAPI
      WRITE(IPR,1159)SATI
      WRITE(IPR,1160)SRI
      WRITE(IPR,1161)SRAIM
      WRITE(IPR,1162)SRO
      WRITE(IPR,1163)DRAIM
      WRITE(IPR,1164)DRO
      WRITE(IPR,1170)(RNSP(I),I=1,NSPER)
C
9999  RETURN
c
1000  FORMAT(//15X,'API-HFD CARRYOVER VALUES FOR ',5A4,/)
1120  FORMAT(//,7X,'DEFAULT VALUES WERE USED AS INITIAL ',
     1      'VALUES IN THE CARRYOVER ARRAY.')
1151  FORMAT(14X,'INTERNAL',/,14X,'VARIABLE',/,16X,'NAME',7X,
     1      'DESCRIPTION',20X,'CONTENTS',/)
1155  FORMAT(16X,'TAPI  ',4('.'),' 12Z API ',23('.'),1X,F5.2)
1156  FORMAT(16X,'TATI  ',4('.'),' 12Z ATI ',23('.'),1X,F4.1)
1157  FORMAT(16X,'TRI   ',4('.'),' 12Z RI ',24('.'),1X,F5.2)
1158  FORMAT(16X,'SAPI  ',4('.'),' STORM API AT 12Z ',14('.'),1X,F5.2)
1159  FORMAT(16X,'SATI  ',4('.'),' STORM ATI AT 12Z ',14('.'),1X,F4.1)
1160  FORMAT(16X,'SRI   ',4('.'),' STORM RI AT 12Z ',15('.'),1X,F5.2)
1161  FORMAT(16X,'SRAIM ',4('.'),' STORM RAIN/MELT AS OF 12Z ',
     1      5('.'),1X,F5.2)
1162  FORMAT(16X,'SRO   ',4('.'),' STORM RUNOFF AS OF 12Z ',
     1      8('.'),1X,F5.2)
1163  FORMAT(16X,'DRAIM ',4('.'),' 24-HOUR RAIN/MELT ENDING 12Z ',
     1      2('.'),1X,F5.2)
1164  FORMAT(16X,'DRO   ',4('.'),' 24-HOUR RUNOFF ENDING 12Z ',
     1      5('.'),1X,F5.2)
1170  FORMAT(//,' RAIN/MELT FOR EACH PERIOD WITHIN THE ',
     1      'NEW STORM WINDOW (OLDEST PERIOD IS FIRST):',
     2      24(/36X,F5.2))
      END
