C MEMBER PRC33
C  (from old member FCPRC33)
C
      SUBROUTINE PRC33 (PO,CO)
C
C***********************************************************************
C
C  THIS SUBROUTINE PRINTS CARRYOVER VALUES FROM THE C ARRAY FOR THE
C  API-CIN OPERATION.
C
C***********************************************************************
C  SUBROUTINE INITIALLY WRITTEN BY
C        TIM SWEENEY  -  OHRFC       NOVEMBER 1984
C***********************************************************************
C  PRINCIPAL VARIABLES...
C
C  FOR DEFINITION OF VARIABLES IN COMMON BLOCKS, SEE SECTION IX.3.3C
C  OF THE NWSRFS USER'S MANUAL.
C
C     AI             CURRENT AI VALUE
C     AICO           STORM AI VALUE
C     API            CURRENT API VALUE
C     AT             CURRENT UNCORRECTED SYNTHETIC TEMPERATURE
C     AVGT           CURRENT AVERAGE TEMPERATURE
C     CO(1)          INPUT CARRYOVER DATA FROM C ARRAY
C     IFDEB          DEBUG OUTPUT SWITCH, 0 = OFF, 1 = ON
C     INCOFL         READ/NO READ CARRYOVER FLAG, 0 = NO READ, 1 = READ
C     NEWSTM         STORM PERIOD COUNTER
C     NOP            NUMBER OF OPERATION ASSIGNED TO THIS OPERATION
C     PO(1)          INPUT PARAMETRIC DATA FROM P ARRAY
C     RAINCO         STORM RAINFALL VALUE
C     ROCO           STORM RUNOFF VALUE
C     SUBNAM(2)      SUBROUTINE NAME
C     TC             CURRENT CORRECTED SYNTHETIC TEMPERATURE
C     TOT24          24-HOUR RAINFALL/MELT ENDING 12Z
C     WE             CURRENT WATER EQUIVALENT VALUE
C
C***********************************************************************
C
C
      DIMENSION PO(1),CO(1),SUBNAM(2)
C
      COMMON/IONUM/ IN,IPR,IPU
      COMMON/FDBUG/ IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/FCONIT/ IVALUE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc33.f,v $
     . $',                                                             '
     .$Id: prc33.f,v 1.1 1995/09/17 18:49:43 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SUBNAM/4hPRC3,4h3   /,NOP/33/
C
C
C  CALL DEBUG CHECK ROUTINE.
C
      CALL FPRBUG (SUBNAM,1,NOP,IFDEB)
C
C  PRINT HEADING.
C
      WRITE(IPR,11)
C
C  CHECK FOR INITIAL OR CURRENT CARRYOVER.
C
      IF(IVALUE) 103,103,101
C
C  IF INITIAL CARRYOVER, CHECK FOR DEFAULT VALUES.
C
  101 INCOFL=PO(31)
      IF(INCOFL) 102,102,103
  102 WRITE(IPR,12)
      GO TO 9999
C
C  PRINT CURRENT CARRYOVER VALUES.
C
  103 NEWSTM=CO(1)
      RAINCO=CO(2)
      AICO=CO(3)
      ROCO=CO(4)
      API=CO(5)
      AI=CO(6)
      WE=CO(7)
      TOT24=CO(8)
      AVGT=CO(9)
      AT=CO(10)
      TC=CO(11)
      WRITE(IPR,13) NEWSTM,RAINCO,AICO,ROCO,API,AI,WE,TOT24,AVGT,AT,TC
C
C
 9999 RETURN
C
C
   11 FORMAT(/20X,'CARRYOVER VALUES...')
   12 FORMAT(/25X,'DEFAULT VALUES WERE USED.')
   13 FORMAT(/25X,'NEWSTM   RAINCO     AICO     ROCO      API       ',
     1       'AI       WE    TOT24     AVGT       AT       TC'/
     2       27X,I4,3X,F6.2,4X,F5.1,3X,F6.2,4X,F5.2,4X,F5.1,2(3X,F6.2),
     3       3(4X,F5.1))
      END
