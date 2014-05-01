C MEMBER PRC29
C  (from old member FCPRC29)
C
      SUBROUTINE PRC29 (PO,CO)
C                             LAST UPDATE: 02/08/94.09:16:26 BY $WC20SV
C
C
C***********************************************************************
C
C  THIS SUBROUTINE PRINTS CARRYOVER VALUES FROM THE C ARRAY FOR THE
C  API-MKC OPERATION.
C
C***********************************************************************
C  SUBROUTINE INITIALLY WRITTEN BY
C        LARRY BLACK - MBRFC     JULY 1982
C***********************************************************************
C  PRINCIPAL VARIABLES...
C
C  FOR DEFINITION OF VARIABLES IN COMMON BLOCKS, SEE SECTION IX.3.3C
C  OF THE NWSRFS USER'S MANUAL.
C
C     AI             CURRENT AI VALUE
C     AICO           STORM AI VALUE
C     API            CURRENT API VALUE
C     CO(1)          INPUT CARRYOVER DATA FROM C ARRAY
C     IFDEB          DEBUG OUTPUT SWITCH, 0 = OFF, 1 = ON
C     INCOFL         READ/NO READ CARRYOVER FLAG, 0 = NO READ, 1 = READ
C     NEWSTM         STORM PERIOD COUNTER
C     NOP            NUMBER OF OPERATION ASSIGNED TO THIS OPERATION
C     PO(1)          INPUT PARAMETRIC DATA FROM P ARRAY
C     RAINCO         STORM RAINFALL VALUE
C     ROCO           STORM RUNOFF VALUE
C     SUBNAM(2)      SUBROUTINE NAME
C     TOT24          24-HOUR RAINFALL/MELT ENDING 12Z
C     WE             CURRENT WATER EQUIVALENT VALUE
C
C***********************************************************************
C
C
      DIMENSION PO(1),CO(1),SUBNAM(2)
C
      COMMON /IONUM/ IN,IPR,IPU
      COMMON /FDBUG/ IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON /FCONIT/ IVALUE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc29.f,v $
     . $',                                                             '
     .$Id: prc29.f,v 1.1 1995/09/17 18:49:42 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SUBNAM/4hPRC2,4h9   /,NOP/29/
C
C
C  CALL DEBUG CHECK ROUTINE.
C
      CALL FPRBUG (SUBNAM,1,NOP,IFDEB)
C
C  CHECK FOR INITIAL OR CURRENT CARRYOVER.
C
      IF(IVALUE.LE.0) GO TO 101
C
C  IF INITIAL CARRYOVER, CHECK FOR DEFAULT VALUES.
C
      INCOFL=PO(31)
      IF(INCOFL.GT.0) GO TO 101
      WRITE(IPR,11)
      RETURN
C
C  PRINT CURRENT CARRYOVER VALUES.
C
  101 NEWSTM=CO(1)
      RAINCO=CO(2)/100.0
      AICO=CO(3)/10.0
      ROCO=CO(4)/100.0
      API=CO(5)/100.0
      AI=CO(6)/10.0
      WE=CO(7)/100.0
      TOT24=CO(8)/100.0
      WRITE(IPR,12) NEWSTM,RAINCO,AICO,ROCO,API,AI,WE,TOT24
      RETURN
C
C
   11 FORMAT(20X,'CARRYOVER VALUES...'/
     1       25X,'DEFAULT VALUES WERE USED.')
   12 FORMAT(20X,'CARRYOVER VALUES...'/
     1       25X,'NEWSTM    RAINCO      AICO      ROCO       ',
     2       'API        AI        WE     TOT24'/
     3       27X,I4,F10.2,F10.1,2F10.2,F10.1,2F10.2)
      END
