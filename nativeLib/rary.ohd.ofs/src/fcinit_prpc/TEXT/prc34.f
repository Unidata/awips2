C MEMBER PRC34
C  (from old member FCPRC34)
C  VERSION 1
C
C***********************************************************************
C
C THIS SUBROUTINE PRINTS CARRYOVER VALUES FROM THE C ARRAY FOR THE
C API-SLC OPERATION.
C
C***********************************************************************
C
C SUBROUTINE INITIALLY WRITTEN BY
C        DON LAURINE - CBRFC   MARCH 1985
C
C***********************************************************************
C
C PRINCIPAL VARIABLES
C
C FOR DEFINITION OF VARIABLES IN COMMON BLOCKS, SEE SECTION IX.3.3C
C OF THE NWSRFS USER'S MANUAL.
C
C     CO                CARRYOVER ARRAY
C     ICOFLG            CARRYOVER FLAG - 0=DEFAULT 1=USER SUPPLIED
C     IDUR              CARRYOVER - STORM DURATION
C     ISFG              CARRYOVER - STORM FLAG
C     PO                INPUT PARAMETRIC DATA FOR THE P ARRAY
C     PS                CARRYOVER - CURRENT % SNOW COVER
C     SUBNAM            SUBROUTINE NAME
C
C***********************************************************************
C
      SUBROUTINE PRC34(PO,CO)
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc34.f,v $
     . $',                                                             '
     .$Id: prc34.f,v 1.1 1995/09/17 18:49:43 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SUBNAM/4hPRC3,4h4   /,NOP/34/
C
C CALL DEBUG CHECK ROUTINE
C
      CALL FPRBUG(SUBNAM,1,NOP,IFDEB)
C
C WRITE HEADING
C
      WRITE(IPR,900)
C
C CHECK FOR INITIAL OR CURRENT CARRYOVER
C
      IF(IVALUE) 102,102,100
C
C IF INITIAL CARRYOVER, CHECK FOR DEFAULT VALUES
C
100   ICOFLG=PO(10)
      IF(ICOFLG) 101,101,102
101   WRITE(IPR,901)
C
C WRITE CARRYOVER VALUES
C
102   ISFG=CO(3)
      IDUR=CO(4)*6
      PS=CO(2)*100.
      WRITE(IPR,902) CO(1),PS,ISFG,IDUR
C
      RETURN
C
C-----------------------------------------------------------------------
C I/O FORMATS
C
900   FORMAT(/15X,'CARRYOVER VALUES:')
901   FORMAT(15X,'NOTE: DEFAULT VALUES WERE USED')
902   FORMAT(/16X,'API',7X,'%SC',6X,'STORM',6X,'DUR HRS',
     */15X,3('-----',5X),'--------',/14X,F6.2,5X,F5.1,6X,I2,9X,I3)
C
      END
