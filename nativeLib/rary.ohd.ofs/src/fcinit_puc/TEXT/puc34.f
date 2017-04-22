C MEMBER PUC34
C  (from old member FCPUC34)
C  VERSION 1
C
C***********************************************************************
C
C THIS SUBROUTINE PUNCHES CARDS WHICH CAN BE READ IN THE PIN ROUTINE
C TO DEFINE OR REDEFINE A SEGMENT FOR THE API-SLC OPERATION.
C
C***********************************************************************
C
C SUBROUTINE INITIALLY WRITTEN BY
C     DON LAURINE - CBRFC     MARCH 1985
C
C***********************************************************************
C
C PRINCIPAL VARIABLES
C
C OF THE NWSRFS USER'S MANUAL.
C
C     CO            CARRYOVER ARRAY
C     IAPFLG        API TIME SERIES FLAG - 0=DOES NOT EXSIST
C     ICOFLG        CARRYOVER FLAG - 0=DEFAULT 1=USER SUPPLIED
C     IDELAP        DELTA-T FOR API TIME SERIES
C     IDELRF        DELTA-T FOR RAIN+MELT TIME SERIES
C     IDELRO        DELTA-T FOR RUNOFF TIME SERIES
C     IDELSC        DELTA-T FOR % SNOW COVER TIME SERIES
C     IDUR          STORM DURATION
C     IPDFLT        CARRYOVER FLAG - 0=USER SUPPLIED 1=DEFAULT
C     ISCFLG        % SNOW COVER TS FLAG - 0=DOES NOT EXSIST
C     ISFG          STORM FLAG - 0=NO STORM
C     IWN           API RELATIONSHIP CONSTANT (WN)
C     IWX           API RELATIONSHIP CONSTANT (WX)
C     PO            PARAMETER ARRAY
C     SUBNAM        SUBROUTINE NAME
C
C
C***********************************************************************
C
      SUBROUTINE PUC34(PO,CO)
C
      DIMENSION PO(1),CO(1),SUBNAM(2)
C
      COMMON /IONUM/ IN,IPR,IPU
      COMMON /FDBUG/ IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON /PUDFLT/ IPDFLT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc34.f,v $
     . $',                                                             '
     .$Id: puc34.f,v 1.1 1995/09/17 18:50:54 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SUBNAM/4hPUC3,4h4   /,NOP/34/,EMPTY/4h    /
C
C CALL DEBUG CHECK ROUTINE
C
      CALL FPRBUG(SUBNAM,1,NOP,IFDEB)
C
C PUNCH NAME AND OPTION CARD - 1
C
      ICOFLG=0
      IF(IPDFLT.LE.0) ICOFLG=1
C
      IAPFLG=0
      IF(PO(26).NE.EMPTY) IAPFLG=1
C
      ISCFLG=0
      IF(PO(22).NE.EMPTY) ISCFLG=1
C
      WRITE(IPU,900) (PO(I),I=2,9),PO(37),ICOFLG,IAPFLG,ISCFLG
C
C PUNCH TIME SERIES CARDS
C
      IDELRF=PO(11)
      IDELRO=PO(15)
      WRITE(IPU,902) IDELRF,(PO(I),I=12,14),IDELRO,(PO(I),I=16,18)
C
      IF(ISCFLG.NE.1) GO TO 100
      IDELSC=PO(19)
      WRITE(IPU,902) IDELSC,(PO(I),I=20,22)
C
100   IF(IAPFLG.NE.1) GO TO 102
      IDELAP=PO(23)
      WRITE(IPU,902) IDELAP,(PO(I),I=24,26)
C
C PUNCH API CONSTANTS
C
102   IWN=PO(29)
      IWX=PO(30)
      WRITE(IPU,904) PO(27),PO(28),IWN,IWX,(PO(I),I=31,36)
C
C CHECK TO SEE IF CARRYOVERS ARE DEFAULT - IF NO THEN PUNCH
C
      IF(ICOFLG.NE.1) GO TO 104
      ISFG=CO(3)
      IDUR=CO(4)
      WRITE(IPU,904) CO(1),CO(2),ISFG,IDUR
C
104   RETURN
C
C-----------------------------------------------------------------------
C I/O FORMATS
C
900   FORMAT(6A4,1X,3F5.2,3I5)
902   FORMAT(2(I5,5X,2A4,3X,A4))
904   FORMAT(2F5.2,2I5,4F5.2,F5.1,F5.2)
C
      END
