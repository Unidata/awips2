C MEMBER PRP34
C  (from old member FCPRP34)
C  VERSION 1
C
C***********************************************************************
C
C THIS SUBROUTINE PRINTS PARAMETRIC DATA WHICH WAS STORED IN THE
C PO ARRAY BY THE PIN ROUTINE AND PRINTS A SUMMARY OF THIS DATA.
C THIS SUBROUTINE PRINTS PARAMETRIC VALUES FROM THE P ARRAY FOR THE
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
C     ICOFLG            CARRYOVER FLAG - 0=DEFAULT 1=USER SUPPLIED
C     IDELAP            DELTA-T FOR API TIME SERIES (24 HOURS)
C     IDELRF            DELTA-T FOR RAIN+MELT TIME SERIES (6 HOURS)
C     IDELRO            DELTA-T FOR RUNOFF TIME SERIES (6 HOURS)
C     IDELSC            DELTA-T FOR %SNOW COVER TIME SERIES (24 HOURS)
C     IVERS             API-SLC OPERATION VERSION NUMBER
C     IWN               API RELATIONSHIP CONSTANT (WN)
C     IWX               API RELATIONSHIP CONSTANT (WX)
C     PO                INPUT PARAMETRIC DATA FOR THE P ARRAY
C     SUBNAM            SUBROUTINE NAME
C
C***********************************************************************
C
      SUBROUTINE PRP34(PO)
C
      DIMENSION PO(1),SUBNAM(2)
C
      COMMON /IONUM/ IN,IPR,IPU
      COMMON /FDBUG/ IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp34.f,v $
     . $',                                                             '
     .$Id: prp34.f,v 1.1 1995/09/17 18:50:13 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SUBNAM/4hPRP3,4h4   /,NOP/34/,EMPTY/4h    /
C
C CALL DEBUG CHECK ROUTINE
C
      CALL FPRBUG(SUBNAM,1,NOP,IFDEB)
C
C PRINT PO ARRAY ELEMENTS
C
      IWN=PO(29)
      IWX=PO(30)
      IVERS=PO(1)
      IDELRF=PO(11)
      IDELRO=PO(15)
      IDELSC=PO(19)
      IDELAP=PO(23)
      ICOFLG=PO(10)
C
      WRITE(IPR,900) (PO(I),I=2,7),IVERS,PO(9),ICOFLG,PO(8),PO(37)
C
C WRITE RAIN+MELT AND RUNOFF TS INFORMATION
C
      WRITE(IPR,901) (PO(I),I=12,14),IDELRF,(PO(I),I=16,18),IDELRO
C
C IF % SNOW COVER TS REQUIRED WRITE INFORMATION
C
      IF(PO(22)-EMPTY) 100,101,100
100   WRITE(IPR,902) (PO(I),I=20,22),IDELSC
C
C IF API TS REQUIRED WRITE INFORMATION
C
101   IF(PO(26)-EMPTY) 102,103,102
102   WRITE(IPR,903) (PO(I),I=24,26),IDELAP
C
C WRITE API CONSTANTS
C
103   WRITE(IPR,904) PO(27),PO(28),IWN,IWX,(PO(I),I=31,36)
C
      RETURN
C
C-----------------------------------------------------------------------
C I/O FORMATS
C
900   FORMAT(/10X,'AREA NAME: ',6A4,7X,'OPERATION VERSION:',I3,
     *5X,'API LOWER LIMIT:',F7.2,/52X,'C/O INPUT FLAG:',I6,5X,
     *'API REC CONSTANT:',F6.2,/78X,'SIG. PRECIP VALUE:',F5.2)
901   FORMAT(/35X,'TIME SERIES USED FOR THIS OPERATION',
     *//25X,'CONTENTS',14X,'TS I.D.',5X,'TYPE',5X,'TIME INTERVAL',
     */25X,15('-'),7X,8('-'),4X,'----',5X,13('-'),
     */25X,'RAINFALL/MELT',9X,2A4,4X,A4,5X,I6,' HOURS',
     */25X,'RUNOFF',16X,2A4,4X,A4,5X,I6,' HOURS')
902   FORMAT(25X,'% SNOW COVER',10X,2A4,4X,A4,5X,I6,' HOURS')
903   FORMAT(25X,'API',19X,2A4,4X,A4,5X,I6,' HOURS')
904   FORMAT(/15X,'API CONSTANTS:',
     *//17X,'A',7X,'I',7X,'WN',6X,'WX',6X,'E1',6X,'E2',6X,'CP',6X,'K',
     *7X,'M',6X,'POW',/12X,10(3X,'-----')
     */15X,F5.2,3X,F5.2,4X,I3,5X,I3,1X,4(3X,F5.2),3X,F5.1,3X,F5.2)
C
      END
