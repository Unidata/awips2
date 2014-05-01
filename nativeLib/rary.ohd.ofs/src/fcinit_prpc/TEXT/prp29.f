C MEMBER PRP29
C  (from old member FCPRP29)
C
      SUBROUTINE PRP29 (PO)
C                             LAST UPDATE: 02/08/94.09:16:36 BY $WC20SV
C
C
C***********************************************************************
C
C  THIS SUBROUTINE PRINTS PARAMETRIC DATA WHICH WAS STORED IN THE
C  PO ARRAY BY THE PIN ROUTINE AND PRINTS A SUMMARY OF THIS DATA.
C  THIS SUBROUTINE PRINTS PARAMETER VALUES FROM THE P ARRAY FOR THE
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
C     AIADJ          AI ADJUSTMENT FACTOR
C     IAIREL         API/AI RELATIONSHIP NUMBER
C     IDELT          DELTA-T OF TIME SERIES FOR RAINFALL/MELT AND RUNOFF
C     IFDEB          DEBUG OUTPUT SWITCH, 0 = OFF, 1 = ON
C     IFUTWK         FUTURE WEEK NUMBER
C     INCOFL         READ/NO READ CARRYOVER FLAG, 0 = NO READ, 1 = READ
C     NOP            NUMBER OF OPERATION ASSIGNED TO THIS OPERATION
C     NOZON          RUNOFF ZONE NUMBER
C     PO(1)          INPUT PARAMETRIC DATA FROM P ARRAY
C     SUBNAM(2)      SUBROUTINE NAME
C
C***********************************************************************
C
C
      DIMENSION PO(1),SUBNAM(2)
C
      COMMON /IONUM/ IN,IPR,IPU
      COMMON /FDBUG/ IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp29.f,v $
     . $',                                                             '
     .$Id: prp29.f,v 1.1 1995/09/17 18:50:10 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SUBNAM/4hPRP2,4h9   /,NOP/29/,EMPTY/4h    /
C
C
C  CALL DEBUG CHECK ROUTINE.
C
      CALL FPRBUG (SUBNAM,1,NOP,IFDEB)
C
C  PRINT PO ARRAY ELEMENTS.
C
      NOZON=PO(8)
      IAIREL=PO(10)
      IFUTWK=PO(11)
      AIADJ=PO(12)/10.0
      IDELT=PO(13)
      INCOFL=PO(31)
      WRITE(IPR,11) (PO(I),I=2,7),NOZON,PO(9),IAIREL,IFUTWK,
     1              AIADJ,INCOFL
      WRITE(IPR,12) (PO(I),I=14,16),IDELT,(PO(I),I=17,19),IDELT,
     1              (PO(I),I=20,22)
      IF(PO(34).EQ.EMPTY) GO TO 101
      WRITE(IPR,13) (PO(I),I=32,34)
  101 IF(PO(37).EQ.EMPTY) GO TO 102
      WRITE(IPR,14) (PO(I),I=35,37)
  102 WRITE(IPR,15) (PO(I),I=23,30)
      RETURN
C
C
   11 FORMAT(/12X,'ZONE NAME:',2X,6A4,
     1       3X,'ZONE NUMBER:',I8,5X,'RFC ID CODE:',4X,A4/
     2       51X,'API/AI REL NO:',I6,5X,'FUTURE WEEK NO:',I5/
     3       51X,'AI ADJ FACTOR:',F6.1,5X,'C/O INPUT FLAG:',I5)
   12 FORMAT(/20X,'TIME SERIES USED BY THIS OPERATION...'/
     1       25X,'CONTENTS',15X,'TS I.D.',5X,'TYPE',5X,'TIME INTERVAL'/
     2       25X,20('-'),2X,8('-'),5X,4('-'),5X,13('-')/
     3       25X,'RAINFALL/MELT',9X,2A4,5X,A4,I9,' HOURS'/
     4       25X,'RUNOFF',16X,2A4,5X,A4,I9,' HOURS'/
     5       25X,'WATER EQUIVALENT',6X,2A4,5X,A4,7X,'24 HOURS')
   13 FORMAT(25X,'CURRENT API',11X,2A4,5X,A4,7X,'24 HOURS')
   14 FORMAT(25X,'CURRENT AI',12X,2A4,5X,A4,7X,'24 HOURS')
   15 FORMAT(20X,'API/AI CONSTANTS AND RECESSION FACTORS...'/
     1       29X,'C1',8X,'C2',8X,'C3',8X,'C4',8X,'C5',8X,'C6',
     2       6X,'REG1',6X,'REG2'/
     3       21X,6F10.3,2F10.2)
      END
