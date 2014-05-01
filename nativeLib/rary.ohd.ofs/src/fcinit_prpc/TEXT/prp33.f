C MEMBER PRP33
C  (from old member FCPRP33)
C  VERSION 1
C
      SUBROUTINE PRP33 (PO)
C
C***********************************************************************
C
C  THIS SUBROUTINE PRINTS PARAMETRIC DATA WHICH WAS STORED IN THE
C  PO ARRAY BY THE PIN ROUTINE AND PRINTS A SUMMARY OF THIS DATA.
C  THIS SUBROUTINE PRINTS PARAMETER VALUES FROM THE P ARRAY FOR THE
C  API-CIN OPERATION.
C***********************************************************************
C  SUBROUTINE INITIALLY WRITTEN BY
C         TIM SWEENEY  -  OHRFC       NOVEMBER 1984
C
C  Corrected logic to print optional output time series id & code
C         Tim Sweeney, HRL                                April 1996
C***********************************************************************
C  PRINCIPAL VARIABLES...
C
C  FOR DEFINITION OF VARIABLES IN COMMON BLOCKS, SEE SECTION IX.3.3C
C  OF THE NWSRFS USER'S MANUAL.
C
C     AIADJ          AI ADJUSTMENT FACTOR
C     ARECF          API RECESSION FACTOR--NO SNOW
C     ASRF           API RECESSION FACTOR--SNOW
C     IDELT          DELTA-T OF TIME SERIES FOR RAINFALL/MELT AND RUNOFF
C     IFDEB          DEBUG OUTPUT SWITCH, 0 = OFF, 1 = ON
C     INCOFL         READ/NO READ CARRYOVER FLAG, 0 = NO READ, 1 = READ
C     LAT            LATITUDE OF RUNOFF ZONE
C     LON            LONGITUDE OF RUNOFF ZONE
C     NALZN          ALTERNATE RUNOFF ZONE NUMBER
C     NOP            NUMBER OF OPERATION ASSIGNED TO THIS OPERATION
C     NOZON          RUNOFF ZONE NUMBER
C     PO(1)          INPUT PARAMETRIC DATA FROM P ARRAY
C     PRECF          PRECIPITATION RECESSION FACTOR FOR API
C     SUBNAM(2)      SUBROUTINE NAME
C     TCADJ          SYNTHETIC TEMPERATURE ADJUSTMENT
C     WERCR          CRITERIA WE FOR API RECESSION FACTOR WITH SNOW
C
C***********************************************************************
C
C
      DIMENSION PO(1),SUBNAM(2)
C
      COMMON/IONUM/ IN,IPR,IPU
      COMMON/FDBUG/ IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp33.f,v $
     . $',                                                             '
     .$Id: prp33.f,v 1.2 1996/05/07 11:26:25 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA SUBNAM/4hPRP3,4h3   /,NOP/33/,EMPTY/4h    /
C
C
C  CALL DEBUG CHECK ROUTINE.
C
      CALL FPRBUG (SUBNAM,1,NOP,IFDEB)
C
C  PRINT PO ARRAY ELEMENTS.
C
      NOZON=PO(8)
      NALZN=PO(9)
      LAT=PO(10)
      LON=PO(11)
      AIADJ=PO(12)
      IDELT=PO(13)
      TCADJ=PO(26)
      PRECF=PO(27)
      ARECF=PO(28)
      ASRF=PO(29)
      WERCR=PO(30)
      INCOFL=PO(31)
C
      if(po(34).eq.empty) then
        ioutfl = 0
      else
        ioutfl = 1
      endif
C
      WRITE(IPR,11) (PO(I),I=2,7),NOZON,NALZN,LAT,LON,AIADJ,
     1              TCADJ,PRECF,ARECF,ASRF,WERCR,IOUTFL,INCOFL
      WRITE(IPR,12) (PO(I),I=14,16),IDELT,(PO(I),I=17,19),IDELT,
     1              (PO(I),I=20,22),(PO(I),I=23,25),IDELT
c
      IF(ioutfl.gt.0) then
        WRITE(IPR,13) (PO(I),I=32,34)
        WRITE(IPR,14) (PO(I),I=35,37)
      endif
C
  104 RETURN
C
C
   11 FORMAT(/12X,'ZONE NAME:',2X,6A4,5X,'ZONE NUMBER:',I9,
     1       5X,'ALT ZONE NUMBER:',I5/
     2       53X,'LATITUDE:',8X,I4,5X,'LONGITUDE:',6X,I5/
     3       53X,'AI ADJ FACTOR:',F7.1,5X,'TC ADJ FACTOR:',F7.1/
     4       53X,'PRECIP REC FAC:',F6.2,5X,'AP REC FAC:',5X,F5.2/
     5       53X,'AP SNOW REC FAC:',F5.2,5X,'WE REC CRITERIA:',F5.2/
     6       53X,'OUTPUT TS FLAG:',I6,5X,'C/O INPUT FLAG:',I6)
   12 FORMAT(/20X,'TIME SERIES USED BY THIS OPERATION...'/
     1       25X,'CONTENTS',15X,'TS I.D.',5X,'TYPE',5X,'TIME INTERVAL'/
     2       25X,20('-'),2X,8('-'),5X,4('-'),5X,13('-')/
     3       25X,'RAINFALL/MELT',9X,2A4,5X,A4,I9,' HOURS'/
     4       25X,'RUNOFF',16X,2A4,5X,A4,I9,' HOURS'/
     5       25X,'WATER EQUIVALENT',6X,2A4,5X,A4,7X,'24 HOURS'/
     6       25X,'TEMPERATURE',11X,2A4,5X,A4,I9,' HOURS')
   13 FORMAT(25X,'CURRENT API',11X,2A4,5X,A4,7X,'24 HOURS')
   14 FORMAT(25X,'CURRENT AI', 12X,2A4,5X,A4,7X,'24 HOURS')
      END
