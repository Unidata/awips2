C MEMBER PRP38
C  (from old member FCPRP38)
C
      SUBROUTINE PRP38(PO)
C                             LAST UPDATE: 02/08/94.09:16:47 BY $WC20SV
C
C
C#######################################################################
C
C  THIS SUBROUTINE PRINTS OUT PARAMETRIC DATA FOR THE BASEFLOW
C  SIMULATION OPERATION STORED IN THE PO ARRAY BY THE PIN38
C  SUBROUTINE.
C
C#######################################################################
C
C
C  CONTENTS OF THE PO ARRAY:
C
C
C     WORD     NAME      DESCRIPTION                             UNITS
C   ________  ________   _______________________________________________
C      1       IVERS     VERSION NUMBER
C      2       METENG    METRIC/ENGLISH UNIT SWITCH FOR INPUT
C    3 - 4     TSIDBF    BASEFLOW TIME SERIES ID
C      5       DTCBF     DATA TYPE CODE FOR BASEFLOW TIME SERIES
C      6       IDELTA    BASEFLOW TIME SERIES TIME INTERVAL      HOURS
C      7       AREA      RIVER SEGMENT AREA                      KM**2
C      8       CBFLOW    CONSTANT BASEFLOW VALUE FOR SEGMENT     CMS
C      9       IVBF      VARIABLE BASEFLOW OPTION SWITCH
C                        = 0   NO VARIABLE BASEFLOW
C                        = 1   SINGLE BASEFLOW RECESSION COEFF
C                        = 2   RECESSION COEFF SUPPLIES AS T.S.
C     10       NVBFP     NUMBER OF PO ARRAY SPACES NEEDED FOR
C                        VARIABLE BASEFLOW PARAMETERS
C                        = 0   IF PO(9)=ZERO
C                        = 1   IF PO(9)=1
C                        = 3   IF PO(9)=2
C     11       IUSEP     TOTAL NUMBER OF PO ARRAY SPACES REQUIRED
C     12       IUSEC     TOTAL NUMBER OF CO ARRAY SPACES REQUIRED
C   13 - 17              EMPTY
C     18       BFREC     BASEFLOW RECESSION COEFF
C                        (USED IF PO(9) = 1)
C   18 - 19    TSIDRC    BASEFLOW REC COEFF TIME SERIES ID
C                        (USED IF PO(9) = 2)
C     20       DTCRC     DATA TYPE CODE FOR BASEFLOW REC T.S.
C                        (USED IF PO(9) = 2)
C
C#######################################################################
C
      DIMENSION PO(1),SUBNAM(2),TSIDBF(2),TSIDRC(2)
      DIMENSION UNITS(2),RENG(2)
      COMMON /IONUM/ IN,IPR,IPU
      COMMON /FENGMT/METRIC
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp38.f,v $
     . $',                                                             '
     .$Id: prp38.f,v 1.1 1995/09/17 18:50:17 dws Exp $
     . $' /
C    ===================================================================
C
      DATA SUBNAM/4hPRP3,4h8   /,NOP/38/,RNO/4hNO  /
      DATA UNITS/4hMETR,4hIC  /,RENG/4hENGL,4hISH /,YESNO/4hYES /
C
C  CALL DEBUG CHECK ROUTINE
C
      CALL FPRBUG(SUBNAM,1,NOP,IFDEB)
C
C  PULL PARAMETERS FROM THE PO ARRAY
C
      METENG=PO(2)
      TSIDBF(1)=PO(3)
      TSIDBF(2)=PO(4)
      DTCBF=PO(5)
      IDELTA=PO(6)
      AREA=PO(7)
      CBFLOW=PO(8)
      IVBF=PO(9)
      NVBFP=PO(10)
C
C  PULL IN PO(18) THROUGH PO(20) ONLY IF NECESSARY
C
      IF(NVBFP-1)300,200,100
100   TSIDRC(1)=PO(18)
      TSIDRC(2)=PO(19)
      DTCRC=PO(20)
      GO TO 300
200   BFREC=PO(18)
C
C  CHECK TO SEE WHAT UNITS TO DISPLAY DATA IN
C
300   IF(METRIC)310,320,330
310   IF(METENG)320,320,330
320   UNITS(1)=RENG(1)
      UNITS(2)=RENG(2)
      AREA=AREA/2.58999
      CBFLOW=CBFLOW/.028317
      METENG=0
      GO TO 340
330   METENG=1
340   IF(IVBF-1)350,360,360
350   YESNO=RNO
C
C  NOW PRINT OUT INFORMATION
C
360   WRITE(IPR,1000)
      IF(METENG-1)370,380,380
370   WRITE(IPR,1010)
      GO TO 390
380   WRITE(IPR,1015)
390   WRITE(IPR,1020)TSIDBF,DTCBF,IDELTA,AREA,CBFLOW,YESNO
      IF(NVBFP-1)10000,400,500
400   WRITE(IPR,1400)
      WRITE(IPR,1410)BFREC
      GO TO 10000
500   WRITE(IPR,1500)
      WRITE(IPR,1510)TSIDRC,DTCRC
      GO TO 10000
1000  FORMAT(////19X,'PARAMETRIC DATA FOR THE BASEFLOW SIMULATION ',
     1       'OPERATION:',//)
1010  FORMAT(10X,'BASEFLOW TIME SERIES INFORMATION',19X,'CONSTANT',
     1       5X,'VARIABLE',/15X,'ID',8X,'TYPE',5X,'DELTA-T',7X,
     2       'AREA',9X,'BASEFLOW',5X,'BASEFLOW',/34X,'(HOURS)',
     3       6X,'(MI**2)',9X,'(CFS)',/12X,8('-'),5X,4('-'),5X,
     4       7('-'),5X,9('-'),6X,8('-'),5X,8('-'),/)
1015  FORMAT(10X,'BASEFLOW TIME SERIES INFORMATION',19X,'CONSTANT',
     1       5X,'VARIABLE',/15X,'ID',8X,'TYPE',5X,'DELTA-T',7X,
     2       'AREA',9X,'BASEFLOW',5X,'BASEFLOW',/34X,'(HOURS)',
     3       6X,'(KM**2)',9X,'(CMS)',/12X,8('-'),5X,4('-'),5X,
     4       7('-'),5X,9('-'),6X,8('-'),5X,8('-'),/)
1020  FORMAT(12X,2A4,5X,A4,6X,I4,7X,F9.1,5X,F9.1,7X,A4)
1400  FORMAT(//22X,'BASEFLOW RECESSION COEFF',/22X,24('-'),/)
1410  FORMAT(41X,F5.3)
1500  FORMAT(//22X,'BASEFLOW RECESSION COEFF TIME SERIES INFORMATION:',
     1       /35X,'ID',8X,'TYPE',5X,'DELTA-T',/54X,'(HOURS)',
     2       /32X,8('-'),5X,4('-'),5X,6('-'),/)
1510  FORMAT(32X,2A4,5X,A4,7X,'24')
10000 RETURN
      END
