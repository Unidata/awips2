C MEMBER PRP64
C
      SUBROUTINE PRP64(PO)
C.......................................
C THIS SUBROUTINE PRINTS THE INFORMATION ASSOCIATED
C    WITH THE DISTRIBUTED HYDROLOGICAL MODELLING OPERATION.
C
c          9/27/07 add one space in PO array to contain value (1 or 0)
c                  indicating whether to use RAIN+ MELT grid to ingest
c                  into SAC-SMA Model
C SUBROUTINE INITIALLY WRITTEN BY. . .
C       
C    ADD MULTIPLE INFLOWS PRINT OUT 
C.......................................
      DIMENSION PO(1)
    
      REAL REALBASINID(2)
      REAL REALTIMESERIESID(2)
      REAL REALTYPE
      CHARACTER*4  TSTYPE
      CHARACTER*8  BASINID
      CHARACTER*8  TIMESERIESID
      CHARACTER*20 PRECIPTYPE
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
      INTEGER NINFLOWS
      INTEGER*2   MAXINFLOWS, MAXSIZE
      
      EQUIVALENCE(REALBASINID,BASINID)
      EQUIVALENCE(REALTIMESERIESID,TIMESERIESID)
      EQUIVALENCE(REALTYPE,TSTYPE)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source$
     . $',                                                             '
     .$Id$
     . $' /
C    ===================================================================
C
      DATA MAXINFLOWS/5/
C
C
C.......................................
C TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF (ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT (1H0,16H** PRP64 ENTERED)
C     NO DEBUG OUTPUT.
C.......................................
C PRINT INFORMATION ON TIME SERIES THAT IS TO
C BE INITIALIZED.
      MAXSIZE=(6*MAXINFLOWS+9)
      REALTIMESERIESID(1) = PO(2)
      REALTIMESERIESID(2) = PO(3)      
      REALTYPE = PO(4)
      IDT = PO(5)
      REALBASINID(1) = PO(6)
      REALBASINID(2) = PO(7)
      NINFLOWS = PO(MAXSIZE-1)
      IF(PO(MAXSIZE) .EQ. 1) THEN
          PRECIPTYPE = 'RAIN PLUS MELT'	 
      ELSE
          PRECIPTYPE = 'MPE'
      END IF
      
      WRITE(IPR,910)PRECIPTYPE,TIMESERIESID,TSTYPE,IDT,BASINID
  910 FORMAT (17X,'DISTRIBUTED HYDROLOGIC MODELING',//,
     + 17X,'PRECIP DATA TYPE:',2X,A18,//,
     + 21X,'TIME SERIES AND BASIN(S) USED BY THIS OPERATION',//,
     + 16X,'CONTENTS',14X,'ID',9X,'TYPE',5X,'TIME INTERVAL',4X,
     +     'BASIN ID',//,
     + 16X,'OUTLET FLOW',11X,A8,3X,A4,4X,I2, 5H HOURS,11X,A8/) 
      
      IF(NINFLOWS.GT.0) THEN 
      DO 10 K = 1, NINFLOWS        

            REALTIMESERIESID(1) = PO(4*K+4)
            REALTIMESERIESID(2) = PO(4*K+5)
            REALTYPE = PO(4*K+6)
            IDT = PO(4*K+7)

         if (k .eq. 1) then
	      REALBASINID(1) = PO(28)
              REALBASINID(2) = PO(29)
	 end if
	 if (k .eq. 2) then
	      REALBASINID(1) = PO(30)
              REALBASINID(2) = PO(31)
	 end if
	 if (k .eq. 3) then
	      REALBASINID(1) = PO(32)
              REALBASINID(2) = PO(33)
	 end if
	 if (k .eq. 4) then
	      REALBASINID(1) = PO(34)
              REALBASINID(2) = PO(35)
	 end if
	 if (k .eq. 5) then
	      REALBASINID(1) = PO(36)
              REALBASINID(2) = PO(37)
	 end if
         WRITE(IPR,920)TIMESERIESID,TSTYPE,
     1        IDT,BASINID
  920    FORMAT(16X,'INFLOW',16X,A8,3X,A4,4X,I2,5H HOURS,11X,A8)
  10  CONTINUE     
      ENDIF
C.......................................
      RETURN
      END
