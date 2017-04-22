C.......................................
C FILE: PUC64.F
C AUTHOR(S): A. VO, A. VOELLMY, L. CAJINA, W. KWOCK
C DATE CREATED: 5/5/06
C DEVELOPMENT GROUP: OHD HSEB
C PURPOSE: THIS SUBROUTINE PUNCHES THE DHM-OP DEFINITION
C MODULE(S): PUC64
c          9/27/07 add one space in PO array to contain value (1 or 0)
c                  indicating whether we need to use RAIN+ MELT grids to ingest
c                  into SAC-SMA
C.......................................

      SUBROUTINE PUC64(PO)

      DIMENSION PO(1)
      
      INTEGER NUPSTREAMBASIN,TSCCOUNT
      INTEGER*2   MAXINFLOWS, MAXSIZE
      INTEGER NINFLOWS
      REAL REALBASINID(2)
      REAL REALTIMESERIESID(2)
      REAL REALTYPE
     
      CHARACTER*4  TSTYPE,TSTYPETMP
      CHARACTER*8  BASINID,BIDTMP
      CHARACTER*8  TIMESERIESID,TSIDTMP
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
      DATA MAXINFLOWS/5/

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

C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF (ITRACE.GE.1) WRITE(IODBUG,900)
C     NO DEBUG OUTPUT.
C.......................................
C     PUNCH INPUT CARD.
	
      IDT = PO(5)     
      MAXSIZE=(6*MAXINFLOWS+9)
      NINFLOWS = PO(MAXSIZE-1)
     
      IF(NINFLOWS .GT. 0) THEN
        DO 10 K=1,NINFLOWS
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
          WRITE(IPU,901)TIMESERIESID,TSTYPE,IDT,BASINID   
 10     CONTINUE

         
      ENDIF
      
      REALTIMESERIESID(1) = PO(2)
      REALTIMESERIESID(2) = PO(3)
      REALTYPE = PO(4)
      IDT = PO(5)
      REALBASINID(1) = PO(6)
      REALBASINID(2) = PO(7)
      WRITE(IPU,902) TIMESERIESID,TSTYPE,IDT,BASINID
C......................................
900   FORMAT (1H0,16H** PUC64 ENTERED)
901   FORMAT (2X,'INFLOW:',1X,A8,1X,A4,3X,I2,3X,A8)
902   FORMAT (2X,'OUTLET:',1X,A8,1X,A4,3X,I2,3X,A8)

C.......................................
      RETURN
      END
