C MODULE MXX_CALC
C  =====================================================================
C  pgm: MXX_CALC .. Calculates mapx, cv, zp over areas for one time step
C
C  INPUT VARIABLES:
C	IUNIT - DEVICE NUMBER TO READ NEXRAD GRID
C	SEG_BND(3,NSGM(MAXBSN)) - BOUNDARY DEFINITION FOR EACH AREA; 
C               ARRAY INCLUDE THREE COLUMNS: ROW NUMBER OF SEGMENT, 
C		START COLUMN NUMBER IN THE SEGMENT, END COLUMN NUMBER
C       MAXBSN - SIZE OF ARRAYS
C                 (set in routine mx_main.f, passed thru mxx_mapx.f
C                  to set array sizes, then passed here in case a
C                  future statement needs to limit accessing an array)
C	NAREA - NUMBER OF SELECTED AREAS
C	NSGM(MAXBSN) - CUMULATED NUMBER OF SEGMENTS FOR EACH AREA
C       MAXROW - BIGGEST ROW NUMBER IN SEGMENT DEFINITION
C       MINROW - LOWEST ROW NUMBER IN SEGMENT DEFINITION
C	IHRAP(4) - RADAR GRID HEADER INFORMATION:
C	XOR      - X-ORIGIN VALUE
C	YOR	 - Y-ORIGIN VALUE
C	NX       - NUMBER OF COLUMNS IN RECORD
C	NY       - NUMBER OF ROWS
C       IFLAG	 - EXISTANCE OF NEXRADE FILE: 0 - IF YES; ANY - IF NOT
C   OUTPUT VARIABLES:
C	MAPX(MAXBSN) - ESTIMATED MAPX VALUE FOR EACH AREA
C       MAPCV(MAXBSN) - ESTIMATED COEFF. OF VARIATION OF PRECIP OVER
C                      RAINY AREA
C       ZEROP(MAXBSN) - ESTIMATED ZERO RAIN FRACTION OF AREA
C   VARIABLES 'IRADAR(*) AND NSUM(*)' ARE INCLUDED IN ARGUMENT LIST
C   TO KEEP FLEXIBILITY IN THEIR DIMENSION DEFINITIONS
C  =====================================================================
      SUBROUTINE MXX_CALC(IUNIT,SEG_BND,MAXBSN,NAREA,NSGM,MAXROW,
     +                   MINROW,XOR,YOR,NX,NY,IRADAR,MAPX,NSUM,
     +                   MAPCV,ZEROP,IFLAG)

      INTEGER SEG_BND(3,*),NSGM(*),NSUM(*)
      REAL MAPX(*),MAPCV(*),ZEROP(*)
      REAL TMPCV(1000)
      INTEGER*2 IRADAR(NX), KPASS(NAREA)
      INTEGER XOR,YOR,NX,NY
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxx_calc.f,v $
     . $',                                                             '
     .$Id: mxx_calc.f,v 1.2 2005/06/09 19:27:47 dws Exp $
     . $' /
C    ===================================================================
C

c-      CALL FPRBUG('MXX_CALC',1,2,1)
       
      DO I=1,NAREA
        MAPX(I)=0.
        NSUM(I)=0
        KPASS(I)=0
        MAPCV(I)=0.
        ZEROP(I)=0.
        TMPCV(I)=0.
      ENDDO
      
C   SKIP MAPX CALCULATIONS IF NEXRAD FILE DOES NOT EXIST
      IF(IFLAG .NE. 0) GOTO 10
      
C  SKIP FIRST 'MINROW-1' ROWS FROM HRAP GRID
      DO I=YOR,MINROW-1
        READ(IUNIT,ERR=99,END=99) 
      ENDDO

CC GET MAX AND MIN SEGMENT NUMBER FROM ALL AREAS
CC    MAXROW=0
CC    MINROW=9999
CC    DO I=1,NSGM(NAREA)
CC     IF(SEG_BND(1,I) .LE. MINROW) MINROW=SEG_BND(1,I)
CC     IF(SEG_BND(1,I) .GE. MAXROW) MAXROW=SEG_BND(1,I)
CC    ENDDO
      
C  LOOP THROUGH {MINROW,MAXROW} FROM SEGMENT DEFINITION
C  OVER ALL AREAS
      DO I=MINROW,MAXROW
       READ(IUNIT,ERR=99,END=99) IRADAR
 
C  LOOP THROUGH ALL AREAS TO CALCULATE MAPX
       DO J=1,NAREA
        
        IF(J .EQ. 1) THEN
         KE=1
        ELSE
         KE=NSGM(J-1)+1
        ENDIF

C  LOOP THROUGH ALL SEGMENTS IN THE AREA STARTING FROM THE BOTTOM SEGMENT
        DO K=NSGM(J)-KPASS(J),KE,-1

C  THERE IS NO ANY PIXEL IN THE SEGMENT FOR THIS AREA
         IF(SEG_BND(1,K) .GT. I) GOTO 7
         
C  SUM UP ALL PIXELS THAT ARE IN THE SEGMENT FOR THIS AREA
         KPASS(J)=KPASS(J)+1 
         IF(SEG_BND(1,K) .EQ. I) THEN
          DO KX=SEG_BND(2,K),SEG_BND(3,K)
           IXX=KX-XOR+1
           IF(IRADAR(IXX) .GE. 0) THEN
            MAPX(J)=MAPX(J)+IRADAR(IXX)
            NSUM(J)=NSUM(J)+1

C  CALCULATION CV AND ZP
            IF(IRADAR(IXX) .GT. 1.) THEN
             MAPCV(J)=MAPCV(J)+IRADAR(IXX)**2
             TMPCV(J)=TMPCV(J)+IRADAR(IXX)
             ZEROP(J)=ZEROP(J)+1
            ENDIF 
             
           ENDIF
          ENDDO 
          
         ENDIF
        ENDDO 
        
7       CONTINUE
       ENDDO 
       
      ENDDO 

C  CALCULATE MAPX, Cv AND Zp OVER EACH AREA
10    DO J=1,NAREA

       IF(ZEROP(J) .GT. 0.) THEN
         XX=TMPCV(J)*0.01/ZEROP(J)
         SIGM=(MAPCV(J)*0.0001/ZEROP(J)-XX*XX)
         IF(SIGM .LT. 0.) THEN
          MAPCV(J)=0.
         ELSE
          MAPCV(J)=SQRT(SIGM)
         ENDIF  
         IF(XX .GT. 1E-4) THEN
          MAPCV(J)=MAPCV(J)/XX
         ELSE
          MAPCV(J)=10.
         ENDIF  
         ZEROP(J)=1.-ZEROP(J)/NSUM(J)
       ELSE
         MAPCV(J)=0.
         ZEROP(J)=1.  
       ENDIF  
         
       IF(NSUM(J) .NE. 0) THEN
        MAPX(J)=MAPX(J)*0.01/NSUM(J)
       ELSE
        MAPX(J)=-99.
       ENDIF
      ENDDO
      
      RETURN

99    CONTINUE
      IFLAG=99
      DO J=1,NAREA
       MAPX(J)=-99.
      ENDDO 
      RETURN      
      END
