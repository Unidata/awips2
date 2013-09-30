c$pragma c (pin64_is_input_valid)
c$pragma c (pin64_parse_input)
C MEMBER PIN64
C
c          9/27/07 add one space in PO array to contain value (1 or 0)
c                  indicating whether we need to use RAIN+ MELT grids to ingest
c                  into SAC-SMA
c
c
c

      SUBROUTINE PIN64(PO,LEFTP,IUSEP,CO,LEFTC,IUSEC)
C.......................................................................
C  THIS IS THE INPUT SUBROUTINE FOR DISTRIBUTED HYDROLOGICAL MODELLING OPERATION.
C.......................................................................
C  SUBROUTINE INITIALLY WRITTEN BY. . .
C            DHM TEAM.  SEPT 2005
c
c   errorflag  = 1 - Error in using the same time series ID  
c              = 2 - outlet timeseries type error
c                    (not SQIN)
c              = 3 - Time interval error
c              = 5 - inflow timeseries type error
c                    (neither SQIN nor QINE)
c              = 0 - good data
c          9/27/07 add one space in PO array to contain value (1 or 0)
c                  indicating whether we need to use RAIN+ MELT to ingest
c                  into SAC-SMA
C.......................................................................
      PARAMETER (LARRAY=50000)
	  
      DIMENSION PO(1),CO(1),TSID(2),ARRAY(LARRAY)
      DIMENSION BASINID(2)
      DIMENSION SNAME(2)
      REAL*4    OPVER, TSTYPE, REALBLANK      
           
      INTEGER VALID, ERRORFLAG
      INTEGER NCARD, NOUTLET, NINFLOWS       
      CHARACTER*9 TSIDTMP
      CHARACTER*8 TIMESERIESID
      CHARACTER*9 BIDTMP
      CHARACTER*8 BID
      CHARACTER*5 TSTYPETMP
      CHARACTER*4 TIMESERIESTYPE
      
      INTEGER     IDT,IDTTMP
      INTEGER*2   MAXINFLOWS, MAXSIZE
      
      CHARACTER*8 LABEL
      CHARACTER*70 FIRSTCARD
      CHARACTER*80 BLANKCARD /' '/
      CHARACTER*4  BLANK /'    '/       
      CHARACTER*80 INPUTSTR,BUFFSTR,INBUFF 
      
      DIMENSION   LABEL(6)
      DIMENSION   IDTTMP(6),BIDTMP(6)
      DIMENSION   TSIDTMP(6), TSTYPETMP(6)
      DIMENSION   INBUFF(6),ERRORFLAG(6)

      DATA SNAME/4HPIN6,4H4   /
      DATA MAXINFLOWS/5/
      
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
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
C
C
C
C.......................................................................
C  SHOULD ALWAYS BE THE 1ST EXECUTABLE STATEMENT IN PRIMARY SUBROUTINE
      CALL FPRBUG (SNAME,1,64,IBUG)
C.......................................................................
C  TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF (ITRACE.GE.1) WRITE(IODBUG,900)
900   FORMAT (1H0,16H** PIN64 ENTERED)
  
       
C.......................................................................
C  INITIALIZE DATA VARIABLES
      NCARD=0
      VALID = 0
      NINFLOWS = 0
      NOUTLET = 0
C  INITIALIZE # OF SPACE USED IN THE PO AND CO ARRAY
cc      MAXSIZE=(6*MAXINFLOWS+8)
      MAXSIZE=(6*MAXINFLOWS+9)
      IUSEP=MAXSIZE
      IUSEC=1
      INBUFF=BLANKCARD
      
      READ(BLANK,'(A4)')REALBLANK
      DO 20 I = 8,MAXSIZE-1          
          PO(I)=REALBLANK
   20 CONTINUE  
      PO(MAXSIZE)=0   
      
C  VERSION NUMBER FOR THIS OPERATION
      OPVER=1.0
C.......................................................................
C  CHECK TO SEE IF ENOUGH SPACE IS LEFT IN P()
     
      CALL CHECKP(IUSEP,LEFTP,IERR)
      
      IF (IERR.EQ.1) THEN
         WRITE (IPR,901) (SNAME(I),I=1,2),IUSEP,LEFTP
901      FORMAT (1H0,10X,'**ERROR** ',2A4,' NEEDS',I4,
     +     ' WORDS IN P ARRAY,',/,10X,' BUT ONLY',I4,' ARE AVAILABLE.')
         CALL ERROR
         IUSEP=0
         RETURN
      ENDIF
C  CHECK TO SEE IF ENOUGH SPACE IS LEFT IN C()
      CALL CHECKC(IUSEC,LEFTC,IERR)
      
      IF (IERR.EQ.1) THEN
         WRITE (IPR,*) 'NOT ENOUGH SPACE IN CO ARRAY'
         CALL ERROR
         IUSEC=0
         RETURN
      ENDIF

C.......................................................................
C  READ PARAMETERS FROM DHM-OP INPUT CARD  
      NCARD = 0
        
1     READ(IN,'(A)')INPUTSTR      
      BUFFSTR = INPUTSTR

      CALL PIN64_IS_INPUT_VALID(BUFFSTR,VALID)      
      IF(VALID .EQ. 3)THEN	      
	  PO(MAXSIZE) = 1
      END IF
  
      IF(VALID .EQ. 1 .OR. VALID .eq. 2) THEN
         NCARD = NCARD + 1
         INBUFF(NCARD) = INPUTSTR  	 
      END IF 
      
      IF(VALID .EQ. 1) THEN
          NOUTLET = NOUTLET + 1          
      END IF            
      IF(VALID .EQ. 2) THEN         
          NINFLOWS = NINFLOWS + 1   
      END IF
      IF(VALID .EQ. 0) THEN    
         BACKSPACE(IN)        
         GOTO 5
      END IF 
      
      GOTO 1     
      
  5   IF(NOUTLET .GT. 1 .OR. NOUTLET .EQ. 0)THEN
         
         WRITE(IPR,101)NOUTLET
 101     FORMAT ('**ERROR** NUMBER OF OUTLETS = ',I2
     1             'MUST HAVE ONE OUTLET LABEL')
         CALL ERROR
	     IUSEP=0
         GOTO 300 
      END IF
      IF(NINFLOWS .GT. 5) THEN
         
         WRITE(IPR,102)NINFLOWS
 102     FORMAT ('**ERROR** NUMBER OF INFOWS = ',I2
     1           'MUST HAVE LESS THAN OR EQUAL TO 5 INFLOWS')
         CALL ERROR
	     IUSEP=0
        
         GOTO 300 
      END IF
   
      CALL PIN64_PARSE_INPUT(INBUFF,NCARD,TSIDTMP,TSTYPETMP,
     +       IDTTMP,BIDTMP,ERRORFLAG,LABEL)
      
      K = 1
      DO 100 I = 1, NCARD
          BID = BIDTMP(I)
	  IDT = IDTTMP(I)
	  TIMESERIESTYPE = TSTYPETMP(I)
	  TIMESERIESID = TSIDTMP(I)
	 
C.......................................................................
C CHECK IF DATA-TYPES ARE CORRECT
      
	  IF(ERRORFLAG(I) .EQ. 5)THEN
	             
              WRITE(IPR,904)TIMESERIESTYPE
              CALL ERROR
	          IUSEP=0
              GOTO 300
          ENDIF
	  
	  IF (ERRORFLAG(I) .EQ. 2) THEN
              WRITE(IPR,905)TIMESERIESTYPE
              CALL ERROR
	          IUSEP=0
              GOTO 300
	  ENDIF
	  
      
904   FORMAT ('**ERROR** TIME SERIES DATA TYPE MUST BE SQIN OR QINE'
     1    ' FOR DHM-OP INFLOW TIME-SERIES CHANGE ',A4,' TO SQIN.')

905   FORMAT ('**ERROR** TIME SERIES DATA TYPE MUST BE SQIN FOR DHM-OP '
     1    'OUTLET TIME-SERIES CHANGE ',A4,' TO SQIN.')
     
C.......................................................................
C CHECK IF INTERVAL IS 1
          IF (ERRORFLAG(I).EQ.3) THEN
              WRITE(IPR,906)IDT
              CALL ERROR
	      IUSEP=0
              GOTO 300
          ENDIF
906       FORMAT ('**ERROR** TIME STEP INTERVAL MUST BE 1 FOR DHM-OP.'
     1          'CHANGE ',I2,' TO 1.')
         

C READ CHARS INTO REAL  
       READ(TIMESERIESID,'(2A4)')TSID
       READ(TIMESERIESTYPE,'(A4)')TSTYPE
       READ(BID,'(2A4)')BASINID
C.......................................................................
C  CHECK IF THE SPECIFIED TIME SERIES EXISTS
       WRITE (IPR,111) TSID,TSTYPE,IDT
111    FORMAT ('CALL FINDTS : TSID=',2A4,' TSTYPE=',A,' IDT=',I2)

          CALL FINDTS(TSID,TSTYPE,IDT,LOCD,LOCTS,DIM)
          IF (LOCTS.LE.0) THEN
              GO TO 200
          END IF
	  
C.......................................................................
C  STORE PARAMETRIC DATA IN THE P ARRAY
     
        
          IF(ERRORFLAG(I) .EQ. 0) THEN
              IF(LABEL(I)(1:7).EQ."OUTLET:") THEN
                  PO(1)=OPVER+0.01
                  PO(2)=TSID(1)
                  PO(3)=TSID(2)
                  PO(4)=TSTYPE
                  PO(5)=IDT
                  PO(6)=BASINID(1)
                  PO(7)=BASINID(2)
                 
              END IF
c  Storing inflow data
c  po(8) = tsid (1) - next index after outlet data
c  po(9) = tsid (2)
c  po(10) = tstype
c  po(11) = idt 
c  po(..) up to po(37) - upto 5 inflows max 
c  Where K is index of NINFLOWS  PO(4*K+4)=TSID(1)
c                               PO(4*K+5)=TSID(2)  
c                               PO(4*K+6)=TSTYPE
c                               PO(4*K+7)= idt
c  
              IF(LABEL(I)(1:7).EQ."INFLOW:") THEN

                  PO(4*K+4)=TSID(1)
                  PO(4*K+5)=TSID(2)  
                  PO(4*K+6)=TSTYPE
                  PO(4*K+7)=1.0
c store basin id from index 28 for the first inflow basin ID
c keep increment index until reaching to the max of inflows	     
                  IF(K.EQ.1) THEN
		      PO(28)=BASINID(1)
                      PO(29)=BASINID(2)
	          END IF
	          IF(K.EQ.2) THEN
		      PO(30)=BASINID(1)
                      PO(31)=BASINID(2)
	          END IF
	          IF(K.EQ.3) THEN
		      PO(32)=BASINID(1)
                      PO(33)=BASINID(2)
	          END IF
	          IF(K.EQ.4) THEN
		      PO(34)=BASINID(1)
                      PO(35)=BASINID(2)
	          END IF
	          IF(K.EQ.5) THEN
		      PO(36)=BASINID(1)
                      PO(37)=BASINID(2)
	          END IF
                  K=K+1
              END IF
              PO(MAXSIZE-1)=NINFLOWS 
	      
          END IF 
	    
100   CONTINUE

             
C  STORE CARRYOVER DATA IN THE CO ARRAY      
      CO(1)=1
C.......................................................................
C  P ARRAY LOADED AS REQUIRED, EXIT PIN64
C
      GO TO 300
C.......................................................................      
C  ERROR READING CARD
123   WRITE (IPR,907) NCARD
      CALL ERROR
	  GOTO 300
907   FORMAT ('0**ERROR** READING VALUES FROM CARD ',I1,'.')

C.......................................................................
C  TIME SERIES DOES NOT EXIST
200   WRITE (IPR,908) TSID,TSTYPE,IDT
908   FORMAT (1H0,10X,'**ERROR** THE TIME SERIES TO BE SET BY DHM-OP
     1 HAS NOT BEEN DEFINED.',3X,5HI.D.=,2A4,3X,5HTYPE=,A4,3X,3HDT=,I2,
     2/1X,5HHOURS,16X,40HTHUS, THIS OPERATION CANNOT BE EXECUTED.)
      CALL ERROR
C.......................................................................
300   IF (ITRACE.GE.1) WRITE(IODBUG,909)
909      FORMAT(1H0,13H** EXIT PIN64)
      RETURN
      END
