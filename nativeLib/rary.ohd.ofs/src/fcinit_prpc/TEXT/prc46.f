      SUBROUTINE PRC46(PO,CO)
      
C     THIS IS THE PRINT CARRYOVER SUBROUTINE FOR
C     THE NO MISSING VALUES TIME SERIES OPERATION
 
C     ORIGINALLY WRITTEN BY MIKE SMITH HRL SEPT. 1994

C.......................................................................
      DIMENSION PO(1),CO(1)
      DIMENSION SNAME(2)
      
C     COMMON BLOCKS
        COMMON/IONUM/IN,IPR,IPU
        COMMON/FDBUG/IODBUG/ITRACE/IDBALL,NDEBUG,IDEBUG(20)
        COMMON/FCONIT/IVALUE
        COMMON/FENGMT/ METRIC
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc46.f,v $
     . $',                                                             '
     .$Id: prc46.f,v 1.2 2002/02/11 13:22:11 michaelo Exp $
     . $' /
C    ===================================================================
C
        
C     DATA STATEMENTS
        DATA SNAME/4HPRC4,4H6   /
C......................................................................
C     TRACE LEVEL FOR SUBROUTINE = 1, DEBUG SWITCH = IBUG
        CALL FPRBUG(SNAME,1,46,IBUG)
C......................................................................
C     PRINT CARRYOVER VALUE.
C     IVALUE=1, CO ARRAY CONTAINS INITIAL VALUE FROM PIN ROUTINE;
C     IVALUE=0, NOT INITIAL VALUE
      
      WRITE(IPR,900)
  900  FORMAT(/,10X,'NO-MSNG OPERATION PRINT CARRYOVER ROUTINE')
       IF(IVALUE.NE.1) GO TO 100
          ICO=PO(9)


          IF(ICO.EQ.1) GO TO 102
  103      WRITE(IPR,901)
  901      FORMAT(10X,'CARRYOVER SET TO DEFAULT VALUE OF -999.0 ',
     $     'OCCURING AT BEGINNING OF RUN')
     
           GO TO 101
 
  100   IF((CO(1).LT.-998.99).AND.(CO(1).GT.-999.01)) GO TO 103
  102   TYPE=PO(4)
  
        
         CALL FDCODE(TYPE,UNIT,DIM,MSG,NPDT,TSCALE,NADD,IERR)
         
         ITH = PO(5)
         PCO1 = CO(1)
	 PCO2 = CO(2)
         PUNIT=UNIT
          
           IF(METRIC.EQ.0) THEN
             CALL FCONVT(UNIT,DIM,PUNIT,CMULT,CADD,IERR)
             PCO1=CO(1)*CMULT+CADD
           ENDIF

C Added ITEMP=PCo2, because I think when it was printing just PCO2,
C it was causing a core dump on Linux.  (Hank Herr, 12/11/01)   
       ITEMP=PCO2
       WRITE(IPR,902) PCO1,PUNIT,(PO(I),I=2,4),ITH,ITEMP

  902      FORMAT(/,10X,'NO MSNG TS CARRYOVER= ',F10.2,4X,'UNITS: ',A4,
     1     /10X,'THIS IS THE VALUE OF TIME SERIES (I.D.=',1x,2A4,3X,
     2     'TYPE= ',A4,3X,'DT= ',I2,1X,'HOURS',1X,I3,1X,
     3     'TIME STEPS BEFORE CARRY OVER DATE'/)
  101    CONTINUE
         
          RETURN
          END  
	  
