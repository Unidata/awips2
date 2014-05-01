      SUBROUTINE PRP46(PO)
       
C      THIS IS THE PRINT PARAMETER SUBROUTINE FOR THE 
C      NO MISSING VALUES OPERATION

C      SUBROUTINE ORIGINALLY WRITTEN BY  MIKE SMITH HRL  SEPTEMBER, 1994

        DIMENSION PO(1)
        DIMENSION SNAME(2)
        
C       COMMON BLOCKS
         COMMON/IONUM/IN,IPR,IPU
         COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp46.f,v $
     . $',                                                             '
     .$Id: prp46.f,v 1.1 1996/07/11 21:23:01 dws Exp $
     . $' /
C    ===================================================================
C
         
C       DATA STATEMENTS
         DATA SNAME/4HPRP4,4H6   /
         
C......................................................................
C      TRACE LEVEL FOR SUBROUTINE=1,  DEBUG SWITCH=IBUG
 
          CALL FPRBUG(SNAME,1,46,IBUG)
C.....................................................................
C      GET CONTROL VARIABLES
            ITH    = PO(5)
            IRDCO  = PO(9)
            INTERP = PO(10)
C.....................................................................

C  PRINT INFORMATION ON INPUT AND NO MSNG TIME SERIES

       WRITE(IPR,900)
       WRITE(IPR,901) (PO(I),I=2,4),ITH, (PO(I),I=6,8), ITH
       
       IF(IRDCO.EQ.1) THEN
        WRITE(IPR,902)
       ELSEIF(IRDCO.EQ.0) THEN
        WRITE(IPR,903)
       ENDIF 
       
C  PRINT INFORMATION ABOUT OPTIONS

       IF(INTERP.EQ.0)THEN
        WRITE(IPR,904)
       ELSEIF(INTERP.EQ.1) THEN
        WRITE(IPR,905) 
       ENDIF

       WRITE(IPR,906) PO(11)
       
  900  FORMAT(/,10X,'NO MISSING VALUE TIME SERIES OPERATION',
     $' PRINT PARAMETER ROUTINE',/)   
  901  FORMAT(10X,'INPUT TIME SERIES I.D.= ',2A4,3X,'TYPE= ',A4,3X,
     1 15HTIME INTERVAL= ,I2,1X,7HHOURS).
     2 /10X,'NO MISSING TIME SERIES (I.D.= ',2A4,
     3 3X,5HTYPE=,A4,3X,15HTIME INTERVAL= ,I2,1X,7HHOURS).)
     
  902  FORMAT(10X,'OPTION FOR USER DEFINED INITIAL CARRYOVER SELECTED')
  903  FORMAT(10X,'OPTION FOR DEFAULT INITIAL CARRYOVER SELECTED')
  904  FORMAT(10X,'OPTION TO RETAIN LAST VALUE SELECTED')
  905  FORMAT(10X,'OPTION TO LINEARLY INTERPOLATE MISSING VALUES',
     $' SELECTED')
  906  FORMAT(10X,'RECESSION COEFFICIENT IS ',F4.2,2X,' FOR EXTENDING ',
     $'TIME SERIES FORWARDS OR BACKWARDS')

     
       RETURN
       END
