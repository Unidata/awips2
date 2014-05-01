        SUBROUTINE COX46(POLD,COLD,PONEW,CONEW)

C      THIS IS THE CARRYOVER TRANSFER ROUTINE FOR THE NOMSNG OPERATION
C      ORIGINALLY WRITTEN BY MIKE SMITH HRL June. 1996
C
C      THREE CASES FOR CARRYOVER TRANSFER
C       1.  IF T.S. ID, TIME STEP, AND DATA TYPE ARE THE SAME, 
C           TRANSFER OLD CARRYOVER
C       2.  IF T.S. ID AND DATA TYPE ARE THE SAME, BUT TIME STEP IS
C           DIFFERENT, ADJUST CO(2) BY RATIO: 
C               OLD TIME STEP/NEW TIME STEP
C       3.  IF T.S. ID, DATA TYPE AND TIME STEP ARE NOT EQUAL, USE INITIAL
C           CARRYOVER FROM PIN ROUTINE
        
        
        DIMENSION POLD(1),COLD(1),PONEW(1),CONEW(1)
        DIMENSION SNAME(2)
        
        COMMON/IONUM/IN,IPR,IPU
        COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cox46.f,v $
     . $',                                                             '
     .$Id: cox46.f,v 1.2 2001/06/13 10:41:48 mgm Exp $
     . $' /
C    ===================================================================
C
        
        DATA SNAME/4HCOX4,4H6   /
        
C       TRACE LEVEL FOR SUBROUTINE = 1, DEBUG SWITCH=IBUG
        CALL FPRBUG(SNAME,1,46,IBUG)
        
        
        IF (IBUG.EQ.1) THEN
           WRITE(IPR,10)
           WRITE(IPR,15) CONEW(1),CONEW(2)      
  10    FORMAT(/10X,'SUCCESSFUL CALL TO COX46......'/)
  15    FORMAT(10X,'CONEW(1)= ',F10.2,2X,'CONEW(2)= ',F10.2)
           ENDIF
C....................................................................
C         DEBUG OUTPUT... PRINT - POLD() AND COLD(), PONEW(), CONEW()

       IF(IBUG.EQ.0) GO TO 30
       IUSEP=12
          
       WRITE(IODBUG,20) (POLD(I),I=1,IUSEP)
 20    FORMAT(10X,'POLD= ',F10.2,2X,2A4,1X,A4,F10.2,2X,2A4,1X,A4,6F10.2)
 
       WRITE(IODBUG,21) COLD(1), COLD(2)
 21    FORMAT(/,10X,'COLD(1) = ',F10.2,2X,'COLD(2)= ',I3)
       
       WRITE(IODBUG,22) (PONEW(I),I=1,IUSEP)
 22    FORMAT(10X,'PONEW= ',F10.2,2X,2A4,1X,A4,F10.2,2X,
     $2A4,1X,A4,6F10.2)
 
       WRITE(IODBUG,23) CONEW(1), CONEW(2)
 23    FORMAT(/,10X,'CONEW(1) = ',F10.2,2X,'CONEW(2)= ',I3)

C..................................................................
 30    CONTINUE
C       CHECK DIMENSIONS AND UNITS OF TIME SERIES A
         CALL FDCODE(PONEW(4),UNEW,DIMNEW,MSG,NPDT,TSCALE,NADD,IERR)
         CALL FDCODE(POLD(4),UOLD,DIMOLD,MSG,NPDT,TSCALE,NADD,IERR)
         
 
      IF(POLD(2).EQ.PONEW(2).AND.POLD(3).EQ.PONEW(3).AND.
     *POLD(4).EQ.PONEW(4))THEN
           IF(POLD(5).EQ.PONEW(5))THEN
           
C           CASE 1.  OLD CARRYOVER VALUES CAN BE USED
              CONEW(1)=COLD(1)
              CONEW(2)=COLD(2)
              IF (IBUG.EQ.1) WRITE(IODBUG,91)
  91         FORMAT(/,2X,'TIME SERIES ID, DATA TYPE, AND TIME STEP',
     $' ARE THE SAME..... CARRYOVER IS TRANSFERRED')
             
           ELSE

             
C           CASE 2.  OLD CARRYOVER CAN BE USED, BUT ADJUST COLD(2)
             CONEW(1)=COLD(1)
c             convert to integer values
              II=POLD(5)
              JJ=PONEW(5)
              CONEW(2)=COLD(2)*II/JJ
              IF(CONEW(2).LT.1) CONEW(2)=1
              
           ENDIF
           
      ELSE
C         CASE 3. NO CARRYOVER TRANSFER...OLD AND NEW TIME SERIES DO NOT HAVE 
C         THE SAME ID, DATA TYPE AND TIME STEP.  CARRYOVER VALUES
C         OBTAINED FROM  PIN ROUTINE.

          WRITE(IPR,90)POLD(2),POLD(3),POLD(4),POLD(5),PONEW(2),
     $          PONEW(3),PONEW(4),PONEW(5)
          
           
 90    FORMAT(10X,'** WARNING **  NO CARRYOVER TRANS, OLD AND NEW',
     1' TIME SERIES DO NOT HAVE THE SAME ID, DATA TYPE, AND TIME STEP',
     2/25X,'ID, DATA TYPE AND TIME STEP OF OLD T.S. =',1X,2A4,1X,A4,1X,
     3I2,/25X,'ID, DATA TYPE AND TIME STEP OF NEW T.S  =',1X,2A4,1X,A4,
     41X,I2,/,10X,'USE INITIAL CARRYOVER FROM SEGMENT (RE)DEF')
     
         CALL WARN 
          
       ENDIF
         
    
c...................................................................        
C      DEBUG OUTPUT: PRINT FINAL CARRYOVER VALUES
 101   IF(IBUG.EQ.1) WRITE(IODBUG,102) CONEW(1),CONEW(2)
 102   FORMAT(/,10X,'FINAL CONEW(1)= ',F10.2,2X,'CONEW(2)= ',F10.2)
C...................................................................
        RETURN
        END
