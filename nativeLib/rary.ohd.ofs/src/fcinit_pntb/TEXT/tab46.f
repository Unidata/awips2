        SUBROUTINE TAB46(TO,LEFT,IUSET,NXT,LPO,PO,LCO,TS,MTS,
     $                   LWORK,IDT)
     
C................................................................
C       THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE
C       NO MISSING VALUES OPERATION.  ORIGINALLY WRITTEN BY
C       MIKE SMITH, HRL  NOVEMBER 22, 1995

        DIMENSION PO(1), TS(MTS)
	INTEGER TO(1)
	DIMENSION SNAME(2)
	
C       COMMON BLOCKS.
        COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab46.f,v $
     . $',                                                             '
     .$Id: tab46.f,v 1.1 1996/07/11 21:23:37 dws Exp $
     . $' /
C    ===================================================================
C
	
C       DATA STATEMENTS
        DATA SNAME/4HTAB4,4H6   /
	
c	TRACE LEVEL=1, DBUG SWITHC=IBUG
	CALL FPRBUG(SNAME,1,46,IBUG)
c
C...............................................................
C       INITIAL VALUES
          LWORK = 0
	  IDT =   PO(5)
	  LENGTH= 6
	  
C...............................................................
C       CHECK TO SEE IF SPACE IS AVAILABLE IN T()
          CALL CHECKT(LENGTH,LEFT,IERR)
	  IF(IERR.EQ.0) GO TO 100
	  IUSET = 0
	  IDT =   0
	  RETURN
C...............................................................
C       SPACE IS AVAILABLE, MAKE ENTRIES INTO TO()
 100     TO(1) = 46
         TO(2) = NXT+LENGTH
	 TO(3) = LPO
	 TO(4) = LCO
c	
C...............................................................
C       STORE LOCATION OF INPUT TIME SERIES.
          CALL CKINPT(PO(2),PO(4),IDT,LD,TS,MTS,IERR)
	  TO(5) = LD
	  
C       STORE LOCATION OF OUTPUT TIME SERIES.
          CALL FINDTS(PO(6),PO(8),IDT,LD,LTS,DIM)
	  TO(6) = LD

c        PUT FLAG=1 TO INDICATE TO OTHER OPERATIONS THAT THE
C        TIME SERIES CONTAINS DATA VALUES-

          IF(LTS.GT.0) TS(LTS+8)=1.01
	  IUSET=LENGTH
	 
C        ALL ENTRIES HAVE BEEN MADE
C.............................................................
C        DEBUG OUTPUT
          IF(IBUG.EQ.0) GO TO 199
	  WRITE(IODBUG,900) (TO(I),I=1,IUSET)
 900      FORMAT(1H0,10X,'NOMSNG DEBUG - TAB ROUTINE',
     $    /,10X,'CONTENTS OF TO ARRAY:',2X,6I6)      
     
C..............................................................
C       THE TO ARRAY ENTRIES ARE AS FOLLOWS
C        POSITION          CONTENTS
C           1      I.D. NUMNBER FOR THE OPERATION = 46
C           2      LOCATION OF THE NEXT OPERATION IN T ARRAY
C           3      LOCATION OF PARAMETERS IN THE P ARRAY = LPO
C           4      LOCATION OF THE CARRYOVER IN THE C ARRAY = LCO
C           5      LOCATION OF INPUT TIME SERIES IN D ARRAY 
C           6      LOCATION OF OUTPUT TIME SERIES IN D ARRAY
C................................................................

  199  CONTINUE
       RETURN
       END
 
