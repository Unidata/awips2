      SUBROUTINE UNPKLX(KFILDO,IPACK,NDX,LOC,IPOS,IC,NXY,NBIT,
     1                  L3264B,IER,*)
C
C        APRIL 1997   GLAHN   TDL   HP
C        MAY   1997   GLAHN   MODIFIED UNPACKING ALGORITHM TO USE
C                             MVBITS RATHER THAN SHIFTING AND ORING.
C
C        PURPOSE 
C            TO UNPACK NXY VALUES FROM IPACK( ).  THE PACKED VALUES
C            ARE RETURNED IN IC( ) WITH NO REFERENCE VALUE OR 
C            SCALING CONSIDERED.  UNPKLX ELIMINATES THE CALLING OF
C            UNPKBG, AND RATHER INCORPORATES IT INTO THE LOOP.
C            SINCE THIS IS A HIGHLY USED ROUTINE, ALL REASONABLE
C            ATTEMPTS AT EFFICIENCY MUST BE PURSUED.
C
C        DATA SET USE 
C           KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE. (OUTPUT) 
C
C        VARIABLES 
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT) 
C            IPACK(J) = THE ARRAY HOLDING THE ACTUAL PACKED MESSAGE
C                       (J=1,MAX OF NDX).  (INPUT)
C                 NDX = DIMENSION OF IPACK( ).  (INPUT)
C                 LOC = HOLDS WORD POSITION IN IPACK OF NEXT VALUE TO
C                       UNPACK.  (INPUT/OUTPUT)
C                IPOS = HOLDS BIT POSITION IN IPACK(LOC) OF THE FIRST
C                       BIT OF THE NEXT VALUE TO UNPACK.  (INPUT/OUTPUT)
C               IC(K) = UNPACKED DATA RETURNED (K=1,NXY).  (OUTPUT)
C                 NXY = DIMENSION OF IC( ).  THE NUMBER
C                       OF UNPACKED VALUES RETURNED.  (INPUT)
C                NBIT = THE NUMBER OF BITS TO UNPACK FOR EACH VALUE.
C                       (INPUT)
C              L3264B = INTEGER WORD LENGTH OF MACHINE BEING USED.
C                       (INPUT)
C                 IER = ERROR RETURN.  (OUTPUT)
C                       6 = NDX IS NOT LARGE ENOUGH TO FURNISH THE BITS
C                           NECESSARY TO ACCOMMODATE NXY VALUES STARTING
C                           AT THE VALUES LOC AND IPOS.
C                       7 = IPOS NOT IN THE RANGE 1-L3264B.
C                       8 = IBIT NOT IN THE RANGE 0-32.
C                   * = ALTERNATE ERROR RETURN WHEN IER IS NOT "0".
C
C        NON SYSTEM SUBROUTINES CALLED 
C           NONE
C
      DIMENSION IPACK(NDX)
      DIMENSION IC(NXY)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2unpacker/RCS/unpklx.f,v $
     . $',                                                             '
     .$Id: unpklx.f,v 1.1 2004/09/16 16:51:50 dsa Exp $
     . $' /
C    ===================================================================
C
C
C        SET ERROR RETURN AND ZERO ARRAYS.
C
      IER=0
C
C        CHECK LEGITIMATE VALUES OF LOC AND IPOS, AND WHETHER
C        NDX IS SUFFICIENT FOR ALL NXY VALUES.
C
      IF(IPOS.LE.0.OR.IPOS.GT.L3264B)THEN
         IER=7
D        WRITE(KFILDO,101)IPOS,IER
D101     FORMAT(/' IPOS = ',I6,' NOT IN THE RANGE 1 TO L3264B',
D    1           ' IN UNPKLX.  RETURN FROM UNPKLX WITH IER = ',I4)
         GO TO 900 
      ENDIF
C
      IF(NBIT.LT.0.OR.NBIT.GT.32)THEN
         IER=8
D        WRITE(KFILDO,102)NBIT,IER
D102     FORMAT(/' NBIT = ',I6,' NOT IN THE RANGE 0 TO 32 IN UNPKLX.',
D    1           ' RETURN FROM UNPKLX WITH IER = ',I4)
         GO TO 900
      ENDIF
C
      IF(NBIT*NXY.GT.(L3264B+1-IPOS)+(NDX-LOC)*L3264B)THEN
         IER=6
D        WRITE(KFILDO,103)NXY,NBIT,LOC,IPOS,NDX,IER
D103     FORMAT(/' NXY = ',I9,' AND NBIT = ',I6,' REQUIRE MORE BITS',
D    1           ' THAN ARE AVAILABLE IN IPACK( ),',
D    2           ' WITH LOC =',I8,', IPOS =',I4,', AND NDX =',I8,'.'/
D    3           ' RETURN FROM UNPKLX WITH IER =',I4)
         GO TO 900
      ENDIF
C      
      DO 300 K=1,NXY 
         NVALUE=0
C
      IF(NBIT.EQ.0)GO TO 150
C   
      NEWIPOS=IPOS+NBIT
C
      IF(NEWIPOS.LE.L3264B+1)THEN
         CALL MVBITS(IPACK(LOC),L3264B+1-NEWIPOS,NBIT,NVALUE,0)
C
         IF(NEWIPOS.LE.L3264B)THEN
            IPOS=NEWIPOS
         ELSE
            IPOS=1
            LOC=LOC+1
         ENDIF
C
      ELSE
         NBIT1=L3264B+1-IPOS
         NBIT2=NBIT-NBIT1
         CALL MVBITS(IPACK(LOC),0,NBIT1,NVALUE,NBIT2)
         LOC=LOC+1
         CALL MVBITS(IPACK(LOC),L3264B-NBIT2,NBIT2,NVALUE,0)
         IPOS=NBIT2+1
      ENDIF
C
 150  IC(K)=NVALUE
C
 300  CONTINUE
C
 900  IF(IER.NE.0)RETURN 1
C
      RETURN
      END
