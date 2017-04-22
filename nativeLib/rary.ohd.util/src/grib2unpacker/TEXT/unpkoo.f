      SUBROUTINE UNPKOO(KFILDO,IPACK,ND5,LOCN,IPOS,JMIN,LBIT,
     1                  NOV,LX,IWORK,ND2X3,L3264B,IER)
C
C        JUNE 1997   GLAHN   TDL   MOS-2000
C        MAY  1999   GLAHN   CHANGED DIMENSION OF IWORK.
C        FEB  2001   LAWRENCE   UPDATED THIS ROUTINE TO USE
C                               MVBITS INSTEAD OF ISHFT.  THIS
C                               WILL IMPROVE THE UNPACKING
C                               EFFICIENCY OF THIS ROUTINE. 
C
C        PURPOSE
C            UNPACKS DATA IN TDLPACK FORMAT WHEN THERE ARE NO
C            MISSING VALUES.  SCALING IS NOT DONE IN THIS ROUTINE.
C            THIS ROUTINE IS CALLED FROM UNPACK TO ELIMINATE
C            MULTIPLE CALLS TO UNPKBG.  THE WORD POINTER LOCN AND
C            BIT POSITION POINTER IPOS ARE UPDATED AS NECESSARY.
C
C        DATA SET USE
C           KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE. (OUTPUT) 
C
C        VARIABLES 
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C            IPACK(J) = ARRAY TO UNPACK FROM (J=1,ND5).  (INPUT)
C                 ND5 = DIMENSION OF IPACK( ).  (INPUT)
C                LOCN = WORD IN IPACK( ) TO START UNPACKING.  UPDATED
C                       AS NECESSARY AFTER UNPACKING IS COMPLETED.
C                       (INPUT/OUTPUT)
C                IPOS = BIT POSITION (COUNTING LEFTMOST BIT IN WORD
C                       AS 1) TO START UNPACKING.  MUST BE GE 1 AND
C                       LE 32.  UPDATED AS NECESSARY
C                       AFTER PACKING IS COMPLETED.  (INPUT/OUTPUT)
C             JMIN(L) = THE MINIMUM VALUE SUBTRACTED FROM EACH GROUP
C                       L BEFORE PACKING (L=1,LX).  (INPUT)
C             LBIT(L) = THE NUMBER OF BITS NECESSARY TO HOLD THE
C                       PACKED VALUES FOR EACH GROUP L (L=1,LX). 
C                       (INPUT)
C              NOV(L) = THE NUMBER OF VALUES IN GROUP L (L=1,LX).
C                       (INPUT)
C                  LX = THE NUMBER OF VALUES IN LBIT( ), JMIN( ), AND
C                       NOV( ).  ALSO USED AS THEIR DIMENSIONS.
C                       (INPUT)
C            IWORK(J) = THE UNPACKED DATA ARE RETURNED IN THIS ARRAY.
C                       (J=1,MAX OF ND2X3).  (OUTPUT)
C               ND2X3 = DIMENSION OF IWORK( ).  (INPUT)
C              L3264B = INTEGER WORD LENGTH OF MACHINE BEING USED.
C                       (INPUT)
C                 IER = STATUS RETURN:
C                       0 = GOOD RETURN.
C                       6 = NOT ENOUGH ROOM IN IPACK( ) OR IWORK( ) TO
C                           ACCOMMODATE THE DATA INDICATED BY LBIT( )
C                           AND NOV( ).
C                       7 = IPOS NOT IN RANGE 1 TO 32.
C                       8 = LBIT(L) NOT IN RANGE 0 TO 30.
C
C        LOCAL VARIABLES
C                   K = USED TO INDEX THE IWORK( ) ARRAY
C                       WHILE KEEPING TRACK OF THE CURRENT GROUP
C                       OF DATA BEING PROCESSED.
C                 L,M = LOOP INDEX VARIABLES.
C NEWIPOS,NBIT1,NBIT2 = VARIABLES USED IN THE MANIPULATION OF
C                       BIT POSITIONS WHILE UNPACKING VALUES FROM
C                       IPACK().
C
C        NON SYSTEM SUBROUTINES CALLED
C            NONE (MVBITS IS A SYSTEM INTRINSIC)
C
      DIMENSION IPACK(ND5)
      DIMENSION IWORK(ND2X3)
      DIMENSION JMIN(LX),LBIT(LX),NOV(LX)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2unpacker/RCS/unpkoo.f,v $
     . $',                                                             '
     .$Id: unpkoo.f,v 1.1 2004/09/16 16:51:50 dsa Exp $
     . $' /
C    ===================================================================
C
C
C        CHECK CORRECTNESS OF INPUT AND SET STATUS RETURN.
C
      IER=0
C
      IF(IPOS.LE.0.OR.IPOS.GT.L3264B)THEN
         IER=7
D        WRITE(KFILDO,101)IPOS,IER
D101     FORMAT(/' IPOS = ',I6,' NOT IN THE RANGE 1 TO L3264B.',
D    1           '  RETURN FROM UNPKOO WITH IER = ',I4)
         GO TO 900 
      ENDIF
C
      K=0
C
      DO 350 L=1,LX
      IF(LBIT(L).LT.0.OR.LBIT(L).GT.30)THEN
         IER=8
D        WRITE(KFILDO,102)LBIT(L),L,IER
D102     FORMAT(/' ****LBIT(L) = ',I6,' FOR L =',I6,' NOT IN THE RANGE',
D    1           ' 0 TO 30.  RETURN FROM UNPKOO WITH IER = ',I4)
         GO TO 900
      ENDIF
C
      IF(LBIT(L)*NOV(L).GT.(L3264B+1-IPOS)+(ND5-LOCN)*L3264B)THEN
         IER=6
D        WRITE(KFILDO,103)NOV(L),LBIT(L),L,LOCN,IPOS,ND5,IER
D103     FORMAT(/,' ****NOV(L) = ',I9,' AND LBIT(L) = ',I6,' FOR L =',I6
D    1        ,' REQUIRE MORE BITS THAN ARE AVAILABLE IN IPACK( ),',/,
D    2        ' WITH LOCN =',I8,', IPOS =',I4,', AND ND5 =',I8,'.',/,
D    3        ' RETURN FROM UNPKOO WITH IER =',I4)
         GO TO 900
      ENDIF
C
      IF(NOV(L)+K.GT.ND2X3)THEN
         IER=6
D        WRITE(KFILDO,104)NOV(L),L,ND2X3,IER
D104     FORMAT(/' ****NOV(L) = ',I9,' FOR L =',I6,
D    1          ' INDICATES MORE VALUES THAN CAN BE PUT INTO IWORK( ),'/
D    2          '     WITH ND2X3 =',I8,'.',
D    3          '  RETURN FROM UNPKOO WITH IER =',I4)
         GO TO 900
      ENDIF
C
C        TEST FOR LBIT(L) = 0 OUT OF LOOP.
      IF(LBIT(L).NE.0)GO TO 330
C
      DO 249 M=1,NOV(L)
         IWORK(K+M)=JMIN(L)
 249  CONTINUE
C
      K=K+NOV(L)
      GO TO 350
C
 330  DO 355 M=1,NOV(L)
C
C        TRANSFER LBIT(L) BITS FROM IPACK( ) TO NVALUE.
C        WORD BOUNDARIES MUST BE TAKEN INTO ACCOUNT. 
C
         NVALUE=0
         NEWIPOS=IPOS+LBIT(L)
C
         IF(NEWIPOS.LE.L3264B+1)THEN
            CALL MVBITS(IPACK(LOCN),L3264B+1-NEWIPOS,LBIT(L),
     1                  NVALUE,0)
C
            IF(NEWIPOS.LE.L3264B)THEN
               IPOS=NEWIPOS
            ELSE
               IPOS=1
               LOCN=LOCN+1
            ENDIF
C
         ELSE
            NBIT1=L3264B+1-IPOS
            NBIT2=LBIT(L)-NBIT1
            CALL MVBITS(IPACK(LOCN),0,NBIT1,NVALUE,NBIT2)
            LOCN=LOCN+1
            CALL MVBITS(IPACK(LOCN),L3264B-NBIT2,NBIT2,NVALUE,0)
            IPOS=NBIT2+1
         ENDIF
C  
         IWORK(K+M)=JMIN(L)+NVALUE
C
 355  CONTINUE
C
      K=K+NOV(L)
 350  CONTINUE
C
 900  RETURN
      END
