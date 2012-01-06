      SUBROUTINE UNPKPS(KFILDO,IPACK,ND5,LOCN,IPOS,MISSP,MISSS,
     1                  JMIN,LBIT,NOV,LX,IWORK,ND2X3,L3264B,IER)
C
C        JUNE 1997   GLAHN   TDL   MOS-2000
C        MAY  1999   GLAHN   CHANGED DIMENSION OF IWORK.
C        FEB  2001   LAWRENCE   UPDATED THIS ROUTINE TO USE
C                               MVBITS INSTEAD OF ISHFT.  THIS
C                               WILL IMPROVE THE UNPACKING
C                               EFFICIENCY OF THIS ROUTINE.
C
C        PURPOSE 
C            UNPACKS DATA IN TDLPACK FORMAT WHEN THERE CAN BE PRIMARY
C            AND SECONDARY MISSING VALUES.  SCALING IS NOT DONE IN THIS
C            ROUTINE. BUT THE REFERENCE VALUE IS USED.  CALLED FROM
C            UNPACK TO ELIMINATE MULTIPLE CALLS TO UNPKBG.  THE WORD
C            POINTER LOCN AND BIT POSITION POINTER IPOS ARE UPDATED
C            AS NECESSARY.
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
C                       LE L3264B.  UPDATED AS NECESSARY
C                       AFTER PACKING IS COMPLETED.  (INPUT/OUTPUT)
C               MISSP = THE PRIMARY MISSING VALUE TO RETURN IN IWORK( )
C                       WHEN THE PACKED DATUM INDICATES A PRIMARY
C                       MISSING VALUE.  (INPUT)
C               MISSS = THE SECONDARY MISSING VALUE TO RETURN IN
C                       IWORK( ) WHEN THE PACKED DATUM INDICATES
C                       A SECONDARY MISSING VALUE.  (INPUT)
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
C            IWORK(J) = THE UNPACKED DATA ARE RETURNED
C                       (J=1,MAX OF ND2X3).  (OUTPUT)
C               ND2X3 = DIMENSION OF IWORK( ).  (INPUT)
C              L3264B = INTEGER WORD LENGTH OF MACHINE BEING USED.
C                       (INPUT)
C                 IER = STATUS RETURN:
C                       0 = GOOD RETURN.
C                       6 = NOT ENOUGH ROOM IN IPACK( ) OR IWORK( ) TO
C                           ACCOMMODATE THE DATA INDICATED BY LBIT( )
C                           AND NOV( ).
C                       7 = IPOS NOT IN RANGE 1 TO L3264B.
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
      DIMENSION LB2M1(0:30),LB2M2(0:30)
C
      SAVE LB2M1,LB2M2
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2unpacker/RCS/unpkps.f,v $
     . $',                                                             '
     .$Id: unpkps.f,v 1.1 2004/09/16 16:51:50 dsa Exp $
     . $' /
C    ===================================================================
C
C
      DATA IFIRST/0/
C
C         CALCULATE THE POWERS OF 2 THE FIRST TIME ENTERED.
C
      IF(IFIRST.EQ.0)THEN
         IFIRST=1
         LB2M1(0)=0
         LB2M2(0)=-1
C
         DO 100 J=1,30
         LB2M1(J)=(LB2M1(J-1)+1)*2-1
         LB2M2(J)=(LB2M2(J-1)+2)*2-2
 100     CONTINUE
C
      ENDIF
C
C        CHECK CORRECTNESS OF INPUT AND SET STATUS RETURN.
C
      IER=0
C
      IF(IPOS.LE.0.OR.IPOS.GT.L3264B)THEN
         IER=7
D        WRITE(KFILDO,101)IPOS,IER
D101     FORMAT(/' IPOS = ',I6,' NOT IN THE RANGE 1 TO L3264B.',
D    1           '  RETURN FROM UNPKPS WITH IER = ',I4)
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
D    1           ' 0 TO 30.  RETURN FROM UNPKPS WITH IER = ',I4)
         GO TO 900
      ENDIF
C
      IF(LBIT(L)*NOV(L).GT.(L3264B+1-IPOS)+(ND5-LOCN)*L3264B)THEN
         IER=6
D        WRITE(KFILDO,103)NOV(L),LBIT(L),L,LOCN,IPOS,ND5,IER
D103     FORMAT(/' ****NOV(L) = ',I9,' AND LBIT(L) = ',I6,' FOR L =',I6,
D    1         ' REQUIRE MORE BITS THAN ARE AVAILABLE IN IPACK( ),',/,
D    2         '     WITH LOCN =',I8,', IPOS =',I4,', AND ND5 =',I8,'.',
D    3         '  RETURN FROM UNPKPS WITH IER =',I4)
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
      MISSPK=LB2M1(LBIT(L))
      MISSSK=LB2M2(LBIT(L))
C
C        NOTE THAT IT IS NOT NECESSARY TO TEST FOR LBIT(L) EQ 0,
C        AS IT IS IN UNPKOO AND UNPKPO, BECAUSE THIS CANNOT BE
C        TRUE WHEN THERE CAN BE SECONDARY MISSING VALUES.
C
      DO 355 M=1,NOV(L)
C
         K=K+1
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
         IF(NVALUE.EQ.MISSPK)THEN
            IWORK(K)=MISSP
         ELSEIF(NVALUE.EQ.MISSSK)THEN
            IWORK(K)=MISSS
         ELSE
            IWORK(K)=JMIN(L)+NVALUE
C
            IF(IWORK(K).EQ.MISSP)THEN
               IWORK(K)=IWORK(K)-1
            ELSEIF(IWORK(K).EQ.MISSS)THEN
               IWORK(K)=IWORK(K)-1
            ENDIF
C              THE ABOVE STATEMENTS ARE NECESSARY TO GUARD AGAINST A 
C              LEGITIMATE VALUE BEING INTERPRETED AS A MISSING.
C              MISSS MUST NOT BE EXACTLY ONE LESS THAN MISSP.
C
         ENDIF
C
 355  CONTINUE
C
 350  CONTINUE
C
 900  RETURN
      END
