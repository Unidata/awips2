      SUBROUTINE UNPKPO(KFILDO,IPACK,ND5,LOCN,IPOS,MISSP,JMIN,
     1                  LBIT,NOV,LX,MAXGPREF,IWORK,ND2X3,L3264B,IER)
C
C        JUNE     1997   GLAHN   TDL   MOS-2000
C        MAY      1999   GLAHN   CHANGED DIMENSION OF IWORK.
C        FEBRUARY 2001   LAWRENCE   UPDATED THIS ROUTINE TO USE
C                                MVBITS INSTEAD OF ISHFT.  THIS
C                                WILL IMPROVE THE UNPACKING
C                                EFFICIENCY OF THIS ROUTINE.
C        NOVEMBER 2001   GLAHN   MODIFIED TO HANDLE PRIMARY MISSINGS
C                                PROPERLY WHEN A GROUP IS ALL OF ONE
C                                VALUE AND MISSING.  ADDED MAXGPREF TO
C                                CALL.  CHANGED ND5 TO ND2X3 IN TEST
C                                ON NOVL( )+K  ABOVE 104.
C
C        PURPOSE
C            UNPACKS DATA IN TDLPACK FORMAT WHEN THERE CAN BE PRIMARY
C            MISSING VALUES.  SCALING IS NOT DONE IN THIS ROUTINE.
C            CALLED FROM UNPACK TO ELIMINATE MULTIPLE CALLS TO UNPKBG.
C            THE WORD POINTER LOCN AND BIT POSITION POINTER IPOS ARE
C            UPDATED AS NECESSARY.
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
C                       (INPUT-OUTPUT)
C                IPOS = BIT POSITION (COUNTING LEFTMOST BIT IN WORD
C                       AS 1) TO START UNPACKING.  MUST BE GE 1 AND
C                       LE L3264B.  UPDATED AS NECESSARY
C                       AFTER PACKING IS COMPLETED.  (INPUT-OUTPUT)
C               MISSP = THE PRIMARY MISSING VALUE TO RETURN IN IWORK( )
C                       WHEN THE PACKED DATA INDICATE A MISSING VALUE.
C                       (INPUT)
C             JMIN(L) = THE MINIMUM VALUE SUBTRACTED FROM EACH GROUP
C                       L BEFORE PACKING (L=1,LX).  (INPUT)
C             LBIT(L) = THE NUMBER OF BITS NECESSARY TO HOLD THE
C                       PACKED VALUES FOR EACH GROUP L (L=1,LX). 
C                       (INPUT)
C              NOV(L) = THE NUMBER OF VALUES IN GROUP L (L=1,LX).
C                       (INPUT)
C                  LX = THE NUMBER OF VALUES IN LBIT( ), JMIN( ), AND
C                       NOV( ).  ALSO USED AS THEIR DIMENSIONS.
C                      (INPUT)
C            MAXGPREF = THE MAXIMUM VALUE THAT CAN BE STORED IN THE
C                       GROUP REFERENCES.  (INPUT)
C            IWORK(J) = THE UNPACKED DATA ARE RETURNED
C                       (J=1,MAX OF ND2X3).  (OUTPUT)
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
      DIMENSION LB2M1(0:30)
C
      SAVE LB2M1
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2unpacker/RCS/unpkpo.f,v $
     . $',                                                             '
     .$Id: unpkpo.f,v 1.1 2004/09/16 16:51:50 dsa Exp $
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
C
         DO 100 J=1,30
         LB2M1(J)=(LB2M1(J-1)+1)*2-1
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
         WRITE(KFILDO,101)IPOS,IER
 101     FORMAT(/' IPOS = ',I6,' NOT IN THE RANGE 1 TO L3264B.',
     1           '  RETURN FROM UNPKPO WITH IER = ',I4)
         GO TO 900 
      ENDIF
C
      K=0
C
      DO 350 L=1,LX
      IF(LBIT(L).LT.0.OR.LBIT(L).GT.30)THEN
         IER=8
         WRITE(KFILDO,102)LBIT(L),L,IER
 102     FORMAT(/' ****LBIT(L) = ',I6,' FOR L =',I6,' NOT IN THE RANGE',
     1           ' 0 TO 30.  RETURN FROM UNPKPO WITH IER = ',I4)
         GO TO 900
      ENDIF
C
      IF(LBIT(L)*NOV(L).GT.(L3264B+1-IPOS)+(ND5-LOCN)*L3264B)THEN
         IER=6
         WRITE(KFILDO,103)NOV(L),LBIT(L),L,LOCN,IPOS,ND5,IER
 103     FORMAT(/' ****NOV(L) = ',I9,' AND LBIT(L) = ',I6,' FOR L =',I6,
     1         ' REQUIRE MORE BITS THAN ARE AVAILABLE IN IPACK( ),',/,
     2         '     WITH LOCN =',I8,', IPOS =',I4,', AND ND5 =',I8,'.',
     3         '  RETURN FROM UNPKPO WITH IER =',I4)
         GO TO 900
      ENDIF
C
      IF(NOV(L)+K.GT.ND2X3)THEN
         IER=6
         WRITE(KFILDO,104)NOV(L),L,ND2X3,IER
 104     FORMAT(/' ****NOV(L) = ',I9,' FOR L =',I6,
     1       ' INDICATES MORE VALUES THAN CAN BE PUT INTO IWORK( ),',/,
     2       '     WITH ND2X3 =',I8,'.',/,
     3       '  RETURN FROM UNPKPO WITH IER =',I4)
         GO TO 900
      ENDIF
C
C        TEST FOR LBIT(L) = 0 OUT OF LOOP.
C
      IF(LBIT(L).NE.0)GO TO 330
C
      DO 249 M=1,NOV(L)
C
         K=K+1
C
         IF(JMIN(L).EQ.MAXGPREF)THEN
            IWORK(K)=MISSP
         ELSE
            IWORK(K)=JMIN(L)
         ENDIF
C
 249  CONTINUE
C
      GO TO 350
C
C        NORMAL PROCESSING WHEN LBIT(L) NE 0.
C
 330  MISSPK=LB2M1(LBIT(L))
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
         ELSE
            IWORK(K)=JMIN(L)+NVALUE
C
            IF(IWORK(K).EQ.MISSP)THEN
               IWORK(K)=IWORK(K)-1
            ENDIF
C              THE ABOVE STATEMENTS ARE NECESSARY TO GUARD AGAINST A 
C              LEGITIMATE VALUE BEING INTERPRETED AS A MISSING.
C
         ENDIF
C
 355  CONTINUE
C
 350  CONTINUE
C
 900  RETURN
      END
