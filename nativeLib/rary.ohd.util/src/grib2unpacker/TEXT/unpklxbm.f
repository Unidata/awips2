      SUBROUTINE UNPKLXBM(KFILDO,IPACK,NDX,LOCN,IPOS,A,IBMP,NXY,NBIT,
     1                    FMISS,REF,IBITMAP,ISCAL,SCAL10,SCAL2,CLEAN,
     2                    L3264B,IER)
C
C        APRIL    1997   GLAHN    TDL   HP
C        MAY      1997   GLAHN    MODIFIED UNPACKING ALGORITHM TO USE
C                                 MVBITS RATHER THAN SHIFTING AND ORING.
C        JULY     1999   LAWRENCE UPDATED THIS ROUTINE TO CONSIDER A BITMAP
C                                 (IF ONE IS PRESENT) AND TO PERFORM THE
C                                 THE NECESSARY SCALINGS AND COMPUTATIONS
C                                 INTERNALLY. THIS HAS BEEN DONE IN ATTEMPT
C                                 TO SPEED UP THE SIMPLE UNPACKING
C                                 PROCEDURE.
C        JANUARY  2001   GLAHN    COMMENTS, CHANGED IER = 6 TO 9; ADDED
C                                 IF THEN TO HANDLE ZERO VALUE EFFICIENTLY;
C                                 ADDED ISCAL
C        FEBRUARY 2001   GLAHN    CHANGED LOC TO LOCN; INSERTED SELECT
C                                 CASE (ISCAL) IN 2 PLACES
C        MARCH    2001   LAWRENCE FIXED AN ERROR THAT WAS CAUSING THE DATA
C                                 FIELD TO BE IMPROPERLY CONSTRUCTED WHEN 
C                                 THE CLEAN ARGUMENT IS .TRUE.
C        FEBRUARY 2002   GLAHN    FIXED ERROR WHEN NBIT=0; ELIMINATED 
C                                 STATEMENT 160 
C
C        PURPOSE
C            UNPACKS NXY VALUES FROM IPACK( ), USING THE SIMPLE METHOD.
C            THE PACKED VALUES ARE RETURNED IN IC( ) WITH REFERENCE 
C            VALUE AND SCALING FACTORS CONSIDERED.  UNPKLXBM ELIMINATES
C            MULTIPLE CALLS TO UNPKBG, AND RATHER INCORPORATES IT
C            INTO THE LOOP.  SINCE THIS IS A HIGHLY USED ROUTINE, ALL
C            REASONABLE ATTEMPTS AT EFFICIENCY MUST BE PURSUED.
C
C        DATA SET USE 
C           KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE. (OUTPUT) 
C
C        VARIABLES 
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT) 
C            IPACK(J) = THE ARRAY HOLDING THE ACTUAL PACKED MESSAGE
C                       (J=1,MAX OF NDX).  (INPUT)
C                 NDX = DIMENSION OF IPACK( ).  (INPUT)
C                LOCN = THE WORD POSITION FROM WHICH TO UNPACK THE
C                       NEXT VALUE. (INPUT/OUTPUT)
C                IPOS = THE BIT POSITION IN LOCN FROM WHICH TO START
C                       UNPACKING THE NEXT VALUE.  (INPUT/OUTPUT)
C                A(K) = UNPACKED DATA RETURNED (K=1,NXY).  (OUTPUT)
C             IBMP(K) = CONTAINS A BITMAP CORRESPONDING TO
C                       TO THE DATA TO BE UNPACKED INTO A() (K=1,NXY).
C                       (INPUT)
C                 NXY = DIMENSION OF IC( ).  THE NUMBER
C                       OF UNPACKED VALUES RETURNED.  (INPUT)
C                NBIT = THE NUMBER OF BITS TO UNPACK FOR EACH VALUE.
C                       (INPUT)
C               FMISS = VALUE TO INSERT INTO A( ) AT POINTS WHERE
C                       THE BIT MAP INDICATES VALUES ARE MISSING.
C                       THIS IS NORMALLY XMISSP (FOR SECTON 7) OR
C                       ZERO (FOR SECTON 2).  (INPUT)
C                 REF = THE REFERENCE VALUE.  (INPUT)
C             IBITMAP = 1 IF THERE WAS A BIT-MAP PACKED INTO
C                       THIS GRIB2 MESSAGE.
C                       0 IF THERE WAS NOT A BIT-MAP IN THIS
C                       GRIB2 MESSAGE. (LOGICAL) (INPUT)
C               ISCAL = INDICATES COMBINATIONS OF SCALING.
C                       0 = NONE
C                       1 = DECIMAL ONLY
C                       2 = BINARY ONLY
C                       3 = BOTH DECIMAL AND BINARY
C              SCAL10 = DECIMAL SCALING PARAMETER. (INPUT)
C               SCAL2 = BINARY SCALING PARAMETER. (INPUT)
C               CLEAN = A LOGICAL FLAG THAT ONLY APPLIES IF THE
C                       SIMPLE PACKING METHOD IS BEING USED IN
C                       CONJUNCTION WITH A BIT-MAP.
C                       TRUE: KEEP THE DATA IN A() "CLEAN". DO NOT
C                             PUT MISSING VALUES IN A(). IT WILL
C                             BE UP TO THE USER USING THE BIT-MAP
C                             TO KEEP TRACK OF THE LOCATIONS OF THE
C                             MISSING VALUES.
C                       FALSE: PUT THE MISSING VALUES INTO THEIR
C                              PROPER PLACES IN THE A() ARRAY.
C                       (INPUT)
C              L3264B = INTEGER WORD LENGTH OF MACHINE BEING USED.
C                       (INPUT)
C                 IER = ERROR RETURN.  (OUTPUT)
C                       7 = IPOS NOT IN THE RANGE 1-L3264B.
C                       8 = IBIT NOT IN THE RANGE 0-32.
C                       9 = NDX IS NOT LARGE ENOUGH TO FURNISH THE BITS
C                           NECESSARY TO ACCOMMODATE NXY VALUES STARTING
C                           AT THE VALUES LOCN AND IPOS.
C
C        LOCAL VARIABLES
C                   K = A LOOP INDEXING VARIABLE. 
C NEWIPOS,NBIT1,NBIT2 = VARIABLES USED IN THE MANIPULATION OF 
C                       BIT POSITIONS WHILE UNPACKING VALUES FROM
C                       IPACK(). 
C              NVALUE = THE UNPACKED VALUE.
C             OSCAL10 = THE RECIPROCAL OF THE SCALING FACTOR, SCAL10.
C
C        NON SYSTEM SUBROUTINES CALLED 
C           NONE
C
      LOGICAL CLEAN
C
      DIMENSION IPACK(NDX)
      DIMENSION A(NXY)
      DIMENSION IBMP(NXY)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2unpacker/RCS/unpklxbm.f,v $
     . $',                                                             '
     .$Id: unpklxbm.f,v 1.1 2004/09/16 16:51:50 dsa Exp $
     . $' /
C    ===================================================================
C
C
C        SET ERROR RETURN AND ZERO ARRAYS.
C
      IER=0
C
C        CHECK LEGITIMATE VALUES OF LOCN AND IPOS, AND WHETHER
C        NDX IS SUFFICIENT FOR ALL NXY VALUES.
C
      IF(IPOS.LE.0.OR.IPOS.GT.L3264B)THEN
         IER=7
         GO TO 900 
      ENDIF
C
      IF(NBIT.LT.0.OR.NBIT.GT.32)THEN
         IER=8
         GO TO 900
      ENDIF
C
      IF(NBIT*NXY.GT.(L3264B+1-IPOS)+(NDX-LOCN)*L3264B)THEN
         IER=9
         GO TO 900
      ENDIF
C
C        COMPUTE RECIPROCAL OF SCAL10 SO THAT MULTIPLICATION
C        CAN BE USED IN SCALING RATHER THAN DIVISION.
C
      OSCAL10=1./SCAL10
C
C        TEST FOR THE EXISTENCE OF A BITMAP HERE.
C    
      IF(IBITMAP.EQ.0)THEN
C
C           A BIT MAP IS NOT PRESENT.
C
         DO 300 K=1,NXY 
            NVALUE=0
C
            IF(NBIT.EQ.0)THEN
C
               SELECT CASE (ISCAL)
                  CASE(0)
                     A(K)=REF
                  CASE(1)
                     A(K)=REF*OSCAL10
                  CASE(2)
                     A(K)=REF
                  CASE(3)
                     A(K)=REF*OSCAL10
               END SELECT
C
            ELSE
C   
               NEWIPOS=IPOS+NBIT
C
               IF(NEWIPOS.LE.L3264B+1)THEN
                  CALL MVBITS(IPACK(LOCN),L3264B+1-NEWIPOS,NBIT,
     1                        NVALUE,0)
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
                  NBIT2=NBIT-NBIT1
                  CALL MVBITS(IPACK(LOCN),0,NBIT1,NVALUE,NBIT2)
                  LOCN=LOCN+1
                  CALL MVBITS(IPACK(LOCN),L3264B-NBIT2,NBIT2,NVALUE,0)
                  IPOS=NBIT2+1
               ENDIF
C
               SELECT CASE (ISCAL)
                  CASE(0)
                     A(K)=NVALUE+REF
                  CASE(1)
                     A(K)=(NVALUE+REF)*OSCAL10
                  CASE(2)
                     A(K)=NVALUE*SCAL2+REF
                  CASE(3)
                     A(K)=(NVALUE*SCAL2+REF)*OSCAL10
               END SELECT
C
            ENDIF
C
 300     CONTINUE
C
      ELSE
C
C        A BIT MAP IS PRESENT.
C
         K=1
         J=1
C
         DO WHILE(K.LE.NXY)
            NVALUE=0
C
            IF(IBMP(K).EQ.0)THEN
C
               IF(.NOT.CLEAN)THEN
                  A(J)=FMISS
                  J=J+1
               ENDIF
C
               K=K+1
               CYCLE
            ENDIF
C
            IF(NBIT.EQ.0)THEN
C
               SELECT CASE (ISCAL)
                  CASE(0)
                     A(J)=REF
                  CASE(1)
                     A(J)=REF*OSCAL10
                  CASE(2)
                     A(J)=REF
                  CASE(3)
                     A(J)=REF*OSCAL10
               END SELECT
C
               J=J+1
               K=K+1
            ELSE
               NEWIPOS=IPOS+NBIT
C
               IF(NEWIPOS.LE.L3264B+1)THEN
                  CALL MVBITS(IPACK(LOCN),L3264B+1-NEWIPOS,NBIT,
     1                        NVALUE,0)
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
                  NBIT2=NBIT-NBIT1
                  CALL MVBITS(IPACK(LOCN),0,NBIT1,NVALUE,NBIT2)
                  LOCN=LOCN+1
                  CALL MVBITS(IPACK(LOCN),L3264B-NBIT2,NBIT2,NVALUE,0)
                  IPOS=NBIT2+1
               ENDIF
C
C
               SELECT CASE (ISCAL)
                  CASE(0)
                     A(J)=NVALUE+REF
                  CASE(1)
                     A(J)=(NVALUE+REF)*OSCAL10
                  CASE(2)
                     A(J)=NVALUE*SCAL2+REF
                  CASE(3)
                     A(J)=(NVALUE*SCAL2+REF)*OSCAL10
               END SELECT
C
               J=J+1
               K=K+1
            ENDIF
C
         END DO
C
      ENDIF
C
 900  RETURN
      END
