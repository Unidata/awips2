      SUBROUTINE UNPKCMBM(KFILDO,IPACK,NDX,LOC,IPOS,A,IBMP,NXY,LBIT,
     1                    JMIN,NOV,LX,MAXGPREF,IMISSING,XMISSP,XMISSS,
     2                    REF,MAPBIT,SCAL10,SCAL2,CLEAN,L3264B,IER)
C
C        APRIL   1997   GLAHN    TDL   HP
C        MAY     1997   GLAHN    MODIFIED UNPACKING ALGORITHM TO USE
C                                MVBITS RATHER THAN SHIFTING AND ORING.
C        JULY    1999   LAWRENCE UPDATED THIS ROUTINE TO CONSIDER A BITMAP
C                                (IF ONE IS PRESENT) AND TO PERFORM THE
C                                THE NECESSARY SCALINGS AND COMPUTATIONS
C                                INTERNALLY. THIS HAS BEEN DONE IN ATTEMPT
C                                TO SPEED UP THE COMPLEX UNPACKING
C                                PROCEDURE.
C        JULY    1999   LAWRENCE FIXED AN ELUSIVE BUG...
C        APRIL   2000   LAWRENCE UPDATED THIS ROUTINE TO MEET THE NEEDS
C                                OF THE LATEST GRIB2 DOCUMENTATION.
C                                NOTE THAT THE SECONDARY BIT-MAP HAS
C                                BEEN DONE AWAY WITH.
C        NOVEMBER 2001   GLAHN   MODIFIED TO HANDLE BOTH PRIMARY AND
C                                SECONDARY MISSINGS PROPERLY WHEN A
C                                GROUP IS ALL OF ONE VALUE AND MISSING.
C                                ADDED IBXX2( ).  CHANGED UNDEFINED 
C                                FMISS TO XMISSP IN TWO PLACES
C                                REGARDING BIT MAP. ADDED MAXGPREF TO
C                                CALL
C        JANUARY  2001   GLAHN   ELIMINATED NVALUE WHEN NO MISSINGS AND
C                                ALL VALUES THE SAME
C                                
C
C        PURPOSE 
C            TO UNPACK NXY VALUES FROM IPACK( ) USING THE COMPLEX
C            UNPACKING METHOD. THE PACKED VALUES ARE RETURNED
C            IN A( ) WITH REFERENCE VALUE AND SCALING
C            FACTORS CONSIDERED.  UNPKCMBM ELIMINATES THE 
C            MULTIPLE CALLS TO UNPKBG, AND RATHER INCORPORATES IT
C            INTO THE LOOP.  SINCE THIS IS A HIGHLY USED ROUTINE, ALL 
C            REASONABLE ATTEMPTS AT EFFICIENCY MUST BE PURSUED.
C        NOTE:  BECAUSE FLOATING POINT COMPUTATIONS ARE USED, AN
C            EXACT INTEGER VALUE MAY NOT BE RETURNED WHEN VERY LARGE.
C            FOR INSTANCE, AN INTEGER VALUE OF 2**30-1 WILL BE
C            RETURNED 2**30 REAL (INCORRECT BY ONE DIGIT).  THIS IS
C            ALSO TRUE OF 2**25-1, BUT 2**24-1 IS OK.
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
C                A(K) = UNPACKED DATA RETURNED (K=1,NXY).  (OUTPUT)
C             IBMP(K) = CONTAINS A BITMAP CORRESPONDING TO
C                       TO THE DATA TO BE UNPACKED INTO A() (K=1,NXY).
C                       (INPUT)
C                 NXY = DIMENSION OF IC( ).  THE NUMBER
C                       OF UNPACKED VALUES RETURNED.  (INPUT)
C             LBIT(K) = THE NUMBER OF BITS TO UNPACK FOR EACH VALUE
C                       (K=1,LX). (INPUT)
C             JMIN(K) = THE SECONDARY MINIMA OF THE GROUPS (K=1,LX)
C                       (INPUT) 
C              NOV(K) = THE NUMBER OF VALUES PER EACH GROUP (K=1,LX).
C                       (INPUT)
C                  LX = THE NUMBER OF GROUPS IN THIS PRODUCT. (INPUT)
C            MAXGPREF = THE MAXIMUM VALUE THAT CAN BE STORED IN THE
C                       GROUP REFERENCES.  (INPUT)
C            IMISSING = REPRESENTS OCTET 23 OF SECTION 5. DETERMINES
C                       IF THERE ARE NO MISSING VALUES, PRIMARY MISSING
C                       VALUES, OR PRIMARY AND SECONDARY MISSING VALUES.
C                       (INPUT)
C              XMISSP = VALUE TO INSERT INTO A( ) AT POINTS WHERE
C                       THERE IS A PRIMARY MISSING VALUE.  (INPUT)
C              XMISSS = VALUE TO INSERT INTO A( ) AT POINTS WHERE
C                       THERE IS A SECONDARY MISSING VALUE.  (INPUT)
C                 REF = THE REFERENCE VALUE THAT WE WILL BE USING
C                       TO UNPACK THE DATA. (INPUT)
C              MAPBIT = A FLAG INDICATING IF A BIT-MAP IS PRESENT.
C                       0 WHEN A BIT-MAP IS PRESENT; 1 OTHERWISE.
C                       (INPUT)
C              SCAL10 = DECIMAL SCALING PARAMETER. (INPUT)
C               SCAL2 = BINARY SCALING PARAMETER. (INPUT)
C               CLEAN = .TRUE. IF THERE ARE NO MISSING
C                       VALUES IN THE DATA FIELD.
C                       .FALSE. IF THERE ARE MISSING
C                       VALUES IN THE DATA FIELD (LOGICAL). (INPUT)
C              L3264B = INTEGER WORD LENGTH OF MACHINE BEING USED.
C                       (INPUT)
C                 IER = ERROR RETURN.  (OUTPUT)
C                       6 = NDX IS NOT LARGE ENOUGH TO FURNISH THE BITS
C                           NECESSARY TO ACCOMMODATE NXY VALUES STARTING
C                           AT THE VALUES LOC AND IPOS.
C                       7 = IPOS NOT IN THE RANGE 1-L3264B.
C                       8 = IBIT NOT IN THE RANGE 0-32.
C                      18 = UNRECOGNIZED MISSING VALUE CODE IN
C                           IMISSING.
C
C        LOCAL VARIABLES
C                   K = A LOOP INDEXING VARIABLE.
C                   L = KEEPS TRACK OF THE CURRENT GROUP THAT
C                       WE ARE PROCESSING.
C                   M = KEEPS TRACK OF OUR POSITION IN THE SECONDARY
C                       BIT-MAP DURING THE UNPACK.
C                MISS = CONTAINS THE VALUE OF 2**LBIT(K)-1. WHEN
C                       THERE CAN BE MISSING VALUES, THE MISSING
C                       VALUE IS STORED AS THE LARGEST POSSIBLE
C                       NUMBER STOREABLE IN THE NUMBER OF BITS
C                       NECESSARY TO CONTAIN ALL THE VALUES IN 
C                       THE GROUP.
C NEWIPOS,NBIT1,NBIT2 = VARIABLES USED IN MANIPULATING BIT POSITIONS
C                       DURING THE UNPACKING OF VALUES FROM
C                       IPACK(). 
C              NVALUE = CONTAINS THE VALUE RETRIEVED FROM IPACK().
C                       THIS IS THE VALUE BEFORE WE MULTIPLY
C                       BY THE DECIMAL AND BINARY SCALE FACTORS
C                       AND ADD ON THE 1ST AND SECOND ORDER
C                       MINIMA.
C            IBXX2(J) = AN ARRAY THAT WHEN THIS ROUTINE IS FIRST ENTERED
C                       IS SET TO 2**J, J=0,30. IBXX2(30) = 2**30, WHICH
C                       IS THE LARGEST VALUE PACKABLE, BECAUSE 2**31
C                       IS LARGER THAN THE INTEGER WORD SIZE.
C
C        NON SYSTEM SUBROUTINES CALLED 
C            NONE (MVBITS IS A BITS INTRINSIC)
C
      LOGICAL CLEAN
C
      DIMENSION IPACK(NDX)
      DIMENSION A(NXY)
      DIMENSION IBMP(NXY)
      DIMENSION JMIN(LX),LBIT(LX),NOV(LX)
      DIMENSION IBXX2(0:30)
C
      DATA IFIRST/0/
C
      SAVE IBXX2
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2unpacker/RCS/unpkcmbm.f,v $
     . $',                                                             '
     .$Id: unpkcmbm.f,v 1.1 2004/09/16 16:51:50 dsa Exp $
     . $' /
C    ===================================================================
C
C
C        SET ERROR RETURN AND ZERO ARRAYS.
      IER=0
C
C         CALCULATE THE POWERS OF 2 THE FIRST TIME ENTERED.
C
      IF(IFIRST.EQ.0)THEN
         IFIRST=1
         IBXX2(0)=1
C
         DO 104 J=1,30
         IBXX2(J)=IBXX2(J-1)*2
 104     CONTINUE
C
      ENDIF
C
C        CHECK LEGITIMATE VALUE OF LOC.
C
      IF(IPOS.LE.0.OR.IPOS.GT.L3264B)THEN
         IER=7
D        WRITE(KFILDO,105)IPOS,IER
D105     FORMAT(/' IPOS = ',I6,' NOT IN THE RANGE 1 TO L3264B',
D    1           ' IN UNPKLX.  RETURN FROM UNPKLX WITH IER = ',I4)
         GO TO 900
      ENDIF
C
C       TEST FOR THE EXISTENCE OF A BITMAP HERE...
C
      M=0
C
      IF(MAPBIT.EQ.0)THEN
C
C        A BIT MAP IS NOT PRESENT.
C
         DO K=1,LX
C
C              CHECK TO SEE IF THE NUMBER OF BITS
C              TO UNPACK FOR EACH VALUE IS 0.  IF IT
C              IS 0, THEN DETERMINE IF THE FIELD CAN
C              HAVE MISSING VALUES.  IF IT CAN,
C              THEN A VALUE OF ALL 1'S IN THE FIELD MEANS
C              A PRIMARY MISSING VALUE, AND A VALUE OF ALL
C              1'S MINUE 1 MEANS A SECONDARY MISSING VALUE.
C              OTHERWISE, ALL OF THE VALUES ARE VALID AND
C              THE SAME AS EACH OTHER.
C
            IF(LBIT(K).EQ.0)THEN
C
               IF(IMISSING.EQ.1)THEN
C
                  IF(JMIN(K).EQ.MAXGPREF)THEN
                     VALUE=XMISSP
C Arthur: Added to handle a case that will probably never be seen.
                  ELSE
                     VALUE=((JMIN(K)*SCAL2)+REF)/SCAL10
                  ENDIF
C
               ELSEIF(IMISSING.EQ.2)THEN
C
                  IF(JMIN(K).EQ.MAXGPREF)THEN
                     VALUE=XMISSP
                  ELSEIF(JMIN(K).EQ.MAXGPREF-1)THEN
                     VALUE=XMISSS
C Arthur: Added to handle a case that will probably never be seen.
                  ELSE
                     VALUE=((JMIN(K)*SCAL2)+REF)/SCAL10
                  ENDIF
C
               ELSE
                  VALUE=((JMIN(K)*SCAL2)+REF)/SCAL10
               ENDIF
C
               DO J=1,NOV(K)
                  A(M+J)=VALUE
               ENDDO
C
            ELSE
C
               PMISS=IBXX2(LBIT(K))-1
               SMISS=IBXX2(LBIT(K))-2
C
               DO J=1,NOV(K)
                  NVALUE=0
                  NEWIPOS=IPOS+LBIT(K)
C
                  IF(NEWIPOS.LE.L3264B+1)THEN
                     CALL MVBITS(IPACK(LOC),L3264B+1-NEWIPOS,LBIT(K),
     1                           NVALUE,0)
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
                     NBIT2=LBIT(K)-NBIT1
                     CALL MVBITS(IPACK(LOC),0,NBIT1,NVALUE,NBIT2)
                     LOC=LOC+1
                     CALL MVBITS(IPACK(LOC),L3264B-NBIT2,NBIT2,
     1                           NVALUE,0)
                     IPOS=NBIT2+1
                  ENDIF
C
                  SELECT CASE (IMISSING)
C
                     CASE (0)
C
C                          THERE ARE NO MISSING VALUES IN THIS
C                          DATA FIELD.
                        A(M+J)=(((FLOAT(JMIN(K)+NVALUE)*SCAL2)+REF)/
     1                            SCAL10)
C
                     CASE (1)
C
C                          THERE ARE PRIMARY MISSING VALUES IN
C                          THIS DATA FIELD.
                        IF(NVALUE.EQ.PMISS)THEN
                           A(M+J)=XMISSP
                        ELSE
                           A(M+J)=(((FLOAT(JMIN(K)+NVALUE)*SCAL2)+REF)/
     1                               SCAL10)
                        ENDIF
C
                     CASE (2)
C
C                          THERE ARE PRIMARY AND SECONDARY MISSING
C                          VALUES IN THIS DATA FIELD.
                        IF(NVALUE.EQ.PMISS)THEN
                           A(M+J)=XMISSP
                        ELSEIF(NVALUE.EQ.SMISS)THEN
                           A(M+J)=XMISSS
                        ELSE
                           A(M+J)=(((FLOAT(JMIN(K)+NVALUE)*SCAL2)+REF)/
     1                               SCAL10)
                        ENDIF
C
                     CASE DEFAULT
C
C                          THE MISSING VALUE REPRESENTATION IS
C                          UNRECOGNIZED.
                        IER=18
                        GOTO 900
                  END SELECT
                     
               ENDDO
            ENDIF
C
            M=M+NOV(K)
C
         ENDDO
C
      ELSE
C
C           A BIT-MAP IS PRESENT.
C
         DO K=1,LX
C
            IF(LBIT(K).EQ.0)THEN
               NVALUE=0
C
C                 NO VALUES ARE PACKED BECAUSE ALL OF THE VALUES
C                 IN THIS GROUP ARE THE SAME.
               DO J=1,NOV(K)
C
                  DO WHILE((IBMP(M+J).EQ.0).AND.(.NOT.CLEAN))
                     A(M+J)=XMISSP
                     M=M+1
                  ENDDO 
C
                  A(M+J)=(((FLOAT(JMIN(K)+NVALUE)*SCAL2)+REF)/SCAL10)
C
               ENDDO
C
            ELSE
C
               DO J=1,NOV(K)
C
                  DO WHILE((IBMP(M+J).EQ.0).AND.(.NOT.CLEAN))
                     A(M+J)=XMISSP
                     M=M+1
                  ENDDO 
C
                  NEWIPOS=IPOS+LBIT(K)
C
                  IF(NEWIPOS.LE.L3264B+1)THEN
                     CALL MVBITS(IPACK(LOC),L3264B+1-NEWIPOS,LBIT(K),
     1                           NVALUE,0)
 
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
                     NBIT2=LBIT(K)-NBIT1
                     CALL MVBITS(IPACK(LOC),0,NBIT1,NVALUE,NBIT2)
                     LOC=LOC+1
                     CALL MVBITS(IPACK(LOC),L3264B-NBIT2,NBIT2,
     1                           NVALUE,0)
                     IPOS=NBIT2+1
                  ENDIF
C
                  A(M+J)=(((FLOAT(JMIN(K)+NVALUE)*SCAL2)+REF)/SCAL10)
C
               ENDDO
            ENDIF
C
            M=M+NOV(K)
C
         ENDDO
      ENDIF
C
 900  RETURN
      END
