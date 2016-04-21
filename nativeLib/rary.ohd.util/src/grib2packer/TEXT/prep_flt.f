      SUBROUTINE PREP_FLT(A,IA,NXY,NVAL,ICLEAN,ID,IE,
     1                    XMINA,JMISSP,JMISSS,XMISSP,XMISSS,IER,*)
C
C        MAY     2000   LAWRENCE   ORIGINAL CODING
C        JANUARY 2001   GLAHN      COMMENTS; XMISSP CHANGED TO
C                                  XMISSS IN DO 130 LOOP;  ELIMINATED
C                                  UNUSED IS5( ) AND NS5 FROM CALL
C        JANAURY 2001   LAWRENCE   CHANGED E=2.**IE TO E=2.**(-IE)
C                                  AND NOW MULTIPLY THE VALUES IN
C                                  A( ) BY E INSTEAD OF DIVIDING
C                                  BY IT.
C        JANUARY 2002   GLAHN      CHANAGED INT( ) TO NINT( ); ADDED
C                                  IER AND ALTERNATE RETURN WHEN A( )
C                                  GT MALLOW = 2**30  
C
C        PURPOSE
C            FINDS THE REFERENCE VALUE AND SUBTRACTS IT FROM THE
C            DATA VALUES WHEN THE ORIGINAL DATA FIELD IS FLOATING
C            POINT. THE DATA ARE SCALED BY THE DECIMAL AND BINARY
C            SCALE FACTORS AS WELL.  BOTH PRIMARY AND SECONDARY
C            MISSING VALUES ARE TAKEN INTO ACCOUNT.
C
C            IF A DATA FIELD COMPOSED OF ALL MISSING VALUES IS
C            ENCOUNTERED, THEN THE DATA FIELD WILL BE PACKED
C            USING THE SIMPLE PACKING METHOD IF THAT IS WHAT
C            THE USER SPECIFIED. IF THE SIMPLE METHOD WAS NOT
C            SPECIFIED, THEN THE COMPLEX PACKING METHOD IS USED.
C
C            OPERATIONS WITHIN LOOPS ARE KEPT TO A MINIMUM AT THE
C            EXPENSE OF MORE CODE FOR EFFICIENCY.
C
C        DATA SET USE
C           KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE. (OUTPUT)
C
C        VARIABLES
C                A(K) = CONTAINS THE FLOATING POINT DATA FIELD.
C                       (K=1,NVAL).  IT IS MODIFIED BY SCALING
C                       AND SUBTRACTING THE MINIMUM VALUE.  (INPUT)
C               IA(K) = ONCE THE FLOATING POINT DATA VALUES ARE
C                       SCALED AND THE REFERENCE VALUE HAS BEEN 
C                       REMOVED THEY ARE TRUNCATED TO INTEGERS
C                       AND RETURNED TO THE CALLING ROUTINE VIA
C                       THIS ARRAY.  (OUTPUT)
C                 NXY = DIMENSION OF A( ).  (INPUT)
C                NVAL = THE NUMBER OF VALUES IN A( ).  (INPUT)
C              ICLEAN = 1 WHEN THERE ARE NO MISSING VALUES IN A( ).
C                       0 OTHERWISE.
C                  ID = THE DECIMAL SCALING EXPONENT.  (INPUT)
C                  IE = THE BINARY SCALING EXPONENT.  (INPUT)
C               XMINA = THE FIELD MINIMUM VALUE WHEN THE ORIGINAL DATA
C                       ARE FLOATING POINT.  (OUTPUT)
C              JMISSS = .TRUE. IF THERE IS A SECONDARY MISSING
C                       VALUE IN THE DATA FIELD.  .FALSE. OTHERWISE.
C                       (LOGICAL)  (INPUT)
C              JMISSP = .TRUE. IF THERE IS A PRIMARY MISSING VALUE
C                       IN THE DATA FIELD.  .FALSE. OTHERWISE.
C                       (LOGICAL)  (INPUT)
C              XMISSP = WHEN MISSING POINTS CAN BE PRESENT IN THE DATA,
C                       THEY WILL HAVE THE VALUE XMISSP OR XMISSS WHEN
C                       THE DATA ARE FLOATING POINT.  XMISSP
C                       IS THE PRIMARY MISSING VALUE.  (INPUT)
C              XMISSS = SECONDARY MISSING VALUE INDICATOR WHEN THE DATA
C                       ARE FLOATING POINT.  (INPUT)
C                 IER = ERROR RETURN
C                         0 = GOOD RETURN
C                       920 = VALUE TOO LARGE TO BE PACKED IN 30 BITS.
C                       (OUTPUT)
C
C        LOCAL VARIABLES
C                   D = THE DECIMAL SCALING FACTOR.
C                   E = THE BINARY SCALING FACTOR.
C                IPOS = THE POSITION IN THE ARRAY OF THE FIRST 
C                       NON-MISSING VALUE WHEN THERE ARE MISSING 
C                       VALUES.  (INTERNAL)
C                   K = A LOOP INDEX VARIABLE.
C
C        NON SYSTEM SUBROUTINES CALLED
C           NONE.
C
      PARAMETER (MALLOW=2**30)
c
      LOGICAL JMISSS,JMISSP
C
      DIMENSION A(NXY),IA(NXY)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2packer/RCS/prep_flt.f,v $
     . $',                                                             '
     .$Id: prep_flt.f,v 1.1 2004/09/16 16:52:29 dsa Exp $
     . $' /
C    ===================================================================
C
C
      IER=0
C
      IF(ICLEAN.EQ.1)THEN
C
C           THERE ARE NO MISSING VALUES IN THE ARRAY.
C           THIS IS THE SIMPLEST CASE.
C           PROCESS THE DATA STRAIGHTAWAY.
C
         IF(ID.NE.0)THEN
            D=10.**ID
C
            DO 10 K=1,NVAL
               A(K)=A(K)*D
 10         CONTINUE
C
         ENDIF
C
         XMINA=A(1)
C
         DO 20 K=1,NVAL
            IF(A(K).LT.XMINA)XMINA=A(K)
 20      CONTINUE
C
         IF(XMINA.NE.0)THEN
C
            DO 30 K=1,NVAL
               A(K)=A(K)-XMINA
               IF(A(K).GT.MALLOW)GO TO 901
C                 A(K) SHOULD NOT EXCEED 2**30.  THIS IS CAUSED BY TOO LARGE
C                 A RANGE OF VALUES.
 30         CONTINUE
C
         ENDIF
C
         IF(IE.NE.0)THEN
            E=2.**(-IE)
C
            DO 40 K=1,NVAL
               A(K)=A(K)*E
               IA(K)=NINT(A(K)) 
 40         CONTINUE
C
         ELSE 
C
            DO 45 K=1,NVAL
               IA(K)=NINT(A(K))
 45         CONTINUE
C
         ENDIF
C
      ELSE IF(JMISSP.AND..NOT.JMISSS)THEN
C
C           THERE ARE PRIMARY MISSING VALUES IN THIS FIELD. 
         IF(ID.NE.0)THEN
            D=10.**ID
C
            DO 50 K=1,NVAL
               IF(A(K).EQ.XMISSP)CYCLE
               A(K)=A(K)*D
 50         CONTINUE
C
         ENDIF
C
C           FIND THE FIRST NON-MISSING VALUE AND SET XMINA TO THAT 
C           VALUE.  IPOS WILL END UP CONTAINING THE POSITION
C           OF THE FIRST NON-MISSING VALUE.
         IPOS=1
C
         DO 55 K=1,NVAL
            IF(A(K).NE.XMISSP)EXIT
            IPOS=IPOS+1
 55      CONTINUE
            
         XMINA=A(IPOS)
C
         DO 60 K=IPOS+1,NVAL
            IF(A(K).EQ.XMISSP)CYCLE
            IF(A(K).LT.XMINA)XMINA=A(K)
 60      CONTINUE
C
         IF(XMINA.NE.0.)THEN
C
            DO 70 K=1,NVAL
               IF(A(K).EQ.XMISSP)CYCLE
               A(K)=A(K)-XMINA
               IF(A(K).GT.MALLOW)GO TO 901
C                 A(K) SHOULD NOT EXCEED 2**30.  THIS IS CAUSED BY TOO LARGE
C                 A RANGE OF VALUES.
 70         CONTINUE
C
         ENDIF
C
         IF(IE.NE.0)THEN
            E=2.**(-IE)
C
            DO 80 K=1,NVAL
               IF(A(K).NE.XMISSP)A(K)=A(K)*E
               IA(K)=NINT(A(K)) 
 80         CONTINUE
C
         ELSE
C
            DO 85 K=1,NVAL
               IA(K)=NINT(A(K))
 85         CONTINUE

         ENDIF
C
      ELSEIF(JMISSS.AND.JMISSP)THEN
C
C           THERE ARE BOTH PRIMARY AND SECONDARY
C           MISSING VALUES.
C
         IF(ID.NE.0)THEN
            D=10.**ID
C
            DO 90 K=1,NVAL
               IF((A(K).EQ.XMISSP).OR.(A(K).EQ.XMISSS))CYCLE
               A(K)=A(K)*D
 90         CONTINUE
C
         ENDIF
C
C           FIND THE FIRST NON-MISSING VALUE
C           AND SET XMINA EQUAL TO THAT VALUE.  IPOS
C           WILL END UP CONTAINING THE POSITION
C           OF THE FIRST NON-MISSING VALUE.
         IPOS=1
C
         DO 95 K=1,NVAL
            IF((A(K).NE.XMISSP).AND.(A(K).NE.XMISSS))EXIT
            IPOS=IPOS+1
 95      CONTINUE
            
         XMINA=A(IPOS)
C
         DO 100 K=IPOS+1,NVAL
            IF((A(K).EQ.XMISSP).OR.(A(K).EQ.XMISSS))CYCLE
            IF(A(K).LT.XMINA)XMINA=A(K)
 100     CONTINUE
C
         IF(XMINA.NE.0.)THEN
C
            DO 110 K=1,NVAL
               IF((A(K).EQ.XMISSP).OR.(A(K).EQ.XMISSS))CYCLE
               A(K)=A(K)-XMINA
               IF(A(K).GT.MALLOW)GO TO 901
C                 A(K) SHOULD NOT EXCEED 2**30.  THIS IS CAUSED BY TOO LARGE
C                 A RANGE OF VALUES.
 110        CONTINUE
C
         ENDIF
C
         IF(IE.NE.0)THEN
            E=2.**(-IE)
C
            DO 120 K=1,NVAL
               IF((A(K).NE.XMISSP).AND.(A(K).NE.XMISSS))A(K)=A(K)*E
               IA(K)=NINT(A(K))
 120        CONTINUE
C
         ELSE
C
            DO 125 K=1,NVAL
               IA(K)=NINT(A(K))
 125        CONTINUE
C
         ENDIF
C
      ELSEIF(JMISSS.AND..NOT.JMISSP)THEN
C
C           THERE ARE ONLY SECONDARY MISSING VALUES.
         IF(ID.NE.0)THEN
            D=10.**ID
C
            DO 130 K=1,NVAL
               IF(A(K).EQ.XMISSS)CYCLE
               A(K)=A(K)*D
 130        CONTINUE
C
         ENDIF
C
C
C           FIND THE FIRST NON-MISSING VALUE
C           AND SET XMINA TO THAT VALUE.  IPOS
C           WILL END UP CONTAINING THE POSITION
C           OF THE FIRST NON-MISSING VALUE.
         IPOS=1
C
         DO 135 K=1,NVAL
            IF(A(K).NE.XMISSS)EXIT
            IPOS=IPOS+1
 135     CONTINUE
            
         XMINA=A(IPOS)
C
         DO 140 K=IPOS+1,NVAL
            IF(A(K).EQ.XMISSS)CYCLE
            IF(A(K).LT.XMINA)XMINA=A(K)
 140     CONTINUE
C
         IF(XMINA.NE.0.)THEN
C
            DO 150 K=1,NVAL
               IF(A(K).EQ.XMISSS)CYCLE
               A(K)=A(K)-XMINA
               IF(A(K).GT.MALLOW)GO TO 901
C                 A(K) SHOULD NOT EXCEED 2**30.  THIS IS CAUSED BY TOO LARGE
C                 A RANGE OF VALUES.
 150        CONTINUE
C
         ENDIF
C
         IF(IE.NE.0)THEN
            E=2.**(-IE)
C
            DO 160 K=1,NVAL
               IF(A(K).NE.XMISSS)A(K)=A(K)*E
               IA(K)=NINT(A(K))
 160        CONTINUE
C
         ELSE
C
            DO 165 K=1,NVAL
               IA(K)=NINT(A(K))
 165        CONTINUE
C
         ENDIF
C
      ENDIF
C
 900  RETURN
C
C        ALTERNATE RETURN SECTION.
C
 901  IER=920
D     WRITE(12,902)K,A(K),XMINA
D902  FORMAT(' TOO LARGE A RANGE OF VALUES TO PACK IN PREP_FLT.',
D    1       'K, A(K), XMINA ARE',I12,2F12.1)
      RETURN1
      END
