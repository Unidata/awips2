      SUBROUTINE PREP_INT(IA,NXY,NVAL,ICLEAN,ID,IE,
     1                    MINA,JMISSP,JMISSS,MISSP,MISSS,IER,*)
C
C        MAY      2000   LAWRENCE  ORIGINAL CODING
C        JANUARY  2001   GLAHN     COMMENTS; MISSP CHANGED TO
C                                  MISSS IN DO 130 LOOP;  ELIMINATED
C                                  UNUSED IS5( ) AND NS5 FROM CALL
C        JANUARY  2001   LAWRENCE  CHANGED JE=2**IE TO JE=2**(-IE)
C                                  AND NOW MULTIPLY THE VALUES IN
C                                  IA( ) BY JE INSTEAD OF DIVIDING
C                                  BY IT.
C        NOVEMBER 2001   GLAHN     MODIFIED PURPOSE TO INDICATE 
C                                  INTEGER DATA
C        JANUARY  2002   GLAHN     ADDED IER RETURN FOR DATA WITH
C                                  TOO WIDE A RANGE
C
C        PURPOSE
C            FINDS THE REFERENCE VALUE AND SUBTRACTS IT FROM THE
C            DATA VALUES WHEN THE ORIGINAL DATA FIELD IS INTEGER. 
C            THE DATA ARE SCALED BY THE DECIMAL AND BINARY
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
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C               IA(K) = CONTAINS THE INTEGER DATA FIELD.
C                       (K=1,NVAL).  (INPUT/OUTPUT)
C                 NXY = DIMENSION OF IA( ).  (INPUT)
C                NVAL = THE NUMBER OF VALUES IN IA( ).  (INPUT)
C              ICLEAN = 1 WHEN THERE ARE NO MISSING VALUES IA( ).
C                       0 OTHERWISE.  (INPUT)
C                  ID = THE DECIMAL SCALING EXPONENT.  (INPUT)
C                  IE = THE BINARY SCALING EXPONENT.  (INPUT)
C                MINA = THE FIELD MINIMUM VALUE WHEN THE ORIGINAL DATA
C                       ARE INTEGER.  (OUTPUT)
C              JMISSS = .TRUE. IF THERE IS A SECONDARY MISSING
C                       VALUE IN THE DATA FIELD.  .FALSE. OTHERWISE.
C                       (LOGICAL)
C              JMISSP = .TRUE. IF THERE IS A PRIMARY MISSING VALUE
C                       IN THE DATA FIELD.  .FALSE. OTHERWISE.
C                       (LOGICAL)
C               MISSP = WHEN MISSING POINTS CAN BE PRESENT IN THE DATA,
C                       THEY WILL HAVE THE VALUE MISSP OR MISSS WHEN
C                       THE DATA ARE INTEGER.  XMISSP IS THE PRIMARY
C                       MISSING VALUE.  (INPUT)
C               MISSS = SECONDARY MISSING VALUE INDICATOR WHEN THE DATA
C                       ARE INTEGER.  (INPUT)
C                 IER = ERROR RETURN.
C                         0 = GOOD RETURN
C                       920 = A VALUE LARGER THAN WHAT CAN BE PACKED
C                             INTO 30 BITS HAS BEEN ENCOUNTERED.
C                       (OUTPUT) 
C
C        LOCAL VARIABLES
C                IPOS = THE POSITION IN THE ARRAY OF THE FIRST
C                       NON-MISSING VALUE WHEN THERE ARE MISSING
C                       VALUES.  (INTERNAL)
C                  JD = THE DECIMAL SCALING FACTOR.
C                  JE = THE BINARY SCALING FACTOR.
C                   K = A LOOP INDEX VARIABLE.
C
C        NON SYSTEM SUBROUTINES CALLED
C           NONE.
C
      LOGICAL JMISSS, JMISSP
C
      DIMENSION IA(NXY)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2packer/RCS/prep_int.f,v $
     . $',                                                             '
     .$Id: prep_int.f,v 1.1 2004/09/16 16:52:29 dsa Exp $
     . $' /
C    ===================================================================
C
C
      IER=0
C
      IF(ICLEAN.EQ.1)THEN
C
C           THERE ARE NO MISSING VALUES IN THE ARRAY.
C           THIS IS THE SIMPLEST CASE ...
C           PROCESS THE DATA STRAIGHT AWAY.
C
         IF(ID.NE.0)THEN
            JD=10**ID
C
            DO 10 K=1,NVAL
               IA(K)=IA(K)*JD
 10         CONTINUE
C
         ENDIF
C
         MINA=IA(1)
C
         DO 20 K=1,NVAL
            IF(IA(K).LT.MINA)MINA=IA(K)
 20      CONTINUE
C
         IF(MINA.NE.0)THEN
C
            DO 30 K=1,NVAL
               IA(K)=IA(K)-MINA
               IF(IA(K).LT.0)GO TO 901
C                 IA(K) SHOULD NOT BE NEGATIVE.  THIS IS PROBABLY AN
C                 OVERFLOW BECAUSE OF TOO LARGE A RANGE OF VALUES.
 30         CONTINUE
C
         ENDIF
C
         IF(IE.NE.0)THEN
            JE=2**(-IE)
C
            DO 40 K=1,NVAL
               IA(K)=IA(K)*JE
 40         CONTINUE
C
         ENDIF
C
      ELSE IF(JMISSP.AND..NOT.JMISSS)THEN
C
C           THERE ARE PRIMARY MISSING VALUES IN THIS FIELD. 
C
         IF(ID.NE.0)THEN
            JD=10**ID
C
            DO 50 K=1,NVAL
               IF(IA(K).EQ.MISSP)CYCLE
               IA(K)=IA(K)*JD
 50         CONTINUE
C
         ENDIF
C
C           FIND THE FIRST NON-MISSING VALUE
C           AND SET MINA TO THAT VALUE.  IPOS
C           WILL END UP CONTAINING THE 
C           POSITION OF THE FIRST NON-MISSING
C           VALUE.
         IPOS=1
C
         DO 55 K=1,NVAL
            IF(IA(K).NE.MISSP)EXIT
            IPOS=IPOS+1
 55      CONTINUE
C
         MINA=IA(IPOS)
C
         DO 60 K=IPOS+1,NVAL
            IF(IA(K).EQ.MISSP)CYCLE
            IF(IA(K).LT.MINA)MINA=IA(K)
 60      CONTINUE
C
         IF(MINA.NE.0)THEN
C
            DO 70 K=1,NVAL
               IF(IA(K).EQ.MISSP)CYCLE
               IA(K)=IA(K)-MINA
               IF(IA(K).LT.0)GO TO 901
C                 IA(K) SHOULD NOT BE NEGATIVE.  THIS IS PROBABLY AN
C                 OVERFLOW BECAUSE OF TOO LARGE A RANGE OF VALUES.
 70         CONTINUE
C
         ENDIF
C
         IF(IE.NE.0)THEN
            JE=2**(-IE)
C
            DO 80 K=1,NVAL
               IF(IA(K).EQ.MISSP)CYCLE
               IA(K)=IA(K)*JE
 80         CONTINUE
C
         ENDIF
C
      ELSE IF(JMISSS.AND.JMISSP)THEN
C
C           THERE ARE BOTH PRIMARY AND SECONDARY 
C           MISSING VALUES.
         IF(ID.NE.0)THEN
            JD=10**ID
C
            DO 90 K=1,NVAL
               IF((IA(K).EQ.MISSP).OR.(IA(K).EQ.MISSS))CYCLE
               IA(K)=IA(K)*JD
 90         CONTINUE
C
         ENDIF
C
C           FIND THE FIRST NON-MISSING VALUE
C           AND SET MINA EQUAL TO THAT VALUE.  IPOS
C           WILL END UP CONTAINING THE POSITION
C           OF THE FIRST NON-MISSING VALUE.
         IPOS=1
C
         DO 95 K=1,NVAL
            IF((IA(K).NE.MISSP).AND.(IA(K).NE.MISSS))EXIT
            IPOS=IPOS+1
 95      CONTINUE
C
         MINA=IA(IPOS)
C
         DO 100 K=IPOS+1,NVAL
            IF((IA(K).EQ.MISSP).OR.(IA(K).EQ.MISSS))CYCLE
            IF(IA(K).LT.MINA)MINA=IA(K)
 100     CONTINUE
C
         IF(MINA.NE.0)THEN
C
            DO 110 K=1,NVAL
               IF((IA(K).EQ.MISSP).OR.(IA(K).EQ.MISSS))CYCLE
               IA(K)=IA(K)-MINA
               IF(IA(K).LT.0)GO TO 901
C                 IA(K) SHOULD NOT BE NEGATIVE.  THIS IS PROBABLY AN
C                 OVERFLOW BECAUSE OF TOO LARGE A RANGE OF VALUES.
 110        CONTINUE
C
         ENDIF
C
         IF(IE.NE.0)THEN
            JE=2**(-IE)
C
            DO 120 K=1,NVAL
               IF((IA(K).EQ.MISSP).OR.(IA(K).EQ.MISSS))CYCLE
               IA(K)=IA(K)*JE
 120        CONTINUE
C
         ENDIF
C
      ELSE IF(JMISSS.AND..NOT.JMISSP)THEN
C
C           THERE ARE ONLY SECONDARY MISSING VALUES.
         IF(ID.NE.0)THEN
            JD=10**ID
C
            DO 130 K=1,NVAL
               IF(IA(K).EQ.MISSS)CYCLE
               IA(K)=IA(K)*JD
 130        CONTINUE
C
         ENDIF
C
C           FIND THE FIRST NON-MISSING VALUE
C           AND SET MINA TO THAT VALUE.  IPOS
C           WILL END UP CONTAINING THE 
C           POSITION OF THE FIRST NON-MISSING
C           VALUE.
         IPOS=1
C
         DO 135 K=1,NVAL
            IF(IA(K).NE.MISSS)EXIT
            IPOS=IPOS+1
 135     CONTINUE
C
         MINA=IA(IPOS)
C
         DO 140 K=IPOS+1,NVAL
            IF(IA(K).EQ.MISSS)CYCLE
            IF(IA(K).LT.MINA)MINA=IA(K)
 140     CONTINUE
C
         IF(MINA.NE.0)THEN
C
            DO 150 K=1,NVAL
               IF(IA(K).EQ.MISSS)CYCLE
               IA(K)=IA(K)-MINA
               IF(IA(K).LT.0)GO TO 901
C                 IA(K) SHOULD NOT BE NEGATIVE.  THIS IS PROBABLY AN
C                 OVERFLOW BECAUSE OF TOO LARGE A RANGE OF VALUES.
 150        CONTINUE
C
         ENDIF
C
         IF(IE.NE.0)THEN
            JE=2**(-IE)
C
            DO 160 K=1,NVAL
               IF(IA(K).EQ.MISSS)CYCLE
               IA(K)=IA(K)*JE
 160        CONTINUE
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
D     WRITE(12,902)K,IA(K),MINA
D902  FORMAT(' TOO LARGE A RANGE OF VALUES TO PACK IN PREP_INT.',
D    1       'K, IA(K), MINA ARE',3I12)
      RETURN1
      END
