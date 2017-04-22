      SUBROUTINE INT_MAP(IA,IB,NXY,IS5,NS5,ICLEAN,IBITMAP,
     1                   MISSP,iwork,JER,NDJER,KJER)
C
C        MAY      2000   LAWRENCE ORIGINAL CODING
C        JANUARY  2001   GLAHN    COMMENTS
C        NOVEMBER 2001   GLAHN    ADDED JER, NDFER, AND KJER TO
C                                 CALL AND CALLED TRACE
C
C        PURPOSE
C            PROCESSES THE BIT-MAP ASSOCIATED WITH A GRID OF INTEGER
C            DATA VALUES.  EXACTLY HOW A BIT-MAP IS PROCESSED DEPENDS
C            PRIMARILY UPON WHICH PACKING METHOD IS BEING USED TO
C            COMPRESS THE GRIDDED DATA.  THE BIT-MAP INDICATES WHERE
C            MISSING VALUES ARE LOCATED IN THE DATA FIELD BEING PACKED.
C            A 0 INDICATES A BAD VALUE WHILE 1 INDICATES A GOOD VALUE.
C            THERE IS A ONE-TO-ONE CORRESPONDENCE BETWEEN THE VALUES
C            IN THE BIT-MAP AND THE DATA IN THE DATA FIELD BEFORE 
C            MISSING VALUES ARE REMOVED.
C
C            A BIT-MAP IS REQUIRED WITH SIMPLE PACKING TO
C            INDICATE WHERE MISSING VALUES ARE LOCATED IN THE
C            DATA FIELD.  IF A FIELD TO BE PACKED USING THE
C            SIMPLE METHOD ISN'T ACCOMPANIED BY A BIT-MAP AND
C            THERE ARE MISSING VALUES EMBEDDED IN THE DATA,
C            THEN THIS ROUTINE WILL GENERATE A BIT-MAP AND
C            REMOVE THE MISSING VALUES FROM THE DATA FIELD.
C
C            THE COMPLEX PACKING METHOD DOES NOT NEED A BIT-MAP
C            TO INDICATE WHERE MISSING VALUES ARE LOCATED IN THE
C            DATA FIELD.  A BIT-MAP CAN BE SUPPLIED BY THE USER
C            IF THEY ARE PROVIDING A DATA FIELD WITHOUT THE MISSING
C            VALUES IN IT.  THIS ROUTINE WILL PLACE THE MISSING
C            VALUES INTO THEIR PROPER PLACES IN THE GRID IN THAT
C            CASE AND ELIMINATE THE BIT MAP.
C
C        DATA SET USE
C           NONE
C
C        VARIABLES
C               IA(K) = IA( ) CONTAINS THE DATA TO BE COMPRESSED
C                       (K=1,NXY).  (INPUT/OUTPUT)
C               IB(K) = THE BIT MAP WHEN ONE IS USED (K=1,NXY).
C                       IT CAN BE INPUT OR IT CAN BE CALCULATED IF
C                       THE SIMPLE METHOD IS USED.  COMPLEX AND SPATIAL
C                       DIFFERENCING METHODS DO NOT USE A BIT MAP,
C                       BUT WILL ACCEPT ONE AND INSERT THE MISSING
C                       VALUES INTO THE FIELD BEFORE PACKING.
C                       (INPUT/OUTPUT)
C                 NXY = DIMENSION OF IA( ), IB( ),AND IWORK( ).
C                       (INPUT)
C              IS5(K) = THE VALUES ASSOCIATED WITH SECTION 5, KEYED
C                       TO THE OCTET NUMBER(K=1,NS5). THE ELEMENT
C                       USED IN THIS ROUTINE IS:
C                       IS5(10), TEMPLATE NUMBER:
C                         0 = SIMPLE
C                         1 = NOT SUPPORTED
C                         2 = COMPLEX
C                         3 = SPATIAL DIFFERENCING
C                       (INPUT)
C                 NS5 = THE DIMENSION OF IS5( ).  (INPUT)
C              ICLEAN = 1 WHEN THERE ARE NO MISSING VALUES IN IA( ).
C                       0 OTHERWISE.  (INPUT/OUTPUT)
C             IBITMAP = 1 WHEN THERE IS A BITMAP IN IB( , ).
C                       0 OTHERWISE.  (INPUT/OUTPUT)
C               MISSP = WHEN MISSING POINTS CAN BE PRESENT IN THE DATA,
C                       THEY WILL HAVE THE VALUE MISSP.  (INPUT)
C            JER(J,K) = RETURN STATUS CODES AND SEVERITY LEVELS
C                       (J=1,NDJER)(K=1,2).  VALUES CAN COME FROM
C                       SUBROUTINES; OTHERWISE: 0 = GOOD RETURN.
C                       (INPUT/OUTPUT)
C               NDJER = DIMENSION OF JER( ).  (INPUT)
C                KJER = NUMBER OF VALUES IN JER( ).  (INPUT/OUTPUT)
C
C        LOCAL VARIABLES
C            IWORK(K) = USED TO CONTAIN THE DATA FIELD WHEN WE
C                       ARE EXPANDING IT TO CONTAIN MISSING
C                       VALUES (ONLY DONE WITH THE COMPLEX
C                       PACKING METHOD).  (AUTOMATIC ARRAY)
C                   K = A LOOPING/ARRAY INDEXING VARIABLE.
C                   M = USED TO KEEP TRACK OF THE POSITION OF
C                       REAL VALUES VERSUS MISSING VALUES IN 
C                       THE DATA FIELD.
C
C        NON SYSTEM SUBROUTINES CALLED
C          NONE 
C
      DIMENSION IA(NXY),IB(NXY),IWORK(NXY)
      DIMENSION IS5(NS5)
      DIMENSION JER(NDJER,2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2packer/RCS/int_map.f,v $
     . $',                                                             '
     .$Id: int_map.f,v 1.1 2004/09/16 16:52:29 dsa Exp $
     . $' /
C    ===================================================================
C
C
      IF(IS5(10).EQ.0)THEN
C
C           SIMPLE PACKING IS BEING USED. IF THE DATA FIELD
C           CONTAINS MISSING VALUES, THEN SEE IF THE USER HAS
C           SUPPLIED A BITMAP. IF ONE HASN'T BEEN SUPPLIED,THEN
C           GENERATE ONE.
C
         IF(ICLEAN.EQ.0)THEN
C
            IF(IBITMAP.EQ.0)THEN
               IBITMAP=1
C                 IBITMAP BEING OVERRIDDEN AND A BIT MAP BEING
C                 GENERATED.
               CALL PK_TRACE(KFILDO,JER,NDJER,KJER,913,0)
C
               M=1
C
               DO 10 K=1,NXY
C
                 IF(IA(K).EQ.MISSP)THEN
                    IB(K)=0
                 ELSE
                    IB(K)=1
                    IA(M)=IA(K)
                    M=M+1
                 ENDIF
C
 10            CONTINUE
C
               ICLEAN=1
            ENDIF
C
         ENDIF
C
C           IF THE USER DOESN'T SUPPLY A BIT-MAP
C           AND THE FIELD IS INDICATED AS CLEAN,
C           THEN IT IS ASSUMED THAT THERE ARE NO
C           MISSING VALUES. IN THAT CASE THE BIT-MAP HAS
C           ALL ONE'S AND DOES NOT NEED TO BE PACKED IN
C           SECTION 6 NOR SUPPLIED BY THE USER.
      ELSEIF(IS5(10).GE.2)THEN
C
C           A FORM OF COMPLEX PACKING IS BEING USED.
C           IF A BIT-MAP HAS BEEN SUPPLIED AND THE FIELD
C           IS CLEAN, THEN EXPAND THE DATA FIELD TO INCLUDE
C           THE MISSING VALUES AND DISCARD THE BIT-MAP.
C
         IF(IBITMAP.EQ.1)THEN
C
            IF(ICLEAN.EQ.1)THEN
               M=1
C
               DO 20 K=1,NXY
                  IF(IB(K).EQ.0)THEN
                     IWORK(K)=MISSP
                  ELSE
                     IWORK(K)=IA(M)
                     M=M+1
                  ENDIF
 20            CONTINUE
C
               DO 30 K=1,NXY
                  IA(K)=IWORK(K)
 30            CONTINUE
C
               IBITMAP=0
               ICLEAN=0
C                 IBITMAP AND ICLEAN BEING OVERRIDDEN AND A
C                 THE BIT MAP BEING INCORPORATED INTO THE DATA.
               CALL PK_TRACE(KFILDO,JER,NDJER,KJER,914,0)
C
            ENDIF
C
         ENDIF
C
      ENDIF
C
      RETURN
      END
