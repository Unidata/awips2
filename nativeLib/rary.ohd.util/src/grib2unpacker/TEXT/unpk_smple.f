      SUBROUTINE UNPK_SMPLE(KFILDO,A,ND2X3,NX,NY,IPACK,ND5,LOCN,IPOS,
     1                      IS5,NS5,IS7,NS7,IBITMAP,IBMP,IUNPKOPT,
     2                      L3264B,REF,XMISSP,IER,*)
C
C        APRIL    1995   GLAHN   TDL   HP
C        APRIL    1995   MOELLER  RENAMED SUBROUTINE AND CHANGED 
C                                 STATEMENT TO UNPACK LBIT.
C        DECEMBER 1995   MOELLER  REMOVED KFILDO FROM CALL SEQUENCE
C        JANUARY  1996   MOELLER  CHANGED SCALE FACTORS TO REAL;
C                                 MODIFIED VARIABLE NAMES TO BE MORE
C                                 DESCRIPTIVE
C        JANUARY  1996   CALKINS  ADDED KFILDO TO CALL SEQUENCE
C        JUNE     1999   LAWRENCE UPDATED FOR UNPK_GRIB2; REORDERED
C                                 AND RENAMED MANY OF THE CALLING
C                                 ARGUMENTS TO BE MORE CONSISTENT
C                                 WITH OTHER ROUTINES; UPDATED THE
C                                 DOCUMENTATION
C        JANUARY  2001   GLAHN    COMMENTS; CHANGED IER = 17 TO 702;
C                                 CHANGED FMISS TO XMISSP; DEFINED      
C                                 ISCAL AND PUT IT INTO CALL TO 
C                                 UNPKLXBM
C        FEBRUARY 2001   GLAHN    COMMENTS
C        NOVEMBER 2001   GLAHN    CHANGED IER=702 TO IER=705
C
C        PURPOSE
C            UNPACKS DATA THAT WAS PACKED USING THE SIMPLE
C            PACKING SCHEME. THIS ROUTINE WILL UTILIZE
C            A BIT MAP IF THERE WAS ONE PROVIDED.
C
C            NOTE THAT THE FOLLOWING EQUATION IS USED IN UNPACKING THE
C            DATA:
C                 Y = [R + X * (2 ** E)] * (10 ** -D)]
C                 WHERE
C                     Y = THE VALUE WE ARE UNPACKING
C                     R = THE REFERENCE VALUE (FIRST ORDER MINIMA)
C                     X = THE PACKED VALUE
C                     E = THE BINARY SCALE FACTOR
C                     D = THE DECIMAL SCALE FACTOR
C
C        DATA SET USE
C           KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE. (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C                A(K) = UNPACKED GRID POINT DATA VALUES
C                       RETURNED (K=1,ND2X3).  (OUTPUT)
C               ND2X3 = THE DIMENSION OF A( ) AND IBMP( ).  (INPUT)
C               NX,NY = THE ACTUAL DIMENSIONS OF THE GRID WE ARE
C                       PROCESSING. (INPUT)
C            IPACK(J) = THE ARRAY HOLDING THE ACTUAL PACKED MESSAGE
C                       (J=1,ND5). (INPUT)
C                 ND5 = DIMENSION OF IPACK(). (INPUT)
C                LOCN = THE WORD POSITION FROM WHICH TO UNPACK THE
C                       NEXT VALUE. (INPUT/OUTPUT)
C                IPOS = THE BIT POSITION IN LOCN FROM WHICH TO START
C                       UNPACKING THE NEXT VALUE.  (INPUT/OUTPUT)
C              IS5(L) = THE ARRAY CORRESPONDING TO SECTION 5
C                       OF THE GRIB2 CODE. (INPUT)
C                 NS5 = DIMENSION OF IS5( ). (INPUT)
C              IS7(L) = HOLDS THE VALUES FOR GRIB2 SECTION 7 (L=1,NS7).
C                       (INPUT-OUTPUT)
C                 NS7 = DIMENSION OF IS7( ). (INPUT)
C             IBITMAP = 1 IF THERE WAS A BIT-MAP PACKED INTO
C                         THIS GRIB2 MESSAGE.
C                       0 IF THERE WAS NOT A BIT-MAP IN THIS
C                         GRIB2 MESSAGE. (LOGICAL) (INPUT)
C             IBMP(K) = WORK ARRAY (K=1,ND2X3).  HOLDS BIT MAP,
C                       IF ONE IS PRESENT. (INPUT)
C            IUNPKOPT = 0 DON'T UNPACK THIS DATA GRID. AN ERROR
C                         WAS ENCOUNTERED.
C                       1 UNPACK THIS GRID USING THE SIMPLE METHOD,
C                         LEAVE THE MISSING VALUES IN THE GRID,
C                         DO NOT RETURN A BIT-MAP.
C                       2 UNPACK THIS GRID USING THE SIMPLE METHOD,
C                         REMOVE THE MISSING VALUES FROM THE GRID,
C                         RETURN A BIT-MAP INDICATING MISSING VALUE
C                         LOCATIONS.
C                       (INPUT)
C              L3264B = INTEGER WORD LENGTH OF MACHINE BEING USED.
C                       A WORKING COPY N = L3264B FOR USE IN CALLS TO
C                       PKBG. (INPUT)
C                 REF = THE REFERENCE VALUE THAT WE WILL BE USING 
C                       TO UNPACK THE DATA. (INPUT)
C              XMISSP = VALUE TO INSERT INTO A( ) AT POINTS WHERE
C                       THE BIT MAP INDICATES VALUES ARE MISSING.
C                       (INPUT)
C                 IER = ERROR RETURN CODE.  (OUTPUT)
C                         0 = GOOD RETURN ... THE ROUTINE PROBABLY
C                             WORKED.
C                       6-8 = ERROR RETURN CODES FROM UNPKBG AND
C                             UNPKLXBM. SEE THE DOCUMENTATION IN
C                             THESE ROUTINES. 
C                       705 = THE INTERNAL ARRAYS ARE NOT DIMENSIONED
C                             LARGE ENOUGH TO CONTAIN AND PROCESS THE
C                             GRID.  INCREASE THE SIZE OF ND2X3.
C                   * = ALTERNATIVE ERROR RETURN.    
C
C             LOCAL VARIABLES
C             ISCALED = THE DECIMAL SCALING FACTOR. 
C             ISCALEB = THE BINARY SCALING FACTOR. 
C              IWIDTH = THE WIDTH OF THE PACKED VALUES. 
C              MAPBIT = 0 WHEN A BIT MAP IS NOT PRESENT.
C                       1 OTHERWISE.  
C                 NXY = THE PRODUCT OF NX*NY. USED AS THE ACTUAL 
C                       AMOUNT OF DATA IN A() AND IBMP(). MUST
C                       BE LESSER THAN OR EQUAL TO ND2X3. 
C                 REF = THE MINIMUM VALUE THAT WAS SUBTRACTED
C                       BEFORE PACKING (I.E., THE REFERENCE VALUE).
C               ISCAL = INDICATES COMBINATIONS OF SCALING.
C                       0 = NONE
C                       1 = DECIMAL ONLY
C                       2 = BINARY ONLY
C                       3 = BOTH DECIMAL AND BINARY
C              SCAL10 = DECIMAL SCALING PARAMETER = 10.**ISCALED.
C               SCAL2 = BINARY SCALING PARAMETER = 2.**ISCALEB.
C
C        NON SYSTEM SUBROUTINES CALLED
C           UNPKLXBM
C
      LOGICAL CLEAN
C
      DIMENSION A(ND2X3),IBMP(ND2X3)
      DIMENSION IPACK(ND5)
      DIMENSION IS5(NS5),IS7(NS7)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2unpacker/RCS/unpk_smple.f,v $
     . $',                                                             '
     .$Id: unpk_smple.f,v 1.1 2004/09/16 16:51:50 dsa Exp $
     . $' /
C    ===================================================================
C
C
C        SET ERROR RETURN.
C
      IER=0
      N=L3264B
C
C        DETERMINE THE APPROPRIATE SCALE FACTORS, REFERENCE VALUES,
C        AND FIELD WIDTHS FROM THE IS5 ARRAY TEMPLATE FOR SIMPLE
C        PACKING.
      ISCALED=IS5(18)
      ISCALEB=IS5(16)
      IWIDTH=IS5(20)
C
C        NX*NY = THE NUMBER OF GRID POINTS THAT WE HAVE TO
C        PROCESS. THE ACTUAL NUMBER OF DATA VALUES PACKED
C        WILL BE EQUAL TO (IF THERE IS NO BIT-MAP) OR LESS THAN
C        THIS VALUE (IF THERE IS A BIT MAP WITH MISSING VALUES).
      NXY=NX*NY
C
C        CHECK TO MAKE SURE THAT NXY IS LE ND2X3.
C
      IF(NXY.GT.ND2X3)THEN
         IER=705
         GO TO 900
      ENDIF
C
C        DEFINE ISCAL AND COMPUTE SCAL10 AND SCAL2.
C
      IF(ISCALED.EQ.0.AND.ISCALEB.EQ.0)THEN
         ISCAL=0
         SCAL10=1.
         SCAL2=1.
      ELSEIF(ISCALED.NE.0.AND.ISCALEB.EQ.0)THEN
         ISCAL=1
         SCAL10=10.**ISCALED
         SCAL2=1.
      ELSEIF(ISCALED.EQ.0.AND.ISCALEB.NE.0)THEN
         ISCAL=2
         SCAL10=1.
         SCAL2=2.**ISCALEB
      ELSEIF(ISCALED.NE.0.AND.ISCALEB.NE.0)THEN
         ISCAL=3
         SCAL10=10.**ISCALED
         SCAL2=2.**ISCALEB
      ENDIF
C
C        SET CLEAN.
C
      IF(IUNPKOPT.EQ.1)THEN
         CLEAN=.FALSE.
      ELSE
         CLEAN=.TRUE.
      ENDIF
C
C        UNPACK THE VALUES.
C***D     WRITE(KFILDO,850)ND5,LOCN,IPOS,NXY,IWIDTH,
C***D    1           XMISSP,REF,IBITMAP,ISCAL,SCAL10,SCAL2,IUNPKOPT,N,IER
C***D850  FORMAT(/' UNPK_SMPLE--ND5,LOCN,IPOS,NXY,IWIDTH,',
C***D    1           'XMISSP,REF,IBITMAP,ISCAL,SCAL10,SCAL2,IUNPKOPT,N,IER'/
C***D    2         5I8,F8.1,F12.0,2I6,2F8.1,3I6)
C
      CALL UNPKLXBM(KFILDO,IPACK,ND5,LOCN,IPOS,A,IBMP,NXY,IWIDTH,
     1              XMISSP,REF,IBITMAP,ISCAL,SCAL10,SCAL2,CLEAN,N,IER)
C
 900  IF(IER.NE.0)RETURN 1
C
      RETURN
      END
