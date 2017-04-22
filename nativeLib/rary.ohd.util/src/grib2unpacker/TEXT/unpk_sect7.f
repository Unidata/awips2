      SUBROUTINE UNPK_SECT7(KFILDO,jmin,lbit,nov,iwork,A,ND2X3,NX,NY,
     1                      IUNPKOPT,IPACK,REF,ND5,IS5,NS5,IS7,NS7,
     2                      IBITMAP,IB,XMISSP,XMISSS,BOUST,L3264B,
     3                      LOCN,IPOS,IER,ISEVERE,*)
C
C        MARCH     2000   LAWRENCE GSC/TDL    ORIGINAL CODING
C        JANUARY   2001   GLAHN    COMMENTS; CHANGED IER = 16 TO 708;
C                                  OMITTED EXISTS FROM CALL SEQUENCE;
C                                  ADDED IER = 701; ADDED TEST FOR
C                                  SECTION LENGTH WITH IER = 708;
C                                  ELIMINATED IS7( ) AND NS7 IN CALL
C                                  TO UNPK_CMPLX
C        FEBRUARY  2001   GLAHN    CHECKED SIZE OF NS7; COMMENTS
C        NOVEMBER  2001   GLAHN    ADDED DIMENSION IB(ND2X3)
C        MARCH     2002   GLAHN    CHANGED IER= 708 TO 709
C        SEPTEMBER 2002   GLAHN    ADDED IS5(32) IN CALL TO UNPK_CMPLX.
C
C        PURPOSE
C            UNPACKS SECTION 7, THE DATA SECTION OF A GRIB2 MESSAGE.
C
C        DATA SET USE
C           KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE. (OUTPUT
C                    FILE)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE. (INPUT)
C                A(J) = THE UNPACKED DATA (J=1,ND2X3).  (OUTPUT)
C               ND2X3 = THE SIZE OF A( ).  (INPUT)
C                  NX = THE NUMBER OF COLUMNS IN THE PRODUCT. (INPUT)
C                  NY = THE NUMBER OF ROWS IN THE PRODUCT. (INPUT)
C            IUNPKOPT = 0 DON'T UNPACK THIS DATA GRID. AN ERROR
C                         WAS ENCOUNTERED.
C                       1 UNPACK THIS GRID USING THE SIMPLE METHOD,
C                         LEAVE THE MISSING VALUES IN THE GRID,
C                         DO NOT RETURN A BIT-MAP.
C                       2 UNPACK THIS GRID USING THE SIMPLE METHOD,
C                         REMOVE THE MISSING VALUES FROM THE GRID,
C                         RETURN A BIT-MAP INDICATING MISSING VALUE
C                         LOCATIONS.
C                       3 UNPACK THIS GRID USING THE COMPLEX METHOD,
C                         LEAVE THE MISSING VALUES IN THE GRID,
C                         DO NOT RETURN A BIT-MAP.
C                       4 UNPACK THIS GRID USING THE COMPLEX METHOD,
C                         REMOVE THE MISSING VALUES FROM THE GRID,
C                         RETURN A BIT-MAP INDICATING MISSING VALUE
C                         LOCATIONS.
C                       5 UNPACK THIS GRID USING THE SECOND ORDER
C                         DIFFERENCES METHOD,
C                         LEAVE THE PRIMARY MISSING VALUES IN THE GRID,
C                         DO NOT RETURN A BIT-MAP.
C                       6 UNPACK THIS GRID USING THE SECOND ORDER
C                         DIFFERENCES METHOD,
C                         REMOVE THE PRIMARY MISSING VALUES FROM
C                         THE GRID,
C                         RETURN A BIT-MAP INDICATING THE PRIMARY
C                         MISSING VALUE LOCATIONS.
C                         (INPUT)
C            IPACK(J) = THE ARRAY THAT HOLDS THE ACTUAL PACKED MESSAGE
C                       (J=1,ND5). (INPUT/OUTPUT)
C                 REF = THE REFERENCE VALUE OF THE FIELD. (INPUT)
C                 ND5 = THE SIZE OF THE ARRAY IPACK( ). (INPUT)
C              IS5(J) = THE GRID DEFINITION DATA THAT IS UNPACKED FROM
C                       IPACK( ) IS PLACED INTO THIS ARRAY (J=1,NS5).
C                       (OUTPUT)
C                 NS5 = SIZE OF IS5( ). (INPUT)
C              IS7(J) = THE DATA THAT IS UNPACKED FROM
C                       IPACK( ) IS PLACED INTO THIS ARRAY (J=1,NS7).
C                       (OUTPUT)
C                 NS7 = SIZE OF IS7( ). (INPUT)
C             IBITMAP = 1 IF THERE WAS A BIT-MAP PACKED INTO
C                       THIS GRIB2 MESSAGE.
C                       0 IF THERE WAS NOT A BIT-MAP IN THIS
C                       GRIB2 MESSAGE. (LOGICAL) (INPUT)
C               IB(K) = WORK ARRAY (K=1,ND2X3).  HOLDS BIT-MAP,
C                       IF ONE IS PRESENT. (INPUT)
C              XMISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS
C                       ZERO WHEN THERE ARE NO VALUES IN ARRAY A( , )
C                       THAT WERE TREATED AS MISSING.  (INPUT)
C              XMISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS
C                       ZERO WHEN THERE ARE NO SECONDARY MISSING
C                       VALUES.  THIS VALUE DOES NOT APPLY TO SIMPLE
C                       OR COMPLEX PACKING.  (INPUT)
C               BOUST = .TRUE. IF THE DATA FIELD WAS SCANNED
C                       BOUSTROPHEDONICALLY. .FALSE. OTHERWISE.
C                       (LOGICAL) (INPUT)
C              L3264B = THE INTEGER WORD LENGTH IN BITS OF THE MACHINE
C                       BEING USED. VALUES OF 32 AND 64 ARE
C                       ACCOMMODATED. (INPUT)
C                LOCN = THE WORD POSITION FROM WHICH TO UNPACK THE
C                       NEXT VALUE. (INPUT/OUTPUT)
C                IPOS = THE BIT POSITION IN LOCN FROM WHICH TO START
C                       UNPACKING THE NEXT VALUE.  (INPUT/OUTPUT)
C                 IER = RETURN STATUS CODE. (OUTPUT)
C                         0 = GOOD RETURN.
C                       6-8 = ERROR CODES GENERATED BY UNPKBG. SEE THE
C                             DOCUMENTATION IN THE PKBG ROUTINE.
C                       701 = IS7(5) DOES NOT INDICATE SECTION 7.
C                       702 = IS7( ) HAS NOT BEEN DIMENSIONED LARGE
C                             ENOUGH TO CONTAIN THE ENTIRE TEMPLATE. 
C                       709 = TYPE OF PACKING UNRECOGNIZED.
C                       799 = UNEXPECTED END OF MESSAGE.
C                   * = ALTERNATE RETURN WHEN IER NE 0.
C
C             LOCAL VARIABLES
C             LOCN7_1 = SAVES THE WORD POSITION LOCN IN IPACK
C                       UPON ENTRY TO STORE BACK TO LOCN IN CASE
C                       THERE IS A FATAL ERROR.
C             IPOS7_1 = SAVES THE BIT POSITION IPOS IN LOCN
C                       UPON ENTRY TO STORE BACK TO IPOS IN CASE
C                       THERE IS A FATAL ERROR.
C               LSECT = CONTAINS THE LENGTH OF SECTION 7
C                       AS UNPACKED FROM THE FIRST FOUR
C                       BYTES IN THE SECTION.
C               NSECT = SECTION NUMBER.
C                   K = A LOOPING INDEX VARIABLE.
C                   N = L3264B = THE INTEGER WORD LENGTH IN BITS OF
C                       THE MACHINE BEING USED. VALUES OF 32 AND
C                       64 ARE ACCOMMODATED.
C
C        NON SYSTEM SUBROUTINES CALLED
C           ENDOMESS, UNPK_SMPLE, UNPK_CMPLX, UNPKBG
C
      DIMENSION A(ND2X3),IB(ND2X3)
      DIMENSION IPACK(ND5)
      DIMENSION IS5(NS5),IS7(NS7)
C
      LOGICAL BOUST,ENDOMESS
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2unpacker/RCS/unpk_sect7.f,v $
     . $',                                                             '
     .$Id: unpk_sect7.f,v 1.1 2004/09/16 16:51:50 dsa Exp $
     . $' /
C    ===================================================================
C
C
      N=L3264B
      IER=0
C
C        CHECK THE DIMENSIONS OF IS7( ).
C
      IF(NS7.LT.6)THEN
         IER=702
         GO TO 900
      ENDIF
C
C        ALL ERRORS GENERATED BY THIS ROUTINE ARE FATAL.
      ISEVERE=2
C
      LOCN7_1=LOCN
      IPOS7_1=IPOS
C
C        UNPACK THE LENGTH OF THE SECTION, LSECT.
      CALL UNPKBG(KFILDO,IPACK,ND5,LOCN,IPOS,LSECT,32,N,IER,*900)
C
C        CHECK FOR AN UNEXPECTED END OF MESSAGE,
C        ACCOMMODATING FOR A 64-BIT WORD.
      IF(ENDOMESS(LSECT,N))THEN
         IER=799
         GO TO 900
      ENDIF
C
C        UNPACK THE NUMBER OF THE SECTION, NSECT. CHECK
C        TO MAKE SURE THAT THIS IS SECTION 7.
      CALL UNPKBG(KFILDO,IPACK,ND5,LOCN,IPOS,NSECT,8,N,IER,*900)
C
      IF(NSECT.NE.7)THEN
         IER=701
         LOCN=LOCN7_1
         IPOS=IPOS7_1
         GO TO 900
      ENDIF
C
      DO K=1,NS7
         IS7(K)=0
      ENDDO
C
      IS7(1)=LSECT
      IS7(5)=NSECT
C
      SELECT CASE (IUNPKOPT)
C
         CASE (0)
C
         CASE (1:2)
C
C              USE THE SIMPLE METHOD TO UNPACK THIS GRIB2 MESSAGE.
            CALL UNPK_SMPLE(KFILDO,A,ND2X3,NX,NY,IPACK,ND5,LOCN,IPOS,
     1                      IS5,NS5,IS7,NS7,IBITMAP,IB,IUNPKOPT,L3264B,
     2                      REF,XMISSP,IER,*900)
C
         CASE (3:6)
C
C              USE THE COMPLEX METHOD (WITH OR WITHOUT SECOND
C              ORDER SPATIAL DIFFERENCES) TO UNPACK THIS GRIB2 
C              MESSAGE.
            CALL UNPK_CMPLX(KFILDO,jmin,lbit,nov,iwork,A,ND2X3,NX,NY,
     1                      IPACK,ND5,LOCN,IPOS,IS5,NS5,IS5(32),
     2                      IBITMAP,IB,IUNPKOPT,BOUST,L3264B,REF,
     3                      XMISSP,XMISSS,IER,*900)
C
         CASE DEFAULT
C
C              THE METHOD OF UNPACKING IS NOT SUPPORTED.
            IER=709
      END SELECT
C
C        ERROR RETURN SECTION
 900  IF(IER.NE.0)RETURN 1
C
      RETURN
      END
