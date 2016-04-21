      SUBROUTINE PACK_OPT(KFILDO,IA,IB,NXY,NVAL,ICLEAN,IBITMAP,
     1                    IS5,NS5,IS7,NS7,JMISSS,
     2                    MISSP,MINPK,NUMOCTET,IPKOPT,iwork,JER,
     3                    NDJER,KJER,*)
C
C        MAY      2000   LAWRENCE  ORIGINAL CODING - BASED ON LOGIC
C                                  DEVELOPED BY HARRY GLAHN.
C        JANUARY  2001   GLAHN     COMMENTS; XMISSP CHANGED TO
C                                  XMISSS IN DO 130 LOOP;
C        JANUARY  2001   GLAHN/LAWRENCE REMOVED UNUSED MISS, IS6( ),
C                                  AND NS6 FROM CALL; REMOVED
C                                  SETTING IS5(6)
C        NOVEMBER 2001   GLAHN     SEVERAL DIAGNOSTIC CALLS TO PK_TRACE 
C        JANUARY  2002   GLAHN     INSERTED IER = 906 and 907
C
C        PURPOSE
C            DETERMINES WHICH PACKING METHOD IS TO BE USED
C            TO PACK THE DATA.  THE USER HAS THE FIRST SAY
C            IN WHAT TYPE OF PACKING METHOD TO USE BY SETTING
C            THE APPROPRIATE VALUE IN IS5(10).  THIS ROUTINE
C            WILL THEN DETERMINE IF IT IS BENEFICIAL AND
C            POSSIBLE TO USE THE PACKING METHOD THAT THE USER
C            CHOSE.
C
C        DATA SET USE
C           KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE. (OUTPUT)
C
C        VARIABLE
C             KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C               IA(K) = CONTAINS THE INTEGER DATA FIELD.
C                       (K=1,NVAL).  (INPUT/OUTPUT)
C               IB(K) = THE BIT MAP WHEN ONE IS USED.  (INPUT/OUTPUT)
C                       IT CAN BE INPUT OR IN CAN BE CALCULATED IF
C                       THE SIMPLE METHOD IS USED (K=1,NXY).
C                       COMPLEX AND SPATIAL DIFFERENCING DO NOT
C                       USE A BIT MAP, BUT WILL ACCEPT ONE AND INSERT
C                       THE MISSING VALUES.
C                 NXY = DIMENSION OF IA( ) AND IB( ).  (INPUT)
C                NVAL = THE NUMBER OF VALUES IN IA( ).  (INPUT)
C              ICLEAN = 1 WHEN THERE ARE NO MISSING VALUES IA( ).
C                       0 OTHERWISE.  (INPUT)
C             IBITMAP = 1 WHEN THERE IS A BITMAP IN IB( , ).
C                       0 OTHERWISE.  (INPUT)
C              IS5(K) = THE VALUES ASSOCIATED WITH SECTION 5, KEYED
C                       TO THE OCTET NUMBER (K=1,NS5). (INPUT/OUTPUT)
C                 NS5 = THE DIMENSION OF IS5( ).  (INPUT)
C              IS7(K) = THE VALUES ASSOCIATED WITH SECTION 7, KEYED
C                       TO THE OCTET NUMBER (K=1,NS7).  (OUTPUT)
C                 NS7 = THE DIMENSION OF IS7( ). (INPUT)
C              JMISSS = .TRUE. IF THERE IS A SECONDARY MISSING
C                        VALUE IN THE DATA FIELD.  .FALSE. OTHERWISE
C                        (LOGICAL).  (INPUT)
C               MISSP = THE PRIMARY MISSING VALUE.  (INPUT)
C               MINPK = INCREMENT IN WHICH RANGES WILL BE COMPUTED.
C                       (INPUT)
C            NUMOCTET = THE MINIMUM NUMBER OF OCTETS REQUIRED TO
C                       PACK THE EXTRA DESCRIPTORS WHEN SECOND
C                       ORDER SPATIAL DIFFERENCING IS USED WITH
C                       COMPLEX PACKING.  (OUTPUT) 
C              IPKOPT = PACKING INDICATOR:
C                       0 = ERROR, DON'T PACK
C                       1 = PACK IA( ), SIMPLE
C                       2 = PACK IA( ) AND IB( ), SIMPLE
C                       3 = PACK COMPLEX OR SPATIAL DIFFERENCING
C                       4 = PACK COMPLEX.
C                       (OUTPUT)
C              JER(J) = ARRAY OF ERRORS (J=1,NDJER), MAX OF NDERR.
C                       906 = SIMPLE PACKING, NO MISSING VALUES IN
C                             ARRAY, BIT MAP PROVIDED, AND
C                             NVAL = NXY.  UNUSUAL; NOTIFY USER.
C                       907 = SIMPLE PACKING, NO MISSING VALUES IN
C                             ARRAY, NO BIT MAP PROVIDED, BUT 
C                             NVAL NE NXY, UNRECOVERABLE ERROR.
C                       908 = IS5(23) SET = 0 CONSISTENT WITH
C                             ICLEAN = 1 AND NOT SIMPLE PACKING.
C                       910 = IS5(23) SET = 1 BECAUSE THERE ARE
C                             NO SECONDARY MISSING VALUES IN FIELD.
C                       911 = IS5(10) SET = 2 TO INDICATE COMPLEX
C                             BECAUSE IT IS MORE EFFICIENT THAN
C                             SECOND ORDER DIFFERENCING.
C                       912 = IS5(23) SET = 2 TO INDICATE SECONDARY
C                             MISSING VALUES.
C                       915 = IS5(10) SET = 2 BECAUSE SECONDARY MISSING
C                             VALUES ARE PRESENT AND SECOND ORDER
C                             DIFFERENCING NOT SUPPORTED.
C                       (INPUT/OUTPUT)
C               NDJER = DIMENSION OF JER( ).  (INPUT)
C                KJER = NUMBER OF VALUES IN JER( ).  (INPUT/OUTPUT)
C                   * = ALTERNATE RETURN WHEN JER(KJER) GE 900.
C
C        LOCAL VARIABLES
C                 IER = CONTAINS ANY ERROR CODES GENERATED FROM
C                       UTILITIES CALLED BY THIS ROUTINE.
C                   K = AN ARRAY/LOOPING INDEX.
C               KOUNT = AN ARRAY INDEX THAT IS USED WHEN EITHER
C                       EXPANDING IA( ) TO PLACE MISSING VALUES IN
C                       IT OR GENERATING A BIT-MAP FROM IA( ).
C              SECOND = .TRUE. IF IT IS DETERMINED THAT SECOND
C                       ORDER DIFFERENCES WOULD BE EFFICIENT TO USE.
C                       .FALSE. IF IT IS DETERMINED THAT SECOND
C                       ORDER DIFFERENCES WOULD NOT BE EFFICIENT
C                       TO USE.
C
C        NON SYSTEM SUBROUTINES CALLED
C           PK_TRACE, PK_MISSP, PK_NOMISS
C
      LOGICAL JMISSS, SECOND
C
      DIMENSION IA(NXY),IB(NXY)
      DIMENSION IS5(NS5)
      DIMENSION IS7(NS7)
      DIMENSION JER(NDJER,2)
      DIMENSION IWORK(NXY)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2packer/RCS/pack_opt.f,v $
     . $',                                                             '
     .$Id: pack_opt.f,v 1.1 2004/09/16 16:52:29 dsa Exp $
     . $' /
C    ===================================================================
C
C
      DATA SECOND/.FALSE./
C
      IER=0
D     WRITE(KFILDO,100)ICLEAN,IBITMAP,IS5(10),NVAL
D100  FORMAT(/' IN PACK_OPT--ICLEAN,IBITMAP,IS5(10),NVAL',4I8)
C
      IF(ICLEAN.EQ.1)THEN
C
C           THERE ARE NO MISSING VALUES IN IA( ).
         IF(IBITMAP.EQ.1)THEN
C
C              THERE IS A BIT MAP.
            IF(IS5(10).EQ.0)THEN
C                 SIMPLE PACKING SPECIFIED.
C
               IF(NVAL.EQ.NXY)THEN
C                    NO MISSING VALUES IN ARRAY, BIT-MAP
C                    PROVIDED, SIMPLE, AND NVAL=NXY.
C                    UNUSUAL; NOTIFY USER.
C                    PACK IA( ).
                  IPKOPT=1
                  IER=906
                  CALL PK_TRACE(KFILDO,JER,NDJER,KJER,906,1)
               ELSE
C                    NO MISSING VALUES IN ARRAY, BIT MAP
C                    PROVIDED, SIMPLE, AND NVAL NE NXY.
C                    PACK IA( ) AND IB( ).
                  IPKOPT=2
               ENDIF
C
            ENDIF
         ELSE
            IF(IS5(10).EQ.0)THEN
C                 THERE IS NO BIT MAP
C
               IF(NVAL.EQ.NXY)THEN
C                     NO MISSING VALUES IN ARRAY, NO BIT 
C                     MAP PROVIDED, AND NVAL=NXY.  SIMPLE.
C                     PACK IA( )
                  IPKOPT=1
               ELSE
C                    NO MISSING VALUES IN ARRAY, NO BIT
C                    MAP PROVIDED, AND NVAL NE NXY.SIMPLE.
C                    UNRECOVERABLE ERROR, DO NOT PACK.
                  IPKOPT=0
                  IER=907
                  CALL PK_TRACE(KFILDO,JER,NDJER,KJER,907,2)
                  GO TO 900
               ENDIF
C
            ELSE IF(IS5(10).EQ.2)THEN
C                 COMPLEX PACKING SPECIFIED.
C
C                 NO MISSING VALUES IN THE ARRAY, NO BIT-MAP
C                 PROVIDED, PACK COMPLEX WITHOUT SECOND ORDER
C                 DIFFERENCES.
               IPKOPT=4
C
               IF(IS5(23).NE.0)THEN
                  IS5(23)=0
C                    NO MISSING VALUES IN DATA.
                  CALL PK_TRACE(KFILDO,JER,NDJER,KJER,908,1)
               ENDIF
C
            ELSE
C                 COMPLEX PACKING WITH 2ND ORDER SPATIAL DIFFERENCING.
C
C                 NO MISSING VALUES IN THE ARRAY, NO BIT-MAP
C                 PROVIDED, NOT SIMPLE. PACK COMPLEX WITH
C                 OR WITHOUT SPATIAL DIFFERENCES.
               CALL PK_NOMISS(KFILDO,IA,IB,NXY,MINPK,
     1                        IFIRST,ISECOND,IMINA,
     2                        NUMOCTET,SECOND,IER)
C
               IF(IER.NE.0)THEN
                  CALL PK_TRACE(KFILDO,JER,NDJER,KJER,IER,2)
                  GO TO 900
               ENDIF
C
               IF(SECOND)THEN
                  IPKOPT=3
                  IS7(6)=IFIRST
                  IS7(7)=ISECOND
                  IS7(8)=IMINA
               ELSE
                  IPKOPT=4
C
                  IS5(10)=2
C                    PACK COMPLEX WITHOUT 2ND ORDER DIFFERENCING.
                  CALL PK_TRACE(KFILDO,JER,NDJER,KJER,911,0)
               ENDIF
C
               IF(IS5(23).NE.0)THEN
                  IS5(23)=0
C                    NO MISSING VALUES IN DATA.
                  CALL PK_TRACE(KFILDO,JER,NDJER,KJER,908,1)
               ENDIF
C
            ENDIF
C
         ENDIF
C
      ELSE
C
C           AT THIS POINT, COMPLEX OR SPATIAL DIFFERENCE PACKING
C           IS TO BE DONE, NO BIT MAP.  JMISSP AND JMISSS HAVE BEEN 
C           SET TO INDICATE MISSING PRIMARY AND SECONDARY MISSING 
C           VALUES, RESPECTIVELY, AND THERE ARE AT LEAST PRIMARY
C           MISSING VALUES.  IF SECONDARY MISSING VALUES ARE PRESENT,
C           PACKING HAS TO CONSIDER PRIMARY MISSING VALUES ALSO.
C           JMISSP = 1.  SPATIAL DIFFERENCING IS NOT DONE WHEN
C           SECONDARY MISSING VALUES ARE PRESENT.
C
         IF(JMISSS)THEN
C
            IF(IS5(23).NE.2)THEN
               IS5(23)=2
C                 THERE ARE SECONDARY MISSING VALUES.
               CALL PK_TRACE(KFILDO,JER,NDJER,KJER,912,1)
            ENDIF
C
         ELSE
C
            IF(IS5(23).NE.1)THEN
               IS5(23)=1
C                 THERE ARE NO SECONDARY MISSING VALUES.
               CALL PK_TRACE(KFILDO,JER,NDJER,KJER,910,1)
            ENDIF
C
         ENDIF
C
         IF((IS5(10).EQ.2).OR.(JMISSS))THEN
C              PRIMARY AND SECONDARY MISSING VALUES IN ARRAY.
C              BIT MAP NOT PROVIDED; COMPLEX PACKING.  PACK IA( ).
            IPKOPT=4
C
            IF(IS5(10).EQ.3)THEN
               IS5(10)=2
               CALL PK_TRACE(KFILDO,JER,NDJER,KJER,915,1)
            ENDIF
C
         ELSE
C              PRIMARY MISSING VALUES IN ARRAY, BUT NO
C              SECONDARY MISSING VALUES.  BIT MAP NOT PROVIDED,
C              SPATIAL DIFFERENCE OR COMPLEX.  PACK IA( ).
            CALL PK_MISSP(KFILDO,IA,IB,iwork,NXY,MISSP,MINPK,
     1                    IFIRST,ISECOND,IMINA,NUMOCTET,
     2                    SECOND,IER)
C
            IF(IER.NE.0)THEN
               CALL PK_TRACE(KFILDO,JER,NDJER,KJER,IER,2)
               GO TO 900
            ENDIF
C
            IF(SECOND)THEN
               IPKOPT=3
               IS7(6)=IFIRST
               IS7(7)=ISECOND
               IS7(8)=IMINA
            ELSE
               IPKOPT=4
C
               IF(IS5(10).NE.2)THEN
                  IS5(10)=2
C                    PACK COMPLEX WITHOUT 2ND ORDER DIFFERENCING.
                  CALL PK_TRACE(KFILDO,JER,NDJER,KJER,911,0)
               ENDIF
C
            ENDIF
C
         ENDIF
C
      ENDIF
C
      RETURN
C
 900  RETURN 1
      END
