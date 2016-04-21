      SUBROUTINE PK_SECT7(KFILDO,IA,NXY,IPACK,ND5,LOCN,IPOS,
     1                    IS5,NS5,IS6,NS6,IS7,NS7,INC,MINPK,MISSP,MISSS,
     2                    IPKOPT,LOCN5_20,IPOS5_20,LOCN5_32,
     3                    IPOS5_32,L3264B,jmax,jmin,lbit,nov,misslx,
     4                    newbox,newboxp,IER,ISEVERE,*)
C
C        MARCH    2000   GLAHN   TDL   FOR GRIB2
C        JANUARY  2001   GLAHN   COMMENTS; ADDED CHECK ON SIZE OF
C                                IS5( ) AND ON IS6(5) = 6; REMOVED
C                                IS7(5) = 7; ADDED IER = 703 FOR
C                                A TYPE OF PACKING ERROR
C        NOVEMBER 2001   GLAHN   A FEW DIAGNOSTIC FORMAT CHANGES;
C                                SPLIT TEST ON NS7 AND IS7(5)
C        JANUARY  2002   GLAHN   SET SECTION NUMBER = 99 IN PACKED
C                                DATA WHEN SEVERE ERROR OCCURS
C        FEBRUARY 2002   GLAHN   ADDED SECTIONS 6,7 PRINT FOR SIMPLE
C        MARCH    2002   GALHN   ADDED NON FATAL ERROR RETURNS VIA
C                                IERSAV; ADDED IER=0
C
C        PURPOSE
C            PACKS DATA SECTION 7, THE DATA SECTION, OF A GRIB2
C            MESSAGE.
C
C        DATA SET USE
C           KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE. (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE. (INPUT)
C               IA(J) = THE DATA TO PACK (J=1,NXY).  (INPUT)
C                 NXY = THE SIZE OF IA( ).  (INPUT)
C            IPACK(J) = THE ARRAY THAT HOLDS THE ACTUAL PACKED MESSAGE
C                       (J=1,ND5). (INPUT/OUTPUT)
C                 ND5 = THE SIZE OF THE ARRAY IPACK( ). (INPUT)
C                LOCN = THE WORD POSITION TO PLACE THE NEXT VALUE.
C                       (INPUT/OUTPUT)
C                IPOS = THE BIT POSITION IN LOCN TO START PLACING
C                       THE NEXT VALUE. (INPUT/OUTPUT)
C              IS5(J) = CONTAINS THE GRID DEFINITION DATA THAT
C                       WILL BE PACKED INTO IPACK( ) (J=1,NS5).
C                       (INPUT/OUTPUT)
C                 NS5 = SIZE OF IS5( ). (INPUT)
C              IS6(J) = CONTAINS THE BIT MAP INFORMATION 
C                       CORRESPONDING TO SECTION 6 OF GRIB2 (J=1,NS6).
C                       (INPUT)
C                 NS6 = SIZE OF IS6( ). (INPUT)
C              IS7(J) = CONTAINS THE GRID DEFINITION DATA THAT
C                       WILL BE PACKED INTO IPACK( ) (J=1,NS7).
C                       (INPUT/OUTPUT)
C                 NS7 = SIZE OF IS7( ). (INPUT)
C                 INC = THE INCREMENT TO USE IN DEFINING GROUPS.
C                       (INPUT)
C               MINPK = THE MINIMUM GROUP SIZE.  (INPUT)
C               MISSP = THE PRIMARY MISSING VALUE.  (INPUT)
C               MISSS = THE SECONDARY MISSING VALUE.  (INPUT)
C              IPKOPT = PACKING INDICATOR:
C                       0 = ERROR, DON'T PACK
C                       1 = PACK IA( ), SIMPLE
C                       2 = PACK IA( ) AND IB( ), SIMPLE
C                       3 = PACK COMPLEX OR SPATIAL DIFFERENCING
C                       4 = PACK COMPLEX.
C                      (INPUT)
C           LOCN5_20 = LOCN FOR OCTET 20 IN SECTION 5.  (INPUT)
C           IPOS5_20 = IPOS FOR OCTET 20 IN SECTION 5.  (INPUT)
C           LOCN5_32 = LOCN FOR OCTET 32 IN SECTION 5.  (INPUT)
C           IPOS5_32 = IPOS FOR OCTET 32 IN SECTION 5.  (INPUT)
C              L3264B = THE INTEGER WORD LENGTH IN BITS OF THE MACHINE
C                       BEING USED. VALUES OF 32 AND 64 ARE
C                       ACCOMMODATED. (INPUT)
C                 IER = RETURN STATUS CODE. (OUTPUT)
C                          0 = GOOD RETURN.
C                          1 = PACKING WOULD OVERFLOW IPACK( ).
C                          2 = IPOS NOT IN RANGE 1 TO L3264B.
C                          3 = NBIT NOT IN RANGE 0 TO 32.
C                          4 = NBIT EQ 0, BUT NVALUE NE 0.
C                        701 = IS7(5) DOES NOT INDICATE SECTION 7.
C                        702 = IS7( ) HAS NOT BEEN DIMENSIONED LARGE ENOUGH
C                              TO CONTINUE SECTION 7.
C                        703 = NOT SUPPORTED TYPE OF PACKING.
C                        711 = LBIT INCORRECT.  RETURNED FROM PK_CMPLX.
C                        712 = INCORRECT SPLITTING METHOD.  RETURNED
C                              FROM PK_CMPLX.
C                        713 = UNRECOGNIZED MISSING VALUE FLAG
C                              IN IS5(23).  RETURNED FROM PK_CMPLX.
C             ISEVERE = THE SEVERITY LEVEL OF THE ERROR.
C                       1 = NON FATAL ERROR
C                       2 = A FATAL ERROR 
C                       (OUTPUT)
C                   * = ALTERNATE RETURN WHEN IER NE 0.
C
C             LOCAL VARIABLES
C               CFEED = CONTAINS THE CHARACTER REPRESENTATION
C                       OF A PRINTER FORM FEED.
C               IFEED = CONTAINS THE INTEGER VALUE OF A PRINTER
C                       FORM FEED.
C               IZERO = CONTAINS THE VALUE '0'.  THIS IS USED IN THE
C                       CODE SIMPLY TO EMPHASIZE THAT A CERTAIN 
C                       GROUP OF OCTETS IN THE MESSAGE ARE BEING 
C                       ZEROED OUT.
C                   N = L3264B = THE INTEGER WORD LENGTH IN BITS OF
C                       THE MACHINE BEING USED. VALUES OF 32 AND
C                       64 ARE ACCOMMODATED.
C                IBIT = THE NUMBER OF BITS TO USE TO PACK EACH VALUE
C                       FOR SIMPLE PACKING.  FOR COMPLEX PACKING, IBIT
C                       IS THE BITS TO USE TO PACK THE GROUP MINIMA.
C                       IS5(20) SET TO IBIT.
C               IFILL = NUMBER OF BITS NECESSARY TO FILL OUT AN OCTET.
C         LOCN7,IPOS7 = WORD AND BIT POSITION OF BEGINNNING OF
C                       SECTION 7.
C        1         2         3         4         5         6         7 X
C
C        NON SYSTEM SUBROUTINES CALLED
C           PKBG, PK_SMPLE, PK_CMPLX, LENGTH
C
      CHARACTER*1 CFEED
C
      DIMENSION IA(ND5),IPACK(ND5),IS5(NS5),IS6(NS6),IS7(NS7)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2packer/RCS/pk_sect7.f,v $
     . $',                                                             '
     .$Id: pk_sect7.f,v 1.1 2004/09/16 16:52:29 dsa Exp $
     . $' /
C    ===================================================================
C
C
      DATA IFEED/12/
      DATA IZERO/0/
C
      IER=0
      IERSAV=0
      N=L3264B
      CFEED=CHAR(IFEED)
C
      LOCN7=LOCN
      IPOS7=IPOS
C
C        ALL ERRORS GENERATED BY THIS ROUTINE WHERE THE ALTERNATE 
C        RETURN IS USED ARE FATAL.  NON FATAL ERRORS USE THE 
C        NORMAL RETURN.
      ISEVERE=2
C        IERSAV CAN SAVE AN ERROR RETURN FROM ANOTHER SUBROUTINE.
C
C        CHECK TO MAKE SURE THAT DATA HAS BEEN PROVIDED FOR
C        SECTION 7.
C
      IF(NS7.LT.5)THEN
         IER=702
         GO TO 900
      ELSE
C 
         IF(IS7(5).NE.7)THEN
            IER=701
            GO TO 900
         ENDIF
C
C           BYTES 1-4 MUST BE FILLED IN LATER WITH THE RECORD LENGTH
C           IN BYTES; ABOVE STATEMENT HOLDS THE PLACE.  LOCN7 AND 
C           IPOS7 HOLD THE LOCATION.
C
         CALL PKBG(KFILDO,IPACK,ND5,LOCN,IPOS,IZERO,32,N,IER,*900)
C
         LOCNS=LOCN
         IPOSS=IPOS
C           SAVE LOCN AND IPOS IN CASE OF ERROR, SECTION NUMBER IS
C           SET TO 99.
         CALL PKBG(KFILDO,IPACK,ND5,LOCN,IPOS,IS7(5),8,N,IER,*900)
C
C           DETERMINE THE PACKING METHOD THAT WE WILL
C           USE.
C
         IF(IPKOPT.EQ.1.OR.IPKOPT.EQ.2)THEN
C
C              THIS IS THE SIMPLE METHOD.  THERE ARE IS5(6) VALUES
C              TO PACK.
C
            CALL PK_SMPLE(KFILDO,IA,IS5(6),IPACK,ND5,LOCN,IPOS,IBIT,
     1                       N,IER,*900)
         ELSEIF(IPKOPT.EQ.3.OR.IPKOPT.EQ.4)THEN
C
C              THIS IS THE COMPLEX METHOD OR SPATIAL DIFFERENCE.
C
            CALL PK_CMPLX(KFILDO,IA,NXY,IPACK,ND5,LOCN,IPOS,
     1                    IS5,NS5,IS7,NS7,INC,MINPK,MISSP,
     2                    MISSS,IBIT,LOCN5_32,IPOS5_32,N,jmax,
     3                    jmin,lbit,nov,misslx,newbox,newboxp,
     4                    IER,*900)
            IERSAV=IER
            IER=0
C              IERSAV CAN BE USED TO PROVIDE ERROR TRACING.
C              SUBROUTINE REDUCE CAN PRODUCE NON FATAL ERRORS.
C              OTHER ERRORS ARE FATAL AND RETURN IS TO *900.
            
         ELSE
            IER=703
C              NOT RECOGNIZED TYPE OF PACKING.
            GO TO 900
         ENDIF
C
C              FILL IS5(20) WITH IBIT.  THIS CAN COME FROM
C              EITHER PK_SMPLE OR PK_CMPLX.
C
         IS5(20)=IBIT
         CALL PKBG(KFILDO,IPACK,ND5,LOCN5_20,IPOS5_20,IBIT,8,N,IER,
     1             *900)
C
C           PAD WITH ZEROS TO FILL OUT AN OCTET, IF NECESSARY.
C
         IFILL=MOD(33-IPOS,8)
C
         IF(IFILL.NE.0)THEN
            CALL PKBG(KFILDO,IPACK,ND5,LOCN,IPOS,IZERO,IFILL,N,IER,
     1                *900)
         ENDIF
C
         IS7(1)=LENGTH(KFILDO,IPACK,ND5,N,LOCN7,IPOS7,LOCN,
     1                 IPOS,IER)
C
C           WRITE OUT THE CONTENTS OF SECTION 5 AND THE NON-DATA
C           CONTENTS OF SECTION 7.  SEPARATE SIMPLE, COMPLEX AND
C           COMPLEX WITH SECOND ORDER DIFFERENCES
C
         IF(IS5(10).EQ.0)THEN
D           WRITE(KFILDO,5)
D5          FORMAT(/' *******************************************'
C***D           WRITE(KFILDO,5)CFEED
C***D5          FORMAT(A1,/' *******************************************'
D    1                /' DATA VALUES FOR SECTIONS 5, 6 - SIMPLE PACKING'
D    2                /' *******************************************')
D           WRITE(KFILDO,6)IS5(1),IS5(5),IS5(6),IS5(10),IS5(12),IS5(16),
D    1                     IS5(18),IS5(20),IS5(21),IS6(6),IS6(1)
D6          FORMAT(/' 1:LENGTH OF SECTION ',T56,I10,
D    1      /' 5:NUMBER OF SECTION',T60,I6,
D    2      /' 6:NUMBER OF ACTUAL DATA POINTS ',T56,I10,
D    3      /' 10:DATA REPRESENTATION TEMPLATE NUMBER ',T60,I6,
D    4      /' 12:REFERENCE VALUE ',T54,I12,
D    5      /' 16:BINARY SCALE FACTOR ',T60,I6,
D    6      /' 18:DECIMAL SCALE FACTOR ',T60,I6,
D    7      /' 20:BITS FOR EACH PACKED VALUE',T60,I6,
D    8      /' 21:TYPE OF VALUES ',T60,I6,
D    9      /'  6:BITMAP INDICATOR (SECTON 6) ',T60,I6,
D    A      /'  1:LENGTH OF SECTION 6 ',T56,I10)
C
D        ELSEIF(IS5(10).EQ.2)THEN           
D           WRITE(KFILDO,10)
D10         FORMAT(/' *******************************************'
C***D           WRITE(KFILDO,10)CFEED
C***D10         FORMAT(A1,/' *******************************************'
D    1                /' DATA VALUES FOR SECTION 5 - COMPLEX PACKING'
D    3                /' *******************************************')
D           WRITE(KFILDO,20)IS5(1),IS5(5),IS5(6),IS5(10),IS5(12),
D    1                      IS5(16),IS5(18),IS5(20),IS5(21),IS5(22),
D    2                      IS5(23),IS5(24),IS5(28),IS5(32),IS5(36),
D    3                      IS5(37),IS5(38),IS5(42),IS5(43),IS5(47)
D20         FORMAT(/' 1:LENGTH OF SECTION ',T56,I10,
D    1      /' 5:NUMBER OF SECTION',T60,I6,
D    2      /' 6:NUMBER OF ACTUAL DATA POINTS ',T56,I10,
D    3      /' 10:DATA REPRESENTATION TEMPLATE NUMBER ',T60,I6,
D    4      /' 12:REFERENCE VALUE ',T60,I6,
D    5      /' 16:BINARY SCALE FACTOR ',T60,I6,
D    6      /' 18:DECIMAL SCALE FACTOR ',T60,I6,
D    7      /' 20:BITS TO PACK GROUP REFERENCES',T60,I6,
D    8      /' 21:TYPE OF VALUES ',T60,I6,
D    9      /' 22:SPLITTING METHOD ',T60,I6,
D    A      /' 23:USE OF MISSING VALUES ',T60,I6,
D    B      /' 24:PRIMARY MISSING VALUE ',T60,I6,
D    C      /' 28:SECONDARY MISSING VALUE ',T60,I6,
D    D      /' 32:NUMBER OF GROUPS ',T58,I8,
D    E      /' 36:REFERENCE FOR GROUP WIDTHS ',T60,I6,
D    F      /' 37:BITS TO PACK GROUP WIDTHS ',T60,I6,
D    G      /' 38:REFERENCE FOR GROUP LENGTHS ',T60,I6,
D    H      /' 42:LENGTH INCREMENT FOR GROUP LENGTHS ',T60,I6,
D    I      /' 43:TRUE LENGTH OF LAST GROUP. ',T58,I8,
D    J      /' 47:BITS TO PACK GROUP LENGTHS ',T60,I6)
C
         ELSEIF(IS5(10).EQ.3)THEN
D           WRITE(KFILDO,10)
D15         FORMAT(/' *******************************************'
C***D           WRITE(KFILDO,10)CFEED
C***D15         FORMAT(A1,/' *******************************************'
D    1                /' DATA VALUES FOR SECTION 5 - COMPLEX PACKING'
D    2                /' WITH SECOND ORDER DIFFERENCES'
D    3                /' *******************************************')
D           WRITE(KFILDO,20)IS5(1),IS5(5),IS5(6),IS5(10),IS5(12),
D    1                      IS5(16),IS5(18),IS5(20),IS5(21),IS5(22),
D    2                      IS5(23),IS5(24),IS5(28),IS5(32),IS5(36),
D    3                      IS5(37),IS5(38),IS5(42),IS5(43),IS5(47)
         ENDIF
C
C           WRITE IS5(48) AND IS(49) THAT ONLY PERTAIN TO SECOND ORDER
C           DIFFERENCING.
C
D        IF(IS5(10).EQ.3)THEN
D           WRITE(KFILDO,25)IS5(48),IS5(49)
D25         FORMAT(' 48:ORDER OF SPATIAL DIFFERENCING ',T60,I6,
D    1      /' 49:FIELD WIDTH OF SPATIAL DESCRIPTORS ',T60,I6)
D        ENDIF
C
D        IF(IS5(10).EQ.2)THEN
D           WRITE(KFILDO,30)
D30         FORMAT(/' *******************************************'
C***D           WRITE(KFILDO,30)CFEED
C***D30         FORMAT(A1,/' *******************************************'
D    1                /' DATA VALUES FOR SECTION 7 - COMPLEX PACKING'
D    3                /' *******************************************')
D           WRITE(KFILDO,40)IS7(1),IS7(5)
D40         FORMAT(/' 1:THE NUMBER OF OCTETS IN THIS FIELD ',T57,I9,
D    1             /' 5:THE NUMBER OF THIS SECTION',T60,I6)
D        ELSEIF(IS5(10).EQ.3)THEN
D           WRITE(KFILDO,45)
D45         FORMAT(/' *******************************************'
C***D           WRITE(KFILDO,45)CFEED
C***D45         FORMAT(A1,/' *******************************************'
D    1                /' DATA VALUES FOR SECTION 7 - COMPLEX PACKING'
D    2                /' WITH SECOND ORDER DIFFERENCES'
D    3                /' *******************************************')
D           WRITE(KFILDO,50)IS7(1),IS7(5),IS7(6),IS7(7),IS7(8)
D50         FORMAT(/' 1:THE NUMBER OF OCTETS IN THIS FIELD ',T57,I9,
D    1             /' 5:THE NUMBER OF THIS SECTION',T60,I6,
D    2             /' 6:THE FIRST ORIGINAL VALUE IN THE FIELD ',T58,I8,
D    3             /' 7:THE SECOND ORIGINAL VALUE IN THE FIELD ',T58,I8,
D    4             /' 8:THE OVERALL MIN OF THE 2ND ORDER DIFF ',T58,I8)
D        ELSEIF(IS5(10).EQ.0)THEN
D           WRITE(KFILDO,47)
D47         FORMAT(/' *******************************************'
C***D           WRITE(KFILDO,47)CFEED
C***D47         FORMAT(A1,/' *******************************************'
D    1                /' DATA VALUES FOR SECTION 7 - SIMPLE PACKING'
D    3                /' *******************************************')
D           WRITE(KFILDO,55)IS7(1),IS7(5)
D55         FORMAT(/' 1:THE NUMBER OF OCTETS IN THIS FIELD ',T57,I9,
D    1             /' 5:THE NUMBER OF THIS SECTION',T60,I6)
C
D        ENDIF
C
      ENDIF
C
C         ERROR RETURN SECTION
 900  IF(IER.NE.0)THEN
C
         IF(ISEVERE.EQ.2)THEN
C              FOR A SEVERE ERROR, THE SECTION NUMBER IS SET = 99
C              SO THAT UNPACKING WILL NOT OCCUR.
            IERS=IER
            CALL PKBG(KFILDO,IPACK,ND5,LOCNS,IPOSS,99,8,N,IER,*900)
            IER=IERS
            RETURN 1
         ENDIF
C  
      ENDIF
C
      IF(IERSAV.NE.0)THEN
         IER=IERSAV
         ISEVERE=1
C           THIS PROVIDES FOR NON FATAL ERROR RETURNS. 
      ENDIF     
C
      RETURN
      END
