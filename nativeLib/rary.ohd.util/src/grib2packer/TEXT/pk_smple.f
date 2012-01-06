      SUBROUTINE PK_SMPLE(KFILDO,IA,NVAL,IPACK,ND5,LOCN,IPOS,IBIT,
     1                    L3264B,IER,*)
C
C        MARCH    2000   GLAHN   TDL   HP   FOR GRIB2 
C        JANUARY  2001   GLAHN   CHANGED ALGORITHM FOR COMPUTING IBIT;
C                                COMMENTS
C        NOVEMBER 2001   GLAHN   PUT * IN FRONT OF 900 IN LAST CALL
C                                TO PKBG
C        JANUARY  2002   GLAHN   ADDED ERROR IER = 706
C
C        PURPOSE
C            PACKS DATA AT "UNITS" RESOLUTION PROVIDED IN
C            IA( ) USING THE 'SIMPLE' PACKING METHOD DETAILED IN
C            THE GRIB2 WMO STANDARDS.
C
C            THE FOLLOWING EQUATION IS USED TO PACK THE DATA:
C               X1 = [(Y - R) * (2 ** -E) * (10 ** -D)]
C                    X1 = THE PACKED VALUE
C                     Y = THE VALUE WE ARE PACKING
C                     R = THE REFERENCE VALUE (FIRST ORDER MINIMA)
C                     E = THE BINARY SCALE FACTOR
C                     D = THE DECIMAL SCALE FACTOR
C            R HAS ALREADY BEEN REMOVED UPON ENTRY.
C
C
C        DATA SET USE
C           KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE. (OUTPUT) 
C
C        VARIABLES 
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT) 
C               IA(K) = DATA TO PACK (K=1,NVAL). (INPUT)
C                NVAL = NUMBER OF VALUES IN IA( ).  (INPUT)
C            IPACK(J) = THE ARRAY TO HOLD THE ACTUAL PACKED MESSAGE
C                       (J=1,MAX OF ND5).  (INPUT/OUTPUT)
C                 ND5 = DIMENSION OF IPACK( ).  (INPUT)
C                LOCN = THE WORD POSITION TO PLACE THE NEXT VALUE.
C                       (INPUT/OUTPUT)
C                IPOS = THE BIT POSITION IN LOCN TO START PLACING
C                       THE NEXT VALUE. (INPUT/OUTPUT)
C                IBIT = THE NUMBER OF BITS REQUIRED TO PACK EACH
C                       VALUE IN IA( ).  (OUTPUT) 
C              L3264B = CONTAINS THE NUMBER OF BITS IN A WORD
C                       IMPLEMENTED ON THIS PARTICULAR PLATFORM.
C                       (INPUT).
C                 IER = STATUS ERROR RETURN. (OUTPUT)
C                         0 = GOOD RETURN.
C                         1 = PACKING WOULD OVERFLOW IPACK( ).
C                         2 = IPOS NOT IN RANGE 1 TO L3264B.
C                         3 = NBIT NOT IN RANGE 0 TO 32.
C                         4 = NBIT EQ 0, BUT NVALUE NE 0.
C                       706 = VALUE WILL NOT PACK IN 30 BITS.
C                   * = ALTERNATE RETURN WHEN IER NE 0.
C
C         LOCAL VARIABLES
C               IFILL = NUMBER OF BITS TO PAD SECTION 7 TO A WHOLE
C                       OCTET.
C               IZERO = CONTAINS 0.
C
C        NON SYSTEM SUBROUTINES CALLED
C           PKBG, PK_S7
C
      DIMENSION IA(NVAL)
      DIMENSION IPACK(ND5)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2packer/RCS/pk_smple.f,v $
     . $',                                                             '
     .$Id: pk_smple.f,v 1.1 2004/09/16 16:52:29 dsa Exp $
     . $' /
C    ===================================================================
C
C
      DATA IZERO/0/
C
C        DETERMINE IBIT, THE NUMBER OF BITS REQUIRED TO PACK IA( ).
C        THE INITIAL LOOP IS TO SEE WHETHER THERE ARE ANY NON-ZERO
C        VALUES.  IF THERE AREN'T, THEN ONLY THE REFERENCE IS
C        NEEDED, AND THE NVAL POINTS ARE PACKED ZERO BITS EACH.
C
      IER=0
C
      DO 110 K=1,NVAL
         IF(IA(K).GT.0)GO TO 112
 110  CONTINUE
C
C        DROP THROUGH HERE MEANS ALL VALUES ARE ZERO.
C 
      IBIT=0
      GO TO 130 
C
 112  IBIT=1
      IBXX2=2
C
 114  DO 120 L=K,NVAL
         IF(IA(L).LT.IBXX2)GO TO 120
         IBIT=IBIT+1
C 
         IF(IBIT.GE.31)THEN
            IER=706
D           WRITE(KFILDO,115)IA(L),IER
D115        FORMAT(' ****ERROR IN PK_SMPLE.  VALUE ='I12,
D    1             ' WILL NOT PACK IN 30 BITS.  ERROR CODE =',I5)    
            GO TO 900
         ENDIF
C                 
         IBXX2=IBXX2*2
         GO TO 114
C
 120  CONTINUE      
C
C        PACK THE VALUES IN IA( ).
C
 130  CALL PK_S7(KFILDO,IPACK,ND5,LOCN,IPOS,IA,NVAL,IBIT,
     1           L3264B,IER,*900)
C
C        PAD WITH ZEROS TO FILL OUT AN OCTET, IF NECESSARY.
C
      IFILL=MOD(33-IPOS,8)
C
      IF(IFILL.NE.0)THEN
         CALL PKBG(KFILDO,IPACK,ND5,LOCN,IPOS,IZERO,IFILL,L3264B,
     1             IER,*900)
      ENDIF
C
 900  IF(IER.NE.0)RETURN1
C
      RETURN
      END
