      SUBROUTINE PK_S7(KFILDO,IPACK,ND5,LOCN,IPOS,IA,NVAL,IBIT,
     1                 L3264B,IER,*)
C
C        MAY     1997   GLAHN   TDL   HP
C        MARCH   2000   GLAHN   CHANGED NAME FROM PKS4LX;
C                               IC( ) TO IA( ); NXY TO NVAL;
C                               * = RETURN1
C        JANUARY 2001   GLAHN   COMMENTS; IER = 1 CHANGED TO 705;
C                               STANDARDIZED RETURN
C
C        PURPOSE
C            PACKS NVAL VALUES INTO IPACK( ).  THE PACKED VALUES
C            ARE TAKEN FROM IA( ).  PK_S7 ELIMINATES THE CALLING OF
C            PKBG, AND RATHER INCORPORATES IT INTO THE LOOP.
C            SINCE THIS IS A HIGHLY USED ROUTINE, ALL REASONABLE
C            ATTEMPTS AT EFFICIENCY MUST BE PURSUED.  THIS IS FOR
C            SIMPLE PACKING, THE COUNTERPART OF PK_C7 FOR COMPLEX
C            PACKING.  PK_S7 ACCOMMODATES IBIT = 0
C
C        DATA SET USE 
C           KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE. (OUTPUT) 
C
C        VARIABLES 
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT) 
C            IPACK(J) = THE ARRAY HOLDING THE ACTUAL PACKED MESSAGE
C                       (J=1,MAX OF ND5).  (INPUT/OUTPUT)
C                 ND5 = DIMENSION OF IPACK( ).  (INPUT)
C                LOCN = THE WORD POSITION TO PLACE THE NEXT VALUE.
C                       (INPUT/OUTPUT)
C                IPOS = THE BIT POSITION IN LOCN TO START PLACING
C               IA(K) = DATA TO PACK (K=1,NVAL).  (INPUT)
C                NVAL = DIMENSION OF IA( ).  THE NUMBER OF VALUES 
C                       TO BE PACKED.  (INPUT)
C                IBIT = THE NUMBER OF BITS USED TO PACK EACH VALUE.
C                       (INPUT)
C              L3264B = INTEGER WORD LENGTH OF MACHINE BEING USED.
C                       (INPUT)
C                 IER = ERROR RETURN.
C                          2 = IPOS NOT IN THE RANGE 1-L3264B.
C                          3 = IBIT NOT IN THE RANGE 0-32.
C                        705 = ND5 IS NOT LARGE ENOUGH TO ACCOMMODATE THE
C                              BITS NECESSARY TO PACK NVAL VALUES 
C                              STARTING AT THE VALUES LOCN AND IPOS.
C                       (OUTPUT)
C                   * = ALTERNATE RETURN WHEN IER NE 0.
C
C        LOCAL VARIABLES
C         IBIT1,IBIT2 = USED IN PACKING THE DATA USING MVBITS. THEY
C                       KEEP TRACK OF TEMPORARY BIT POSITIONS.
C             NEWIPOS = USED TO KEEP TRACK OF THE BIT POSITION TO
C                       START PACKING AT.
C
C        NON SYSTEM SUBROUTINES CALLED
C           NONE
C
      DIMENSION IPACK(ND5)
      DIMENSION IA(NVAL)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2packer/RCS/pk_s7.f,v $
     . $',                                                             '
     .$Id: pk_s7.f,v 1.1 2004/09/16 16:52:29 dsa Exp $
     . $' /
C    ===================================================================
C
C
      IER=0
C 
      IF(IBIT.EQ.0)GO TO 900
C        WHEN IBIT = 0, NO VALUES ARE PACKED.
C
C        CHECK LEGITIMATE VALUE OF IPOS.
C
      IF(IPOS.LE.0.OR.IPOS.GT.L3264B)THEN
         IER=2
D        WRITE(KFILDO,101)IPOS,IER
D101     FORMAT(/' IPOS = 'I6,' NOT IN THE RANGE 1 TO L3264B',
D    1           ' IN PK_S7.  RETURN FROM PK_S7 WITH IER = 'I4)
         GO TO 900 
      ENDIF
C
C        CHECK LEGITIMATE VALUE OF IBIT.
C
      IF(IBIT.LT.0.OR.IBIT.GT.32)THEN
         IER=3
D        WRITE(KFILDO,102)IBIT,IER
D102     FORMAT(/' IBIT = 'I6,' NOT IN THE RANGE 0 TO 32 IN PK_S7.',
D    1           ' RETURN FROM PK_S7 WITH IER = 'I4)
         GO TO 900
      ENDIF
C
C        CHECK WHETHER ND5 IS SUFFICIENT FOR ALL NVAL VALUES.
C
      IF(IBIT*NVAL.GT.(L3264B+1-IPOS)+(ND5-LOCN)*L3264B)THEN
         IER=705
D        WRITE(KFILDO,103)NVAL,IBIT,LOCN,IPOS,ND5,IER
D103     FORMAT(/' NVAL = 'I9,' AND IBIT = 'I6,' REQUIRE MORE BITS',
D    1           ' THAN ARE AVAILABLE IN IPACK( ),',
D    2           ' WITH LOCN ='I8,', IPOS ='I4,', AND ND5 ='I8,'.'/
D    3           ' RETURN FROM PK_S7 WITH IER ='I4)
         GO TO 900
      ENDIF
C     
      DO 300 K=1,NVAL 
C
      NEWIPOS=IPOS+IBIT
C
      IF(NEWIPOS.LE.L3264B+1)THEN
         CALL MVBITS(IA(K),0,IBIT,IPACK(LOCN),L3264B+1-NEWIPOS)
C
         IF(NEWIPOS.LE.L3264B)THEN
            IPOS=NEWIPOS
         ELSE
            IPOS=1
            LOCN=LOCN+1
         ENDIF
C
      ELSE
         IBIT1=L3264B+1-IPOS
         IBIT2=IBIT-IBIT1
         CALL MVBITS(IA(K),IBIT2,IBIT1,IPACK(LOCN),0)
         LOCN=LOCN+1
         CALL MVBITS(IA(K),0,IBIT2,IPACK(LOCN),L3264B-IBIT2)
         IPOS=IBIT2+1
      ENDIF
C
 300  CONTINUE
C
 900  IF(IER.NE.0)RETURN 1
C
      RETURN
      END
