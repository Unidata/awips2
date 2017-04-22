      SUBROUTINE PK_C7(KFILDO,IPACK,ND5,LOCN,IPOS,IA,NXY,NOV,LBIT,LX,
     1                 L3264B,NCOUNT,IER,*)
C
C        MAY     1997   GLAHN    TDL   HP
C        JULY    1999   LAWRENCE UPDATED THIS ROUTINE SO THAT IT RETURNS A 
C                                COUNT OF THE VALUES PACKED INTO IPACK.
C                                THIS IS NEEDED BY PK_CMPLX AND IT IS
C                                SHARING THIS ROUTINE WITH PK52. 
C        AUGUST  1999   LAWRENCE UPDATED THE DOCUMENTATION IN THIS ROUTINE 
C                                IN KEEPING WITH TDL STANDARDS.
C        MARCH   2000   GLAHN    UPDATED FOR GRIB2; NAME CHANGED FROM;
C                                PKC4LX; CHANGED LOC TO LOCN
C        JANUARY 2001   GLAHN    COMMENTS; IER = 1 CHANGED TO IER = 705;
C                                ADDED RETURN1; CHANGED IER FROM 932 TO 2
C                                ETC.
C
C        PURPOSE 
C            PACKS UP TO NXY VALUES INTO IPACK( ).  THE PACKED VALUES
C            ARE TAKEN FROM IA( ) WITH NO REFERENCE VALUE OR 
C            SCALING CONSIDERED.  PK_C7 ELIMINATES THE CALLING OF
C            PKBG, AND RATHER INCORPORATES IT INTO THE LOOP.
C            SINCE THIS IS A HIGHLY USED ROUTINE, ALL REASONABLE
C            ATTEMPTS AT EFFICIENCY MUST BE PURSUED.  THIS IS
C            FOR COMPLEX PACKING, THE COUNTERPART OF PK_S7 FOR
C            SIMPLE PACKING.
C
C        DATA SET USE 
C           KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE. (OUTPUT) 
C
C        VARIABLES 
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT) 
C            IPACK(J) = THE ARRAY HOLDING THE ACTUAL PACKED MESSAGE
C                       (J=1,MAX OF ND5).  (INPUT/OUTPUT)
C                 ND5 = DIMENSION OF IPACK( ).  (INPUT)
C                LOCN = HOLDS WORD POSITION IN IPACK OF NEXT VALUE TO
C                       PACK.  (INPUT/OUTPUT)
C                IPOS = HOLDS BIT POSITION IN IPACK(LOCN) OF THE FIRST
C                       BIT OF THE NEXT VALUE TO PACK.  (INPUT/OUTPUT)
C               IA(K) = DATA TO PACK (K=1,NXY).  (INPUT)
C                 NXY = DIMENSION OF IA( ).  ALSO, THE NUMBER OF VALUES 
C                       TO BE PACKED UNLESS ALL MEMBERS OF A GROUP
C                       HAVE THE SAME VALUE.  (INPUT)
C              NOV(K) = THE NUMBER OF VALUES PER GROUP (K=1,LX).(INPUT)
C             LBIT(K) = THE NUMBER OF BITS TO PACK FOR EACH GROUP
C                       (K=1,LX).  (INPUT)
C                  LX = THE NUMBER OF VALUES IN NOV( ) AND LBIT( ).
C                       (INPUT)
C              L3264B = INTEGER WORD LENGTH OF MACHINE BEING USED.
C                       (INPUT)
C              NCOUNT = THE NUMBER OF VALUES PACKED. THIS VALUE IS
C                       INITIALLY ZEROED OUT IN THIS ROUTINE.
C                       (OUTPUT) 
C                 IER = ERROR RETURN.
C                         2 = IPOS NOT IN THE RANGE 1-L3264B.
C                         3 = LBIT( ) NOT IN THE RANGE 0-32.
C                       705 = ND5 IS NOT LARGE ENOUGH TO ACCOMMODATE THE
C                             BITS NECESSARY TO PACK THE VALUES STARTING
C                             AT THE VALUES LOCN AND IPOS.
C                       (OUTPUT)
C                   * = ALTERNATE RETURN WHEN IER NE 0.
C
C        LOCAL VARIABLES
C                   K = THE INDEX OF THE CURRENT DATA ITEM WE ARE 
C                       PACKING.
C                 L,M = LOOP INDEXING VARIABLE.
C               LBITL = THE NUMBER OF BITS REQUIRED TO PACK THE
C                       LARGEST VALUE IN EACH GROUP.
C       LBITL1,LBITL2 = USED IN PACKING THE DATA USING MVBITS. THEY
C                       KEEP TRACK OF TEMPORARY BIT POSITIONS.
C             NEWIPOS = USED TO KEEP TRACK OF THE BIT
C                       POSITION TO START PACKING AT.
C
C        NON SYSTEM SUBROUTINES CALLED 
C           NONE
C
      DIMENSION IPACK(ND5)
      DIMENSION IA(NXY)
      DIMENSION NOV(LX),LBIT(LX)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2packer/RCS/pk_c7.f,v $
     . $',                                                             '
     .$Id: pk_c7.f,v 1.1 2004/09/16 16:52:29 dsa Exp $
     . $' /
C    ===================================================================
C
C
C        SET ERROR RETURN.
C
      IER=0
      NCOUNT=0
C
C        CHECK LEGITIMATE VALUES OF LOCN AND IPOS.
C
      IF(IPOS.LE.0.OR.IPOS.GT.L3264B)THEN
         IER=2
D        WRITE(KFILDO,101)IPOS,IER
D101     FORMAT(/' IPOS = 'I6,' NOT IN THE RANGE 1 TO L3264B IN PK_C7.',
D    1           ' RETURN FROM PK_C7 WITH IER = 'I4)
         GO TO 900 
      ENDIF
C
      K=0
C
      DO 300 L=1,LX
C
      LBITL=LBIT(L)
C
C        CHECK LEGITIMATE VALUES OF LBIT(L) AND WHETHER
C        ND5 IS SUFFICIENT FOR ALL NOV(L) VALUES.
C
      IF(LBITL.LT.0.OR.LBITL.GT.32)THEN
         IER=3
D        WRITE(KFILDO,102)LBITL,L,IER
D102     FORMAT(/' LBIT(L) = 'I6,' FOR L ='I5,'NOT IN THE RANGE',
D    1           ' 0 TO 32 IN PK_C7.',
D    1           ' RETURN FROM PK_C7 WITH IER = 'I4)
         GO TO 900
      ENDIF
C
      IF(LBITL*NOV(L).GT.(L3264B+1-IPOS)+(ND5-LOCN)*L3264B)THEN
         IER=705
D        WRITE(KFILDO,103)NOV(L),LBITL,L,LOCN,IPOS,ND5,IER
D103     FORMAT(/' NOV(L) = 'I9,' AND LBIT(L) = 'I6,' FOR L ='I5,
D    1           ' REQUIRE MORE BITS THAN ARE AVAILABLE IN IPACK( ),',
D    2           ' WITH LOCN ='I8,', IPOS ='I4,', AND ND5 ='I8,'.'/
D    3           ' RETURN FROM PK_C7 WITH IER ='I4)
         GO TO 900
      ENDIF
C
      DO 290 M=1,NOV(L)
      K=K+1
      IF(LBITL.EQ.0)GO TO 290
C        A GROUP WITH ALL VALUES THE SAME IS OMITTED.
C
      NCOUNT=NCOUNT+1
      NEWIPOS=IPOS+LBITL
C
      IF(NEWIPOS.LE.L3264B+1)THEN
         CALL MVBITS(IA(K),0,LBITL,IPACK(LOCN),L3264B+1-NEWIPOS)
C
         IF(NEWIPOS.LE.L3264B)THEN
            IPOS=NEWIPOS
         ELSE
            IPOS=1
            LOCN=LOCN+1
         ENDIF
C
      ELSE
         LBITL1=L3264B+1-IPOS
         LBITL2=LBITL-LBITL1
         CALL MVBITS(IA(K),LBITL2,LBITL1,IPACK(LOCN),0)
         LOCN=LOCN+1
         CALL MVBITS(IA(K),0,LBITL2,IPACK(LOCN),L3264B-LBITL2)
         IPOS=LBITL2+1
      ENDIF
C
 290  CONTINUE
C
 300  CONTINUE
C
 900  IF(IER.NE.0)RETURN1
C
      RETURN
      END
