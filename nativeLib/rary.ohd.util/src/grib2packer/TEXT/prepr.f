      SUBROUTINE PREPR(KFILDO,A,IA,IB,NX,NY,NVAL,ICLEAN,IBITMAP,
     1                 IS5,NS5,IS6,NS6,IS7,NS7,ID,IE,MINA,
     2                 XMINA,MISSP,MISSS,XMISSP,XMISSS,MINPK,
     3                 IPKOPT,iwork,IER,JER,NDJER,KJER,*)
C
C        MARCH    2000   GLAHN   CALLED BY PK_GRIB2
C        MAY      2000   LAWRENCE MODIFIED ROUTINE TO REFLECT
C                                THE LATEST WMO GRIB2 CHANGES.
C                                THE MOST SIGNIFICANT CHANGE
C                                IS THAT THE VALUES ARE FIRST
C                                SCALED BY THE DECIMAL SCALE
C                                FACTOR, THEN THE MINIMUM IS
C                                TAKEN FROM THE DATA FIELD, AND
C                                THEN THE VALUES ARE MULTIPLIED
C                                BY THE BINARY SCALE FACTOR.
C        JANUARY  2001   GLAHN   WRITE(KFILDO) MADE /D; COMMENTS
C        JANUARY  2001   GLAHN   COMMENT FOLLOWING CALL TO PACK_OPT;
C                                ELIMINATED IS5( ) AND NS5 IN CALL TO
C                                PREP_INT AND PREP_FLT
C        JANUARY  2001   GLAHN/LAWRENCE REMOVED UNUSED MISS AND XMISS;
C                                ELIMINATED IS6( ) AND NS6 FROM CALL
C                                TO PACK_OPT
C        NOVEMBER 2001   GLAHN   ADDED JER, NDJER, AND KJER TO CALL
C                                TO CHECK_INT, CHECK_FLT, INT_MAP,
C                                AND FLT_MAP
C        DECEMBER 2001   GLAHN   ADDED KFILDO TO CALL TO CHECK_INT
C                                AND CHECK_FLT
C        DECEMBER 2001   GLAHN   MOVED TEST ON IS5(10) = 0, 2, OR 3 
C                                FROM PK_SECT5.
C        JANUARY  2002   GLAHN   ADDED IER AND *900 TO CALLS TO 
C                                PREP_INT AND PREP_FLT; CHANGED
C                                NVAL COMMENT FROM INPUT TO OUTPUT
C        FEBRUARY 2002   GLAHN   COMMENTS
C
C        PURPOSE
C            FINDS THE REFERENCE VALUE AND SUBTRACTS IT FROM THE
C            DATA VALUES.  EITHER FLOATING OR INTEGER DATA ARE
C            HANDLED, AND WHEN THERE ARE OR AREN'T MISSING VALUES.
C            A BIT MAP IS GENERATED IF NECESSARY, AND VALUES ARE
C            INSERTED INTO THE GRID FOR COMPLEX AND SPATIAL
C            DIFFERENCING.  OPERATIONS WITHIN LOOPS ARE KEPT
C            TO A RELATIVE MINIMUM AT THE EXPENSE OF MORE CODE
C            FOR EFFICIENCY.  SECOND ORDER DIFFERENCING IS NOT
C            DONE WHEN THERE ARE SECONDARY MISSING VALUES PRESENT.
C
C            ALL COMPUTATIONS ARE DONE ON INTEGER DATA UNTIL
C            THEY ARE SCALED WHEN INCOMING DATA ARE INTEGER IN IA( ),
C            THEN PUT BACK IN IA( ).
C            ALL COMPUTATIONS ARE DONE ON FLOATING POINT DATA
C            WHEN INCOMING DATA ARE FLOATING POINT IN A( ), THEN.
C            PUT IN IA( ).
C
C        DATA SET USE
C           KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE. (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C                A(K) = WHEN IS5(21) = 0, A( ) CONTAINS THE DATA
C                       (K=1,NVAL).  (INPUT)
C               IA(K) = WHEN IS5(21) = 1, IA( ) CONTAINS THE DATA
C                       (K=1,NVAL).  THE VALUES TO PACK ARE IN
C                       IA( ) ON OUTPUT.  (INPUT/OUTPUT)
C               IB(K) = THE BIT MAP WHEN ONE IS USED.  (INPUT/OUTPUT)
C                       IT CAN BE INPUT OR IN CAN BE CALCULATED IF
C                       THE SIMPLE METHOD IS USED (K=1,NXY).
C                       COMPLEX AND SPATIAL DIFFERENCING DO NOT
C                       USE A BIT MAP, BUT WILL ACCEPT ONE AND INSERT
C                       THE MISSING VALUES.
C               NX,NY = DIMENSIONS OF THE GRID.  NX*NY IS THE DIMENSION
C                       OF A( ), IA( ), AND IB( ).  (INPUT)
C                NVAL = THE NUMBER OF VALUES IN A( ) OR IA( ).  (OUTPUT)
C              ICLEAN = 1 WHEN THERE ARE NO MISSING VALUES IN A( ) OR
C                         IA( ).
C                       0 OTHERWISE.
C                       (INPUT/OUTPUT)
C             IBITMAP = 1 WHEN THERE IS A BITMAP IN IB( ).
C                       0 OTHERWISE.
C                       (INPUT)
C              IS5(J) = THE VALUES ASSOCIATED WITH SECTION 5, KEYED
C                       TO THE OCTET NUMBER (J=1,NS5).  THE ELEMENTS
C                       USED IN THIS ROUTINE ARE:
C                       IS5(10), TEMPLATE NUMBER:
C                         0 = SIMPLE
C                         1 = NOT SUPPORTED
C                         2 = COMPLEX
C                         3 = SPATIAL DIFFERENCING
C                       IS5(21), TYPE OF ORIGINAL FIELD:
C                         0 = FLOATING POINT
C                         1 = INTEGER
C                 NS5 = THE DIMENSION OF IS5( ).  (INPUT)
C              IS6(J) = THE VALUES ASSOCIATED WITH SECTION 6, KEYED
C                       TO THE OCTET NUMBER.  THE ELEMENTS USED
C                       IN THIS ROUTINE ARE:
C                       IS6(6) = BIT MAP INDICATOR:
C                       0 = BIT MAP INCLUDED
C                       255 = NO BIT MAP
C                       IS6(6) IS MODIFIED ONLY AS NEEDED.  A VALUE
C                       BETWEEN 1 AND 254 IS NOT DISTURBED
C                       (J=1,NS6). (INPUT/OUTPUT)
C                 NS6 = THE DIMENSION OF IS6( ).  (INPUT)
C                  ID = THE DECIMAL SCALING FACTOR.  (INPUT)
C                  IE = THE BINARY SCALING FACTOR.  (INPUT)
C                MINA = THE FIELD MINIMUM VALUE WHEN THE ORIGINAL DATA
C                       ARE INTEGER.  (OUTPUT)
C               XMINA = THE FIELD MINIMUM VALUE WHEN THE ORIGINAL DATA
C                       ARE FLOATING POINT.  (OUTPUT)
C               MISSP = WHEN MISSING POINTS CAN BE PRESENT IN THE DATA,
C                       THEY WILL HAVE THE VALUE MISSP OR MISSS WHEN
C                       THE DATA ARE INTEGER.  MISSP IS THE PRIMARY
C                       MISSING VALUE WHEN THE ORIGINAL DATA ARE
C                       INTEGER.  (INPUT)
C               MISSS = SECONDARY MISSING VALUE INDICATOR WHEN THE DATA
C                       ARE INTEGER.  (INPUT)
C              XMISSP = WHEN MISSING POINTS CAN BE PRESENT IN THE DATA,
C                       THEY WILL HAVE THE VALUE XMISSP OR XMISSS WHEN
C                       THE DATA ARE FLOATING POINT.  XMISSP
C                       IS THE PRIMARY MISSING VALUE.  (INPUT)
C              XMISSS = SECONDARY MISSING VALUE INDICATOR WHEN THE DATA
C                       ARE FLOATING POINT.  (INPUT)
C               MINPK = INCREMENT IN WHICH RANGES WILL BE COMPUTED.
C                       (INPUT)
C              IPKOPT = PACKING INDICATOR:
C                       0 = ERROR, DON'T PACK
C                       1 = PACK IA( ), SIMPLE
C                       2 = PACK IA( ) AND IB( ), SIMPLE
C                       3 = PACK COMPLEX OR SPATIAL DIFFERENCING
C                       4 = PACK COMPLEX.
C                       (OUTPUT)
C                 IER = RETURN STATUS.  (OUTPUT)
C                       0 = GOOD ERROR RETURN.
C                     902 = THERE ARE NO "GOOD" VALUES IN THE GRID
C                           AND THE BIT-MAP INDICATES THAT.
C                     903 = THERE ARE NO VALUES IN THE GRID AND THE
C                           BIT-MAP INDICATES THAT THERE SHOULD BE.
C                     904 = THERE ARE NO VALUES IN THE GRID, AND THERE
C                           IS NO BIT-MAP.
C                     905 = INVALID DATA TYPE INDICATED IN
C                           IS5(21).
C                     906 = NO MISSING VALUES IN THE ARRAY, BIT-MAP
C                           PROVIDED, SIMPLE, AND NVAL=NXY. (WARNING)
C                     907 = NO MISSING VALUES IN THE ARRAY, NO
C                           BIT-MAP PROVIDED, AND NVAL NE NXY WHILE
C                           PACKING SIMPLE.
C                     508 = UNSUPPORTED PACKING TYPE IN IS5(10).
C               NDJER = DIMENSION OF JER( ).  (INPUT)
C                KJER = NUMBER OF VALUES IN JER( ).  (INPUT/OUTPUT)
C                   * = ALTERNATE RETURN WHEN JER GE 900.
C
C        LOCAL VARIABLES
C               CFEED = CONTAINS THE CHARACTER REPRESENTATION
C                       OF A PRINTER FORM FEED.
C               IFEED = CONTAINS THE INTEGER VALUE OF A PRINTER
C                       FORM FEED.
C                 NXY = NX*NY.
C        1         2         3         4         5         6         7 X
C
C        NON SYSTEM SUBROUTINES CALLED
C           BOUSTRO,CHECK_FLT,CHECK_INT,PK_TRACE,FLT_MAP,
C           INT_MAP,PACK_OPT,PREP_FLT,PREP_INT,PREP_NOVAL
C
      CHARACTER*1 CFEED
      LOGICAL JMISSS,JMISSP
C
      DIMENSION A(NX*NY),IA(NX*NY),IB(NX*NY)
      DIMENSION IS5(NS5),IS6(NS6),IS7(NS7)
      DIMENSION JER(NDJER,2)
      DIMENSION IWORK(NX*NY)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2packer/RCS/prepr.f,v $
     . $',                                                             '
     .$Id: prepr.f,v 1.1 2004/09/16 16:52:29 dsa Exp $
     . $' /
C    ===================================================================
C
C
      DATA IFEED/12/
C
      IER=0
      NXY=NX*NY
      IPKOPT=0
      CFEED=CHAR(IFEED)
C
      IF(IS5(10).NE.0.AND.IS5(10).NE.2.AND.IS5(10).NE.3)THEN
C           CHECKS PACKING TYPE; MUST BE 1, 2, OR 3.
         IER=508
         GO TO 900
      ENDIF
C
C        PROCESS THE BIT-MAP, IF ONE WAS SUPPLIED.
      IF(IS5(21).EQ.1)THEN
C           THIS IS INTEGER DATA.
         CALL INT_MAP(IA,IB,NXY,IS5,NS5,ICLEAN,
     1                IBITMAP,MISSP,iwork,JER,NDJER,KJER)
      ELSEIF(IS5(21).EQ.0)THEN
C           THIS IS FLOATING POINT DATA.
         CALL FLT_MAP(A,IB,NXY,IS5,NS5,ICLEAN,
     1                IBITMAP,XMISSP,iwork,JER,NDJER,KJER)
      ELSE
C
C           THE DATA ARE NEITHER INTEGER NOR FLOATING POINT.
C           THE TYPE OF DATA IS NOT LEGITIMATE.
C           ERRORS MAY RESULT IF PACKING CONTINUES.
         CALL PK_TRACE(KFILDO,JER,NDJER,KJER,905,2)
         IPKOPT=0
         GO TO 900
      ENDIF
C
C        PROCESS THE MISSING VALUES, DETERMINING IF THERE
C        ARE NO MISSING VALUES, PRIMARY MISSING VALUES
C        OR SECONDARY MISSING VALUES IN THE FIELD.
C
      IF(IS5(21).EQ.1)THEN
         CALL CHECK_INT(KFILDO,IA,IB,NVAL,NXY,IS5,NS5,ICLEAN,IBITMAP,
     1                  MISSP,MISSS,JMISSP,JMISSS,IER,
     2                  JER,NDJER,KJER,*900)
C           THIS IS INTEGER DATA.
      ELSEIF(IS5(21).EQ.0)THEN
         CALL CHECK_FLT(KFILDO,A,IB,NVAL,NXY,IS5,NS5,ICLEAN,IBITMAP,
     1                  XMISSP,XMISSS,JMISSP,JMISSS,IER,
     2                  JER,NDJER,KJER,*900)
C           THIS IS FLOATING POINT DATA.
      ELSE
         IER=909
         GO TO 900
      ENDIF
C
C        ARE THERE ANY VALUES IN THIS DATA FIELD?
      IF(NVAL.EQ.0)THEN
         CALL PREP_NOVAL(IB,NXY,IBITMAP,IPKOPT,IER)
C
C           WAS A FATAL ERROR ENCOUNTERED?
         IF(IER.EQ.902)THEN
            CALL PK_TRACE(KFILDO,JER,NDJER,KJER,IER,1)
C              IER = 902 IS NOT FATAL.
         ELSEIF(IER.NE.0)THEN
            GO TO 900
         ENDIF
      ELSE
C
C           DETERMINE THE TYPE OF THE DATA.
         IF(IS5(21).EQ.1)THEN
C
C              THE DATA ARE INTEGER.
C              IF THE COMPLEX OR COMPLEX WITH SPATIAL DIFFERENCES
C              PACKING METHOD IS BEING USED, THEN SCAN THE
C              DATA BOUSTROPHEDONICALLY.
C
            IF((IS5(10).EQ.2).OR.(IS5(10).EQ.3))THEN
               CALL BOUSTRO_INT(IA,NX,NY)
            ENDIF
C
            CALL PREP_INT(IA,NXY,NVAL,ICLEAN,ID,IE,
     1                    MINA,JMISSP,JMISSS,MISSP,MISSS,IER,*900)
         ELSE
C
C              THE DATA ARE FLOATING POINT.
C              IF THE COMPLEX OR COMPLEX WITH SPATIAL DIFFERENCES
C              PACKING METHOD IS BEING USED, THEN SCAN THE
C              DATA BOUSTROPHEDONICALLY.
C
            IF((IS5(10).EQ.2).OR.(IS5(10).EQ.3))THEN
               CALL BOUSTRO_FLT(A,NX,NY)
            ENDIF
C
            CALL PREP_FLT(A,IA,NXY,NVAL,ICLEAN,ID,IE,
     1                    XMINA,JMISSP,JMISSS,XMISSP,XMISSS,IER,*900)
D           WRITE(KFILDO,10)CFEED
D10         FORMAT(A1,/' **********************'
D    1                /' ORIGINAL SCALED VALUES'
D    2                /' **********************')
D           WRITE(KFILDO,20) (IA(J),J=1,200)
D20         FORMAT(/' '20I6)
C
         ENDIF
C
      ENDIF
C
C        SET IPKOPT TO A PROPER VALUE IF IT HAS NOT ALREADY BEEN
C        DONE SO IN PREP_NOVAL.
      IF(IPKOPT.EQ.0)THEN
         CALL PACK_OPT(KFILDO,IA,IB,NXY,NVAL,ICLEAN,IBITMAP,
     1                 IS5,NS5,IS7,NS7,JMISSS,MISSP,
     2                 MINPK,NUMOCTET,IPKOPT,iwork,JER,NDJER,
     3                 KJER,*900)
C           PACK_OPT MAY HAVE A NORMAL RETURN WITH A NON FATAL
C           IER.  THIS WILL HAVE BEEN INSERTED INTO JER( , ).
C           PREPR WILL RETURN TO CALLING PROGRAM WITH THAT IER.
C           THIS IS A LITTLE DANGEROUS, BUT THE CALLING PROGRAM
C           PK_GRIB2 JUST CALLS ANOTHER ROUTINE THAT SETS IER = 0.
D        WRITE(KFILDO,30)CFEED,IS7(8)
D30      FORMAT(A1,/' ***************************'
D    1             /' 2ND ORDER DIFFERENCES AFTER'
D    2             /' THE REMOVAL OF THE FIELD'
D    3             /' MINIMUM ',I6,
D    4             /' ***************************')
D        WRITE(KFILDO,40) (IA(J),J=1,200)
D40      FORMAT(/' '20I6)
      ENDIF
C
C        INITIALIZE THE PERTINENT VALUES IN IS5( ) DEPENDING
C        ON THE PACKING OPTION.
 700  IF(IPKOPT.EQ.1)THEN
         IS5(1)=21
         IS5(6)=NVAL
         IS5(10)=0
         IS6(6)=255
      ELSEIF(IPKOPT.EQ.2)THEN
         IS5(1)=21
         IS5(6)=NVAL
         IS5(10)=0
         IS6(6)=0
      ELSEIF(IPKOPT.EQ.3)THEN
         IS5(1)=49
         IS5(6)=NXY
         IS5(10)=3
         IS5(48)=2
         IS5(49)=NUMOCTET
         IS6(6)=255
      ELSEIF(IPKOPT.EQ.4)THEN
         IS5(1)=47
         IS5(6)=NXY
         IS5(10)=2
         IS6(6)=255
      ENDIF
      RETURN
C
 900  RETURN 1 
      END
