C MEMBER PRP53
C-----------------------------------------------------------------------
C
C@PROCESS LVL(77)
C
      SUBROUTINE PRP53 (P)

C     THIS IS THE PRINT PARAMETER ROUTINE FOR SSARR 3-VARIABLE LOOKUP

C     THIS ROUTINE ORIGINALLY WRITTEN BY 
C        RAY FUKUNAGA - NWRFC  AUGUST 1995    

C     ADDED CAPABILITY TO PRINT MULTI VALUE DATA TYPES
C       DARRIN SHARP, RIVERSIDE TECHNOLOGY AUG 2007

C     POSITION   CONTENTS OF P ARRAY
C      1         VERSION NUMBER OF OPERATION
C      2-19      DESCRIPTION - TITLE
C     20         # OF POINTS IN THE P ARRAY

C     21-22      1ST INDEPENDENT VARIABLE (X) TIME SERIES IDENTIFIER
C     23         1ST INDEPENDENT VARIABLE (X) TIME SERIES DATA TYPE CODE
C     24         1ST INDEPENDENT VARIABLE (X) TIME SERIES TIME INTERVAL
C                (DECIMAL PORTION HOLDS MULTIVALUE INDEX WHEN APPLICABLE)

C     25-26      2ND INDEPENDENT VARIABLE (Z) TIME SERIES IDENTIFIER
C     27         2ND INDEPENDENT VARIABLE (Z) TIME SERIES DATA TYPE CODE
C     28         2ND INDEPENDENT VARIABLE (Z) TIME SERIES TIME INTERVAL
C                (DECIMAL PORTION HOLDS MULTIVALUE INDEX WHEN APPLICABLE)

C     29-30      RESULTANT (Y) TIME SERIES IDENTIFIER
C     31         RESULTANT (Y) TIME SERIES DATA TYPE CODE
C     32         RESULTANT (Y) TIME SERIES TIME INTERVAL

C                Z SEGMENTS (Z1,X11,Y11,X12,Y12,Z2,X21,Y21,X22,Y22,ETC)
C     33         NUMBER OF POINTS IN THE Z SEGMENT ARRAY
C     34         THE UNITS THAT THE USER ENTERS THE Z SEGMENT ARRAY
C                'ENGL' OR 'METR' (DEFAULT)
C     35+        Z SEGMENT ARRAY

C     THEREFORE THE NUMBER OF ELEMENTS REQUIRED IN THE P ARRAY IS
C        34 + NUMBER OF POINTS IN THE Z SEGMENT ARRAY


      DIMENSION P(*)

      CHARACTER *  8  XMVDT,ZMVDT
      CHARACTER *  4  CXMET,CZMET,CYMET
      CHARACTER *  4  CXENG,CZENG,CYENG
      CHARACTER *  4  CXDIM,CZDIM,CYDIM
      CHARACTER *  4  CUNIT,CTSCAL

      INTEGER         PINT1,PINT2

C     INT REPRESENTATIONS OF THE WHOLE NUMBER PORTION
C     OF THE VALUES IN P(24) and P(28). THE DECIMAL
C     PORTION MAY CONTAIN A MULTIVALUE INDEX
      INTEGER 	      P24INT,P28INT

      REAL            PREAL(19)
      CHARACTER*4     PCHAR(19)
      EQUIVALENCE    (PREAL(1),PCHAR(1))

      EQUIVALENCE (CUNIT,RUNIT)

C     COMMON BLOCKS

      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/pda/users/sharpd/devl/src/fcinit_prpc/myRCS/prp53.f,v $
     . $',                                                             '
     .$Id: prp53.f,v 1.2 2007/06/27 00:24:36 sharpd Exp $
     . $' /
C    ===================================================================
C
C
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C        SSARR 3-VARIALBE LOOKUP - VERSION XXX
C        ST MARIES (CLDI ROUTED COEI HF > ST MARIES STAGE)            
C
C        ---------------------------------------------------------------
C
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C                                                                 TIME  
C                                                  ID     CODE  INTERVAL
C    1ST INDEPENDENT VARIABLE TIME SERIES (X)   XXXXXXXX  XXXX     XXX
C    2ND INDEPENDENT VARIABLE TIME SERIES (Z)   XXXXXXXX  XXXX     XXX
C
C    MULTIVALUE DATA TYPE FOR TIME SERIES (X)   XXXXXXXX  (OPTIONAL)
C    MULTIVALUE DATA TYPE FOR TIME SERIES (Z)   XXXXXXXX  (OPTIONAL)
C
C    RESULTANT TIME SERIES (Y)                  XXXXXXXX  XXXX     XXX
C
C    Z SEGMENT ARRAY (Z1,X11,Y11,X12,Y12,Z2,X21,Y21,X22,Y22,ETC)
C    X - 1ST INDEPENDENT VARIABLE
C    Z - 2ND INDEPENDENT VARIABLE
C    Y - RESULTANT
C  
C    IN ENGL UNITS
C
C        2         3         4         5         6         7
C2345678901234567890123456789012345678901234567890123456789012
C  Z         X1         Y1         X2         Y2         X3         Y3
CXX.XX XXXXXXX.XX XXXXXXX.XX XXXXXXX.XX XXXXXXX.XX XXXXXXX.XX XXXXXXX.XX

C     CHECK TRACE LEVEL 
      CALL FPRBUG ('PRP53   ',1,53,IBUG)
      IF (IBUG.EQ.1) WRITE(IODBUG,500)
 500  FORMAT(1H0,10X,'DEBUG PRINT PARAMETER ROUTINE FOR SSARR ',
     +               '3-VARIABLE LOOKUP OPERATION ')

C     PRINT HEADING THEN TITLE FROM P ARRAY
        PINT1 = P(1)
        DO 23 II=2,19
   23   PREAL(II) = P(II)
      WRITE(IPR,501) PINT1,(PCHAR(II),II=2,19)
 501  FORMAT(//,6X,'SSARR 3-VARIABLE LOOKUP OPERATION - VERSION ',I3,/,
     +          6X,18A4,/,
     +          6X,76('-'),/)

        PREAL(1) = P(21)
        PREAL(2) = P(22)
        PREAL(3) = P(23)
           P24INT = INT(P(24))
        PREAL(4) = P(25)
        PREAL(5) = P(26)
        PREAL(6) = P(27)
           P28INT = INT(P(28))
        PREAL(7) = P(29)
        PREAL(8) = P(30)
        PREAL(9) = P(31)
           PINT2 = P(32)
        PREAL(10) = P(33)
        NUMVLX=NINT((P(24)-P24INT)*10)
        NUMVLZ=NINT((P(28)-P28INT)*10)
      WRITE(IPR,5022) PCHAR(1),PCHAR(2),PCHAR(3),P24INT
      WRITE(IPR,5023) PCHAR(4),PCHAR(5),PCHAR(6),P28INT
      IF (NUMVLX.GT.0) THEN
         CALL CVTIDX(PCHAR(3),NUMVLX,XMVDT)
         WRITE(IPR,5026) XMVDT
      ENDIF
      IF (NUMVLZ.GT.0) THEN
         CALL CVTIDX(PCHAR(6),NUMVLZ,ZMVDT)
         WRITE(IPR,5027) ZMVDT
      ENDIF
      WRITE (IPR,5024) PCHAR(7),PCHAR(8),PCHAR(9),PINT2,PCHAR(10)
 5022 FORMAT(//,
     +      6X,T67,'TIME',/,
     +      6X,T50,'ID       CODE  INTERVAL',/,
     +      6X,'1ST INDEPENDENT VARIABLE TIME SERIES (X)',     
     +         T49,2A4,2X,A4,5X,I3)
 5023 FORMAT(
     +      6X,'2ND INDEPENDENT VARIABLE TIME SERIES (Z)',    
     +         T49,2A4,2X,A4,5X,I3,/)
 5024 FORMAT(/,
     +      6X,'RESULTANT TIME SERIES (Y)',
     +         T49,2A4,2X,A4,5X,I3,//,
     +      6X,'Z SEGMENT ARRAY (Z1,X11,Y11,X12,Y12,',
     +         'Z2,X21,Y21,X22,Y22,ETC)',/,
     +      6X,'X - 1ST INDEPENDENT VARIABLE',/,
     +      6X,'Z - 2ND INDEPENDENT VARIABLE',/,
     +      6X,'Y - RESULTANT',//,
     +      6X,'UNITS IN ',A,//,
     +      T14,'Z',T24,'X1',T35,'Y1',T46,'X2',T57,'Y2',T68,'X3',
     +          T79,'Y3')
 5026 FORMAT(
     +      6X,'MULTIVALUE DATA TYPE FOR TIME SERIES (X)  ',A8)
 5027 FORMAT(
     +      6X,'MULTIVALUE DATA TYPE FOR TIME SERIES (Z)  ',A8)


      NP = NINT(P(20))
      IC = 35
      IF (IBUG.GE.1) WRITE(IODBUG,35) NP
 35   FORMAT('PRP53: NUMBER OF POINT IN P ARRAY: ',I5)

      IF (PCHAR(10).EQ.'ENGL') THEN 
         CALL FDCODE (P(23),CXMET,CXDIM,IMISS,NVAL,CTSCAL,NADD,IERFLG)
         IF (IERFLG.NE.0) THEN
            IERROR = 1
            PREAL(1) = P(23)
            WRITE(IPR,3100) PCHAR(1)
 3100       FORMAT(//,10X,'*** ERROR *** ERROR IN FDCODE CALL FOR TS ',/
     +                    'CODE: ',A)
         ENDIF
         CALL FCONVT (CXMET,CXDIM,CXENG,RXMF,RXCF,IERFLG)
         IF (IERFLG.NE.0) THEN
            IERROR = 1
            WRITE(IPR,3105) CXMET,CXDIM
 3105       FORMAT(//,10X,'*** ERROR *** ERROR IN FCONVT CALL FOR ',
     +                    'METRIC UNITS AND DIMENSION: ',A,1X,A)
         ENDIF
         CALL FDCODE (P(27),CZMET,CZDIM,IMISS,NVAL,CTSCAL,NADD,IERFLG)
         IF (IERFLG.NE.0) THEN
            IERROR = 1
            PREAL(1) = P(27)
            WRITE(IPR,3100) PCHAR(1)
         ENDIF
         CALL FCONVT (CZMET,CZDIM,CZENG,RZMF,RZCF,IERFLG)
         IF (IERFLG.NE.0) THEN
            IERROR = 1
            WRITE(IPR,3105) CZMET,CZDIM
         ENDIF
         CALL FDCODE(P(31),CYMET,CYDIM,IMISS,NVAL,CTSCAL,NADD,IERFLG)
         IF  (IERFLG.NE.0) THEN
            IERROR = 1
            PREAL(1) = P(31)
            WRITE(IPR,3100) PCHAR(1)
         ENDIF
         CALL FCONVT (CYMET,CYDIM,CYENG,RYMF,RYCF,IERFLG)
         IF (IERFLG.NE.0) THEN
            IERROR = 1
            WRITE(IPR,3105) CYMET,CYDIM
         ENDIF

10001    IF (IC.GT.NP) GO TO 10009
            
            IF (P(IC).LE.-999.) THEN
               Z = P(IC)
            ELSE
               Z  = P(IC)*RZMF + RZCF
            ENDIF

            IF (P(IC+1).LE.-999.) THEN
               X1 = P(IC+1)
            ELSE
               X1 = P(IC+1)*RXMF + RXCF
            ENDIF

            IF (P(IC+2).LE.-999.) THEN
               Y1 = P(IC+2)
            ELSE
               Y1 = P(IC+2)*RYMF + RYCF
            ENDIF

            IF (P(IC+3).LE.-999.) THEN
               X2 = P(IC+3)
            ELSE
               X2 = P(IC+3)*RXMF + RXCF
            ENDIF

            IF (P(IC+4).LE.-999.) THEN
               Y2 = P(IC+4)
            ELSE
               Y2 = P(IC+4)*RYMF + RYCF
            ENDIF

            IF (P(IC+5).LE.-999.) THEN
               X3 = P(IC+5)
            ELSE
               X3 = P(IC+5)*RXMF + RXCF
            ENDIF

            IF (P(IC+6).LE.-999.)  THEN
               Y3 = P(IC+6)
            ELSE
               Y3 = P(IC+6)*RYMF + RYCF
            ENDIF

            WRITE(IPR,5030) Z,X1,Y1,X2,Y2,X3,Y3

            IC = IC + 7
10008       GO TO 10001
10009    CONTINUE

      ELSE

11001    IF (IC.GT.NP) GO TO 11009
      
            WRITE(IPR,5030) (P(ID),ID=IC,IC+6)
 5030       FORMAT(6X,7(F10.2,1X))
         
            IC = IC + 7

11008       GO TO 11001
11009    CONTINUE
      ENDIF

      WRITE(IPR,5031) 
 5031 FORMAT(/,
     +      6X,76('-'),//)                       

      IF (ITRACE.GE.1) WRITE(IODBUG,199)
 199  FORMAT('PRP53:  EXITED:')

      RETURN
      END
