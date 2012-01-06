C MEMBER PRP52
C-----------------------------------------------------------------------
C
C@PROCESS LVL(77)
C
      SUBROUTINE PRP52 (P)

C     THIS IS THE PRINT PARAMETER ROUTINE FOR SSARR SUMMING POINT

C     THIS ROUTINE ORIGINALLY WRITTEN BY 
C        RAY FUKUNAGA - NWRFC   JUNE 1995     

C     POSITION     CONTENTS OF P ARRAY
C      1           VERSION NUMBER OF OPERATION
C      2-19        DESCRIPTION - TITLE
C     20           # OF INPUT TIME SERIES TO SUM

C     21-22        BEGIN INTERVAL OUTPUT TIME SERIES IDENTIFIER
C     23           BEGIN INTERVAL OUTPUT TIME SERIES DATA TYPE CODE
C     24           BEGIN INTERVAL OUTPUT TIME SERIES TIME INTEVAL
C
C     25-26        END INTERVAL OUTPUT TIME SERIES IDENTIFIER
C     27           END INTERVAL OUTPUT TIME SERIES DATA TYPE CODE
C     28           END INTERVAL OUTPUT TIME SERIES TIME INTERVAL

C     FOR EACH INPUT TIME SERIES TO BE SUMMED
C     29-30        INPUT TIME SERIES IDENTIFIER
C     31           INPUT TIME SERIES DATA TYPE CODE
C     32           INPUT TIME SERIES TIME INTERVAL
C     33           CARRYOVER FLAG
C                  = 'CARY', FROM CARRYOVER ARRAY
C                  = 'FLAT', SET EQUAL TO SECOND ELEMENT
C                  = '    ', SET EQUAL TO ZERO
C                  = 'VALU', READ IN FROM INPUT

C     THEREFORE THE NUMBER OF ELEMENTS REQUIRED IN THE P ARRAY IS
C        28 +
C         5 * NUMBER OF INPUT TIME SERIES TO BE SUMMED

      DIMENSION P(*)
      INTEGER      PINT1,PINT2
      REAL         PREAL(19)
      CHARACTER*4  PCHAR(19)
      EQUIVALENCE (PREAL(1),PCHAR(1))

C     COMMON BLOCKS

      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp52.f,v $
     . $',                                                             '
     .$Id: prp52.f,v 1.1 1996/03/21 14:33:39 page Exp $
     . $' /
C    ===================================================================
C

C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C        SSARR SUMMING POINT - VERSION XXXX
C        SUM OF 3 TIME SERIES - 1 BEGIN AND 1 END AND 1 SINGLE T.S.
C
C        NUMBER OF INFLOW TIME SERIES TO BE SUMMED =    3
C        ------------------------------------------------
C
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C                                                    TIME    CARRYOVER
C                                     ID     CODE  INTERVAL     FLAG
C        BEGIN OUTFLOW TIME SERIES XXXXXXXX  XXXX     XX    
C        END   OUTFLOW TIME SERIES XXXXXXXX  XXXX     XX
C
C        INPUT TIME SERIES
C    XXX END   INFLOW TIME SERIES  XXXXXXXX  XXXX     XX   
C    XXX BEGIN INFLOW TIME SERIES  XXXXXXXX  XXXX     XX  
C    XXX END   INFLOW TIME SERIES  XXXXXXXX  XXXX     XX        XXXX
C
C        CARRYOVER VALUES
C         TIME       CARRYOVER
C        SERIES        VALUE
C         XXX        XXXXXXX.XX

C     CHECK TRACE LEVEL 
      CALL FPRBUG ('PRP52   ',1,52,IBUG)
      IF (IBUG.EQ.1) WRITE(IODBUG,500)
 500  FORMAT(1H0,10X,'DEBUG PRINT PARAMETER ROUTINE FOR SSARR ',
     +               'SUMPT OPERATION')

C     PRINT HEADING THEN TITLE FROM P ARRAY
      PINT1 = P(1)
      DO 494 I=2,19
  494 PREAL(I) = P(I)
      WRITE(IPR,501) PINT1,(PCHAR(I),I=2,19)
 501  FORMAT(//,10X,'SSARR SUMMING POINT - VERSION ',I4,/,
     +          10X,18A4,/)

      PINT1 = P(20)
      WRITE(IPR,502) PINT1
 502  FORMAT(10X,'NUMBER OF INFLOW TIME SERIES TO BE SUMMED =',I5 ,/,
     +       10X,'------------------------------------------------')

      PREAL(1) = P(21)
      PREAL(2) = P(22)
      PREAL(3) = P(23)
      PINT1 = P(24)
      PREAL(4) = P(25)
      PREAL(5) = P(26)
      PREAL(6) = P(27)
      PINT2 = P(28)
      WRITE(IPR,5022) (PCHAR(I),I=1,3),PINT1,(PCHAR(I),I=4,6),PINT2
 5022 FORMAT(//,
     +     10X,T54,'TIME',/,
     +     10X,T39,'ID     CODE  INTERVAL',/,
     +     10X,'BEGIN OUTFLOW TIME SERIES ',2A4,1X,A4,4X,I3,/,
     +     10X,'END   OUTFLOW TIME SERIES ',2A4,1X,A4,4X,I3//)

      WRITE(IPR,5023) 
 5023 FORMAT(/,
     +     10X,T54,'TIME    CARRYOVER',/,
     +     10X,T39,'ID     CODE  INTERVAL     FLAG')  

      DO 100 I=1,INT(P(20))

         WRITE(IPR,5025) I
 5025    FORMAT(10X,'INPUT TIME SERIES ',I4)
           J = 5*I
           PREAL(1) = P(J+24)
           PREAL(2) = P(J+25)
           PREAL(3) = P(J+26)
           PINT1 = P(J+27)
           PREAL(4) = P(J+28)
         IF (PCHAR(4) .EQ. 'SQIB') THEN
            WRITE(IPR,503) (PCHAR(J),J=1,3),PINT1,PCHAR(4)
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
 503        FORMAT(10X,'BEGIN INFLOW  TIME SERIES ',
     +                 2A4,1X,A4,4X,I3,8X,A4)
         ELSE
            WRITE(IPR,504) (PCHAR(J),J=1,3),PINT1,PCHAR(4)
 504        FORMAT(10X,'END   INFLOW  TIME SERIES ',
     +                 2A4,1X,A4,4X,I3,8X,A)
         ENDIF

 100  CONTINUE

      IF (ITRACE.GE.1) WRITE(IODBUG,199)
 199  FORMAT('PRP52:  EXITED:')

      RETURN
      END
