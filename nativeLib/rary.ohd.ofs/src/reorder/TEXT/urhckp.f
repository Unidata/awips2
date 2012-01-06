C MEMBER URHCKP
C  (from old member URHCKP)
C***********************************************************************
C                                                                      *
C         MEMBER URHCKP
C                                                                      *
C***********************************************************************
C
       SUBROUTINE URHCKP( IPASS , ISTCOD )
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  URHCKP
C                                                                      *
C    7/18/86   NEW MEMBER FROM TASK 282
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  06/20/82                                       *
C                                                                      *
C              AUTHOR: JONATHAN D. GERSHAN
C                      DATA SCIENCES INC                              *
C                      8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C      THIS ROUTINE CHECKS THE VALIDITY OF A 4 LETTER PASSWORD AGAINST
C       THE DEFINITION FILES.
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C                                                                      *
C                                                                      *
C                                                                      *
C                                                                      *
C                                                                      *
C                                                                      *
C                                                                      *
C                                                                      *
C                                                                      *
C                                                                      *
C                                                                      *
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'uio'
      INCLUDE 'hclcommon/hcuuno'
      INCLUDE 'udsi'
      INCLUDE 'ufreei'
C                                                                      *
C***********************************************************************
C                                                                      *
C          DIMENSION AND TYPE DECLARATIONS:                            *
C                                                                      *
C                                                                      *
C                                                                      *
C                                                                      *
C***********************************************************************
C                                                                      *
C          DATA:                                                       *
C                                                                      *
C                                                                      *
C                                                                      *
C***********************************************************************
C
C
C ********** THIS ROUTINE CHECKS A PASSWORD FOR THE COMPRESS ROUTINES
C ********** A STATUS CODE IS RETURNED  AS FOLLOWS:
C ********** 0 = OK , 1 = NO PASSWORD , 2 = BAD PASSWORD
C
C
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hcntrl'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urhckp.f,v $
     . $',                                                             '
     .$Id: urhckp.f,v 1.1 1995/09/17 19:17:40 dws Exp $
     . $' /
C    ===================================================================
C
C
C
       ISTCOD = 0
       IF ( IPASS .NE. IBLNK ) GO TO 200
C
            WRITE(LPD,1010)
1010   FORMAT(' **ERROR** MISSING PASSWORD!!')
             ISTCOD = 1
             GO TO 999
C
C
200    IF ( IPASS .EQ. HCNTL(5,2) ) GO TO 999
C
         WRITE(LPD,1011)
1011     FORMAT(' **ERROR** INVALID PASSWORD-EXECUTION HALTED')
C
210     ISTCOD = 2
C
999     RETURN
        END
