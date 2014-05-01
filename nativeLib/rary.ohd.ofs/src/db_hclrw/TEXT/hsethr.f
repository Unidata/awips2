C MODULE HSETHR
C-----------------------------------------------------------------------
C    THIS ROUTINE REPLACES THE HOUR AND TIME ZONE CODE VALUES
C    WITH THE USER DEFAULTS FOR DATES WHICH WERE ENTERED WITHOUT
C    HOUR AND OR TIME ZONE CODE.  THE JULIAN HOUR IS CALCULATED
C    FOR DATES WHICH ARE NOT *.
C
C             VERSION:  1.0.0
C                DATE:  5-17-82
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C***********************************************************************
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C         IARR      I   I/O    7    ARRAY CONTAINING DATE
C                                    IARR(1)=JULIAN HOUR
C                                    IARR(2)=MONTH
C                                    IARR(3)=DAY OF MONTH
C                                    IARR(4)=YEAR (2 DIGIT)
C                                    IARR(5)=HOUR OF DAY (FOR TZC)
C                                    IARR(6)=TIME ZONE CODE
C                                    IARR(7)=HOUR*100+MINUTES
C***********************************************************************
      SUBROUTINE HSETHR (IARR)

      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hdflts'

      INTEGER IARR(7),IK
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hsethr.f,v $
     . $',                                                             '
     .$Id: hsethr.f,v 1.2 1998/07/02 20:03:45 page Exp $
     . $' /
C    ===================================================================
C

C  For an hour of -1 use the default hour (local value of 12z)

      IF (IARR(5) .EQ. -1) IARR(5) = 12 - TIME(2)

C  For a blank time zone, use the default code:
C    First set the time zone,
C    Next calculate the julian hour if the month is not *, #, or %

      IF (IARR(6) .EQ. IBLNK) THEN

        IARR(6)=TIME(3)

        IK = IARR(2)
        IF (IK.NE.IASTR .AND. IK.NE.IPOUND .AND. IK.NE.IPRCNT) THEN
          CALL UJLNTC (IARR(2),IARR(3),IARR(4),IARR(5),IARR(1),TIME(2))
        ENDIF

      ENDIF

      RETURN
      END
