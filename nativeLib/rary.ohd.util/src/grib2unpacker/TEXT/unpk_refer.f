      SUBROUTINE UNPK_REFER(IS5,NS5,ICLEAN,IUNPKOPT,IER,*)
C
C        APRIL   2000   LAWRENCE   GSC/TDL   ORIGINAL CODING
C        JANUARY 2001   GLAHN      COMMENTS; CHANGED IER = 1040 TO 708;
C                                  INITIALIZED IUNPKOPT TO 0
C
C        PURPOSE
C           DETERMINES WHAT PACKING SCHEME WAS USED TO PACK THE DATA.
C           DETERMINES WHETHER OR NOT THE USER WANTS THE DATA RETURNED
C           WITH OR WITHOUT MISSING VALUES IMBEDDED IN IT. IF THE USER
C           DOESN'T WANT MISSING VALUES IMBEDDED WITHIN THE DATA FIELD,
C           THEN A BIT-MAP IS RETURNED TO THE USER INDICATING THE
C           LOCATIONS OF THE MISSING VALUES. IF THE USER WANTS MISSING
C           VALUES TO BE INCLUDED IN THE DATA FIELD, THEN NO BIT-MAP
C           IS RETURNED FROM THIS ROUTINE. THE ONLY EXCEPTION TO
C           THIS IS WHEN THE USER HAS A PREDEFINED BIT-MAP. IN THIS
C           CASE, A BIT-MAP IS NOT RETURNED TO THE USER, AND THE
C           DATA FIELD IS RETURNED WITHOUT MISSING VALUES IN IT.
C
C           THE ONLY PACKING METHOD IN WHICH A BIT-MAP IS PACKED INTO
C           THE GRIB2 MESSAGE ALONG WITH THE DATA FIELD IS THE SIMPLE
C           PACKING METHOD. FOR THE COMPLEX AND SECOND ORDER
C           DIFFERENCING METHODS OF PACKING DATA, A BIT-MAP IS NOT
C           ACTUALLY PACKED INTO THE GRIB2 MESSAGE. MISSING VALUES
C           ARE INCLUDED IN THE PACKED DATA. IF THE USER DESIRES A
C           BIT-MAP WITH THESE PACKING METHODS THEN THE PROGRAM
C           WILL GENERATE ONE.
C
C           IN ADDITION TO PRIMARY MISSING VALUES, THE COMPLEX
C           PACKING METHOD ALLOWS FOR THE EXISTENCE OF SECONDARY
C           MISSING VALUES. IF THE USER WANTS A BIT-MAP RETURNED
C           FROM THE UNPACKING OF A DATA FIELD THAT HAS BOTH
C           PRIMARY AND SECONDARY MISSING VALUES IN IT, AND THIS
C           DATA WAS PACKED USING THE COMPLEX METHOD, THEN ONLY
C           THE LOCATIONS OF THE PRIMARY MISSING VALUES WILL BE
C           INDICATED BY THE BIT-MAP. IN THIS CASE, ONLY THE
C           PRIMARY MISSING VALUES WILL BE REMOVED FROM THE
C           DATA FIELD.
C
C        DATA SET USE
C           NONE
C        VARIABLES
C              IS5(J) = THE VALUES ASSOCIATED WITH SECTION 5, KEYED
C                       TO THE OCTET NUMBER.  THE ELEMENTS USED
C                       IN THIS ROUTINE ARE:
C                       IS5(10), TEMPLATE NUMBER:
C                       0 = SIMPLE
C                       1 = NOT SUPPORTED
C                       2 = COMPLEX
C                       3 = SPATIAL DIFFERENCING
C                       (J=1,NS5). (INPUT)
C                 NS5 = SIZE OF IS5( ). (INPUT)
C              ICLEAN = 1 WHEN THE USER DOESN'T WANT PRIMARY MISSING
C                         VALUES IN THE UNPACKED DATA FIELD.
C                       0 WHEN THE USER WANTS PRIMARY MISSING VALUES
C                         TO BE LEFT IN THE UNPACKED DATA FIELD. 
C                         (INPUT)
C            IUNPKOPT = 0 DON'T UNPACK THIS DATA GRID. AN ERROR
C                         WAS ENCOUNTERED. 
C                       1 UNPACK THIS GRID USING THE SIMPLE METHOD,
C                         LEAVE THE MISSING VALUES IN THE GRID,
C                         DO NOT RETURN A BIT-MAP. 
C                       2 UNPACK THIS GRID USING THE SIMPLE METHOD,
C                         REMOVE THE MISSING VALUES FROM THE GRID,
C                         RETURN A BIT-MAP INDICATING MISSING VALUE
C                         LOCATIONS. 
C                       3 UNPACK THIS GRID USING THE COMPLEX METHOD,
C                         LEAVE THE MISSING VALUES IN THE GRID,
C                         DO NOT RETURN A BIT-MAP.
C                       4 UNPACK THIS GRID USING THE COMPLEX METHOD,
C                         REMOVE THE MISSING VALUES FROM THE GRID,
C                         RETURN A BIT-MAP INDICATING MISSING VALUE
C                         LOCATIONS.
C                       5 UNPACK THIS GRID USING COMPLEX PACKING
C                         WITH SECOND ORDER SPATIAL DIFFERENCES.
C                         LEAVE THE PRIMARY MISSING VALUES IN THE GRID,
C                         DO NOT RETURN A BIT-MAP.
C                       6 UNPACK THIS GRID USING COMPLEX PACKING
C                         WITH SECOND ORDER SPATIAL DIFFERENCES.
C                         REMOVE THE PRIMARY MISSING VALUES FROM
C                         THE GRID.
C                         RETURN A BIT-MAP INDICATING THE PRIMARY
C                         MISSING VALUE LOCATIONS.
C                         (OUTPUT)
C                 IER = RETURN STATUS CODE. (OUTPUT)
C                         0 = GOOD RETURN.
C                       708 = INVALID UNPACKING OPTION INDICATED IN
C                             IS5(10).
C                   * = ALTERNATE RETURN WHEN IER NE 0 FROM
C                       SUBROUTINES.
C
C             LOCAL VARIABLES
C             IUNPACK = CONTAINS THE CODE OF THE METHOD TO USE
C                       FOR UNPACKING AS GIVEN BY IS5(10).  INITIALIZED
C                       TO ZERO.
C
C        NON SYSTEM SUBROUTINES CALLED
C           NONE 
C
      DIMENSION IS5(NS5)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2unpacker/RCS/unpk_refer.f,v $
     . $',                                                             '
     .$Id: unpk_refer.f,v 1.1 2004/09/16 16:51:50 dsa Exp $
     . $' /
C    ===================================================================
C
C
      IER=0
      IUNPKOPT=0
C
C        RETRIEVE THE UNPACKING METHOD FROM IS5(10)
      IUNPACK=IS5(10)
C
      SELECT CASE (IUNPACK)
C
         CASE (0)
C
C              SIMPLE UNPACKING
            IF(ICLEAN.EQ.0)THEN
               IUNPKOPT=1
            ELSE
               IUNPKOPT=2
            ENDIF
C
         CASE (2)
C
C              COMPLEX UNPACKING
            IF(ICLEAN.EQ.0)THEN
               IUNPKOPT=3
            ELSE
               IUNPKOPT=4
            ENDIF
C
         CASE (3)
C
C              SECOND ORDER SPATIAL DIFFERENCES
            IF(ICLEAN.EQ.0)THEN
               IUNPKOPT=5
            ELSE
               IUNPKOPT=6
            ENDIF
C
         CASE DEFAULT
C
C              INVALID UNPACKING OPTION.
            IER=708
      END SELECT
C
      IF(IER.NE.0)RETURN 1
C
      RETURN
      END
