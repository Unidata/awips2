C**********************************************************************
C*                                                                    *
C*  Function statement for missing value test.  When using this       *
C*  test it is important to remember to include this file AFTER       *
C*  all declarations, parameter statements, and SAVE statements, but  *
C*  BEFORE any DATA statements (for IRIX compatibility).  It is also  *
C*  necessary to include the file (GEMPRM.PRM) that contains the      *
C*  parameter statements for the missing data values (RMISSD AND      *
C*  RDIFFD).                                                          *
C**                                                                   *
C* Log:                                                               *
C* J. Wu/GSC       08/00       documented about IRIX compatibility    * 
C**********************************************************************
C
	LOGICAL	ERMISS
	ERMISS (xxxx) = ( ABS ( xxxx - RMISSD ) .lt. RDIFFD )
