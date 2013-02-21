C**********************************************************************
C*								      *
C*  Function statement for single dimension of diagnostic grid dgg.   *
C*  It is important to remember to include this file AFTER all        *
C*  declarations, parameter statements, and data statements.          *
C*								      *
C**********************************************************************
C
	IGDPT ( i, j ) = ( j - 1 ) * kxyd + i
