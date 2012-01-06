//------------------------------------------------------------------------------
// Component :: printComponent - prints out the Component contents.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 20 Feb 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Component.h"

void Component :: printComponent( FILE* fp )
{
	char	father_str[MAXC], son_str[MAXC];
	int	i=0;
	if( !fp ){
		return;
	}

	if( _father != NULL ){
		sprintf( father_str, "\tFather: %s\t",  _father->_id );
	}
	else {
		father_str[0] = '\0';
	}

	if( _n_son ){
		sprintf( son_str, "\tSons: " );

		for( i=0; i<_n_son; i++ ){
			strcat( son_str, _son_list[i]->_id );

			if( i != _n_son-1 ){
				strcat( son_str, ", " );
			}
		}
	}
	else {
		son_str[0] = '\0';
	}
	// Print out the id and connectivity information...
	fprintf( fp, "ID: %s%s%s\n", _id, father_str, son_str );

	// Print out the constants if necessary...

	for( i=0; i<_n_const; i++ ){
		fprintf( fp, "\tCONSTANT %s %0.3f\n", _const_id_list[i], _const_val_list[i] );
	}

	for( i=0; i<_n_meth; i++ ){
		if( _expr_list[i] != NULL ) {
			_expr_list[i]->print( fp );
			_method_list[i]->print( fp );
		}
	}

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Component_printComponent.cxx,v $";
 static char rcs_id2[] = "$Id: Component_printComponent.cxx,v 1.2 2006/10/26 15:18:40 hsu Exp $";}
/*  ===================================================  */

}
