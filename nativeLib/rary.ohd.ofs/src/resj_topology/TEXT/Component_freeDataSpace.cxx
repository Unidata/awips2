//------------------------------------------------------------------------------
// Component :: freeDataSpace - deletes the whole tree.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 14 Jan 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
// 18 Feb 1998	Matthew J. Rutherford, RTi Changed the function a bit so that
//				it only deletes it sons and itself.
// 19 Feb 1998	DKW		_expr_list and _method_list stuff.
// 19 Feb 2004  James R. VanShar, RTi	Added _totalInflow deletion.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Component.h"

int Component :: freeDataSpace()
{
	char	routine[]="Component :: freeDataSpace";
	int	i;

	if( _inflow_ts != NULL ) {
		free( _inflow_ts );
	}

	_totalInflow.freeDataSpace();

	// The Components themselves are always allocated with new so we
	// have to use delete to free them...
	for( i=0; i<_n_son; i++ ){
		if( _son_list[i] != NULL ){
			delete _son_list[i];
		}
		_son_list[i] = NULL;
	}

	// We have to use free since we allocated this with realloc.
	if( _son_list != NULL ){
		free( _son_list );
	}

	for( i = 0; i < _n_meth; i++ ){
		if( _expr_list[i] != NULL ){
			delete _expr_list[i];
		}
		_expr_list[i] = NULL;
		if( _method_list[i] != NULL ){
			delete _method_list[i];
		}
		_method_list[i] = NULL;
	}

	if( _expr_list != NULL ){
		delete [] _expr_list;
	}
	if( _method_list != NULL ){
		delete [] _method_list;
	}

	_n_son = 0;

	if( _const_id_list ){
		_const_id_list = FreeStringList( _const_id_list );
	}

	if( _const_val_list ){
		free( _const_val_list );
		_const_val_list = NULL;
	}

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Component_freeDataSpace.cxx,v $";
 static char rcs_id2[] = "$Id: Component_freeDataSpace.cxx,v 1.4 2006/10/26 15:18:32 hsu Exp $";}
/*  ===================================================  */

}
