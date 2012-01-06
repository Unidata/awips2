/*
	File:		setup_tree.c
	Date:		12/14/1994
	Author:		Dale Shelton
	
	Purpose:	Initializes the setup[] array with the 
			appropriate data elements, and function
			pointers.  Used to provide the appropriate
			callbacks for the "Application Setup"
			dialog.	

*/


#include "setup_funcs.h"
#include "setup_tree.h"
#include "CoopComms.h"
#include "CoopRecip.h"
#include "CoopSpons.h"
#include "DamTypes.h"
#include "DcpOwner.h"
#include "GageMaint.h"
#include "GageOwner.h"
#include "GageType.h"
#include "Hsa.h"
#include "Network.h"
#include "ResOwner.h"
#include "Rfc.h"
#include "TelmOwner.h"
#include "TelmPayor.h"
#include "TelmType.h"
#include "Wfo.h"


CoopComms	CommsPtr;
CoopRecip	RecipPtr;
CoopSpons	SponsPtr;
DamTypes	DamPtr;
DcpOwner	DcpPtr;
GageMaint	GMaintPtr;
GageOwner	GOwnerPtr;
GageType	GTypePtr;
Hsa		HsaPtr;
Network		NetPtr;
ResOwner	ROwnerPtr;
Rfc		RfcPtr;
TelmOwner	TOwnerPtr;
TelmPayor	TPayorPtr;
TelmType	TTypePtr;
Wfo		WfoPtr;


/*
	Initialize the setup structure, with the relevant
	data elements, and function pointers.
*/


struct _SetupTree setup[] =
{
 { "owner",
    &ROwnerPtr,
    ROwnerPtr.owner,
    rowner_load,
    DeleteResOwner,
    ( void * ) PutResOwner,
    ( void * ) UpdateResOwner  
 },

 { "type",
    &DamPtr,
    DamPtr.type,
    dtype_load,
    DeleteDamTypes,
    ( void * ) PutDamTypes,
    ( void * ) UpdateDamTypes
 }
};

int	entries = sizeof(setup) / sizeof(struct _SetupTree);
