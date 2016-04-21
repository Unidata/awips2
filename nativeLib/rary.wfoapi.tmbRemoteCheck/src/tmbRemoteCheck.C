// ****************************************************************************
// +++ 	Name: tmbRemoteCheck.C
//Purpose: This program replaces the previously used findprocess.sh script 
//         which is too slow to search each process via a ssh (remote shell).
//         The objective is to search any of the AWIPS applications are
//         running or not.
//
//Inputs:  None
//
// History:
// 18-Jan-05 P. Wu	Initial
// 10-Feb-05 P. Wu	Add textWish check
// ---*************************************************************************
#include <unistd.h>
#include "testmode.H"
#include "tmbUtil.H"

// +++ 	Function: main
// 
//Purpose:  This program is called by TMCP via a remote ssh. By calling
//          the findprocess routine locally, it searches for all the AWIPS
//          applications such as fxaWish, warnGenWish, wwa and RiverPro
//          accordingly. It returns 0 if found noe of them. Otherwise, it 
//          returns a return code accordingly.
// ---*************************************************************************
int main(int argc, char *argv[])
{
    int rc;

    // Remind the user to quit D-2D first by sending to msg window
    rc = findprocess("fxaWish");
    if (rc > 0)
	   return 1;
    // Check for warnGenWish
    rc = findprocess("warnGenWish");
    // Remind the user to quit warnGenWish first by sending to msg window
    if (rc > 0)
	   return 2;
    // Check for WWA
    rc = findprocess("wwa");
    // Remind the user to quit wwa first by sending to msg window
    if (rc > 0)
	   return 3;
    // Check for RiverPro
    rc = findprocess("/awips/hydroapps/whfs/bin/rpf.LX");
    // Remind the user to quit RiverPro first by sending to msg window
    if (rc > 0)
	   return 4;
    // Check for GFE
    rc = findprocess("/awips/GFESuite/bin/run/gfe");
    // Remind the user to quit GFE first by sending to msg window
    if (rc > 0)
	   return 5;
    // Check for textWish
    rc = findprocess("textWish");
    // Remind the user to quit textWish first by sending to msg window
    if (rc > 0)
	   return 6;

    // Check for CAVE
    rc = findprocess("./cave");
    // Remind the user to quit CAVE first by sending to msg window
    if (rc > 0)
    {
    	return 7;
    }

    return 0;
}    
