/*
// Parameters which should be set before calling this function:
//
// int    numpts    =  how many corners the polygon has
// float*  polyx    =  horizontal coordinates of corners
// float*  polyY[]  =  vertical coordinates of corners
// float*  x, y     =  point to be tested
// int* return      =  the return value (0 for out, 1 for in)
//
// Follows basin vector by vector, determining if any vector holds 
// the point (x,y).
//  
// IMPORTANT: This assumes the first point equals the last point
// in the polygons!
*/

void sbptnedg(int* numpts, float* polyx, float* polyy, float* x, float* y, int* retval)
{

    int i;
    int j = 0;
    *retval = 0;
    
    for (i=0; i < *numpts - 1; i++) 
    {
        j++; 

    
        if ( (polyx[i]+(*y-polyy[i])/(polyy[j]-polyy[i])*(polyx[j]-polyx[i])) == *x)
        {
            *retval = 1;
            return;
        }
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbptnedg.c,v $";
 static char rcs_id2[] = "$Id: sbptnedg.c,v 1.1 2005/01/14 20:21:38 hank Exp $";}
/*  ===================================================  */

}
