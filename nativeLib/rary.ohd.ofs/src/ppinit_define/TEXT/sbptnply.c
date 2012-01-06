/*
// Parameters which should be set before calling this function:
//
// int    numpts    =  how many corners the polygon has
// float*  polyx    =  horizontal coordinates of corners
// float*  polyY[]  =  vertical coordinates of corners
// float*  x, y     =  point to be tested
// int* return      =  the return value (0 for out, 1 for in)
//
// (Globals are used in this example for purposes of speed.
// Change as desired.)
//
// The function will return 1 if the point x,y is inside the
// polygon, or 0 if it is not. If the point x,y is exactly on
// the edge of the polygon, then the function may return TRUE or
// FALSE.
//
// Note that division by zero is avoided because the division is
// protected by the "if" clause which surrounds it.
//
// IMPORTANT: This assumes the first point equals the last point
// in the polygons!
*/

void sbptnply(int* numpts, float* polyx, float* polyy, float* x, float* y, int* retval) 
{

    int i;
    int j = 0;
    *retval = 0;
    float slopeinv;
    float checkval;

    for (i=0; i < *numpts - 1; i++) 
    {
        j++; 
    
        if ( ((polyy[i] < *y) && (polyy[j] >= *y)) ||
             ((polyy[j] < *y) && (polyy[i] >= *y)) ) 
        {
            slopeinv=(polyx[j]-polyx[i])/(polyy[j]-polyy[i]);
            checkval=(polyx[i]+(*y-polyy[i])*slopeinv);
            if ( checkval < *x)
            {
                *retval = ! *retval;
            }
        }

    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbptnply.c,v $";
 static char rcs_id2[] = "$Id: sbptnply.c,v 1.1 2005/01/14 20:22:05 hank Exp $";}
/*  ===================================================  */

}
