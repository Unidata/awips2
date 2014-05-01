
/*
** Interp - do a linear interpolation
**
** x		.... "x" point that is known
** xmin, xmax	.... known limits of "x" data
** y		.... "y" point to be found
** ymin, ymax	.... known limits of "y" data
*/
float Interp ( float x, float xmin, float xmax, float ymin, float ymax )
{	float	y;

	if ( (xmax - xmin) == 0.0 )
		y = ymin;
	else	y = ymin + (ymax - ymin)*(x - xmin)/(xmax - xmin);
	return y;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/Interp.c,v $";
 static char rcs_id2[] = "$Id: Interp.c,v 1.1 1999/02/18 15:16:58 dws Exp $";}
/*  ===================================================  */

}

