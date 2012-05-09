/************************************************************************
 * SCNFLL.H                                                             *
 *                                                                      *
 * This header file declares the variables used in scan converting 	*
 * filled polygons.							*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAI          6/96                                          *
 ***********************************************************************/

#define MAX_X 1728
#define MAX_Y 2400
#define MAX_YSZ 5000
#define MaxScreenY 2500   /* reset once integrated to Ysize */
#define MAX_POINTS 100   /* number of points allowed */
#define MAX_3D_POLYVERTS 4  /* man number verts in 3d figure */
#define MAX_POLYS_3D 10    /* max number of polygons in the 3d figure */


/* two dimensional point record. */
typedef struct dcpt2
{
	 int x;
	 int y;
}dcPt2, dcPts2[MAX_POINTS];


/* edge table record */
typedef struct edgerec
{
	 int yUpper;
	 float xIntersect;
	 float dxPerScan;
	 struct edgerec * next;
} edgeRec, *edgePtr;

