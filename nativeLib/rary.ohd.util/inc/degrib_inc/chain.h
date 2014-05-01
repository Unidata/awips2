#ifndef CHAIN_H

#include "degrib_inc/type.h"
#include "degrib_inc/meta.h"
#include "degrib_inc/mymapf.h"

/* x, y chosen as float since that is accurate to +/- .000001 degrees lat
 * which is .000001 * 60 / 1.852 km = .03 m */
typedef struct chainNode {
   float x, y;
   struct chainNode *next;
} chainNode;

typedef struct {
   chainNode *head, *tail, *preTail;
} chainType;

typedef struct {
   chainType *actList, *finList;
   int numAct, numFin;
   double value;
} polyType;

void NewPolys (polyType ** poly, int *numPoly);

#ifdef CHAIN_DEBUG
void CompactPolys (polyType * poly, int numPoly);
#endif

void gribCompactPolys (polyType * poly, int *numPoly, sChar f_nMissing,
                       gridAttribType * attrib, double **polyData);

void FreePolys (polyType * poly, int numPoly);

int Grid2BigPoly (polyType ** poly, int *numPoly, int Nx, int Ny,
                  double *Data);

void ConvertChain2LtLn (polyType * poly, int numPoly, myMaparam * map,
                        sChar LatLon_Decimal);

int CreateBigPolyShp (char *filename, polyType *poly, int numPoly);


#ifdef CHAIN_DEBUG
void PrintPolys (polyType * poly, int numPoly);
#endif

#endif
