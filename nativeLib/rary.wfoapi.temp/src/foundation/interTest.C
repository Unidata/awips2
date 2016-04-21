// ----------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// interTest.C
// Intersection of two lines test rogram
//-----------------------------------------------------------------------------

#ifdef IDENT_C
static const char* const interTest_C_Id =
 "$Id: .interTest.C__temp27950,v 1.2 2003/05/06 23:11:58 fluke Exp $";
#endif
//-----------------------------------------------------------------------------

// -- module ------------------------------------------------------------------
// interTest is a program that test for the intersection of two lines.  
//-----------------------------------------------------------------------------

#include <iostream>
#include <stdlib.h>
#include "commonDefs.h"
//#include <assert.h>
#include <math.h>
#include "CartDomain2D.H"
#include "CartCoord2D.H"

static CartCoord2D<float> uVector(CartCoord2D<float>& startPt, 
  CartCoord2D<float>& endPt);

static CartCoord2D<float> uVector(CartCoord2D<float>& pt);

static CartCoord2D<float> vectorDiff(CartCoord2D<float>& startPt, 
  CartCoord2D<float>& endPt);

static float dotProduct(CartCoord2D<float>& firstPt, 
  CartCoord2D<float>& secondPt);

static bool intersect(CartCoord2D<float>& sLine1, CartCoord2D<float>& eLine1,
  CartCoord2D<float>& sLine2, CartCoord2D<float>& eLine2,
  CartCoord2D<float>& iPt);

static CartCoord2D<float> scaleVector(CartCoord2D<float>& vector, float scale);

// -- fileScope --------------------------------------------------------------
// uVector()
// Routine to compute the unit vector of a line. 
// -- implementation ---------------------------------------------------------
// 
// ---------------------------------------------------------------------------
CartCoord2D<float> uVector(CartCoord2D<float>& startPt,
  CartCoord2D<float>& endPt)
    {
    float x, y;
    float ux, uy;

    x = endPt.x - startPt.x;
    y = endPt.y - startPt.y;

    float length = sqrt((x*x + y*y));

    ux = x / length;
    uy = y / length;

    CartCoord2D<float> unitVector(ux, uy);

    return unitVector;
    }

// -- fileScope --------------------------------------------------------------
// uVector()
// Routine to compute the unit vector of a vector. 
// -- implementation ---------------------------------------------------------
// 
// ---------------------------------------------------------------------------
CartCoord2D<float> uVector(CartCoord2D<float>& pt)
    {
    float ux, uy;

    float length = sqrt((pt.x*pt.x + pt.y*pt.y));

    ux = pt.x / length;
    uy = pt.y / length;

    CartCoord2D<float> unitVector(ux, uy);

    return unitVector;
    }

// -- fileScope --------------------------------------------------------------
// vectorDiff()
// Routine to compute the vector difference between two points. 
// -- implementation ---------------------------------------------------------
// 
// ---------------------------------------------------------------------------
CartCoord2D<float> vectorDiff(CartCoord2D<float>& startPt,
  CartCoord2D<float>& endPt)
    {
    float x, y;

    x = endPt.x - startPt.x;
    y = endPt.y - startPt.y;

    CartCoord2D<float> vDiff(x, y);

    return vDiff;
    }

// -- fileScope --------------------------------------------------------------
// dotProduct()
// Routine to compute the dot product of two vectors. 
// -- implementation ---------------------------------------------------------
// 
// ---------------------------------------------------------------------------
float dotProduct(CartCoord2D<float>& firstPt, CartCoord2D<float>& secondPt)
    {
    return firstPt.x*secondPt.x + firstPt.y*secondPt.y;
    }

// -- fileScope --------------------------------------------------------------
// dotProduct()
// Routine to compute the dot product of two vectors. 
// -- implementation ---------------------------------------------------------
// 
// ---------------------------------------------------------------------------
bool intersect(CartCoord2D<float>& sLine1, CartCoord2D<float>& eLine1, 
  CartCoord2D<float>& sLine2, CartCoord2D<float>& eLine2, 
  CartCoord2D<float>& iPt)
    {
    // compute unit vector of intersection line (U2)
    CartCoord2D<float> interUVector = uVector(sLine2, eLine2);

    // compute vector V (vector originating at the start point of the
    // intersecting line and ending at the start point of the domain line)
    CartCoord2D<float> vVector = vectorDiff(sLine2, sLine1);

    // compute vector W (vector originating at the start point of the 
    // intersecting line and ending at the end point of the domain line)
    CartCoord2D<float> wVector = vectorDiff(sLine2, eLine1);

    // compute the vector perpendicular to V, U3
    float dotVU2 = dotProduct(vVector, interUVector);
    CartCoord2D<float> vScalar = scaleVector(interUVector, dotVU2);

    //CartCoord2D<float> u3 = uVector(vScalar, vVector);
    CartCoord2D<float> vPerp = vectorDiff(vScalar, vVector);
    CartCoord2D<float> u3 = uVector(vPerp);

    // compute length of other perpendiculars
    float pv = dotProduct(vVector, u3);
    float pw = dotProduct(wVector, u3*-1);

    // compute fraction of distance from start of top line vector (U1) 
    // to end of top line vector (U1)
    float frac;
    if ((pv+pw) == 0)
      frac = -99999;
    else
      frac = pv / (pv+pw);

    // compute intersection point
    int sign;
    if (frac < 0) 
      sign = -1;
    else if (frac == 0)
      sign =0;
    else 
      sign = 1;

    CartCoord2D<float> alongU1 = vectorDiff(vVector, wVector);
    float alongU1Len = dotProduct(alongU1, alongU1);
    
    if (frac >=0 && frac <= alongU1Len)
        {
        float ix = sLine1.x + sign*alongU1.x*frac;
        float iy = sLine1.y + sign*alongU1.y*frac;

        iPt = CartCoord2D<float> (ix, iy);
        return 1;
        }
    else 
      return 0;
    }

// -- fileScope --------------------------------------------------------------
// scaleVector()
// Routine to scale a vector. 
// -- implementation ---------------------------------------------------------
// 
// ---------------------------------------------------------------------------
CartCoord2D<float> scaleVector(CartCoord2D<float>& vector, float scale)
    {
    CartCoord2D<float> scaledVector = vector;
    scaledVector.scale(scale, scale);
    return scaledVector;
    }

// main test file

// -- global ------------------------------------------------------------------
// main()
// Main driver program to test intersection of two lines.
//-----------------------------------------------------------------------------
int main()
    {    
    CartCoord2D<float> sLine1(12, 12);
    CartCoord2D<float> eLine1(12, 2);
    
    // intersection line (U2)
    CartCoord2D<float> sLine2(-3, 18);
    CartCoord2D<float> eLine2(-3, 4);

    CartCoord2D<float> iPt;

    bool inter = intersect(sLine1, eLine1, sLine2, eLine2, iPt);

    if (inter)
      std::cout << "Lines intersect at: " << iPt.x << " " << iPt.y << std::endl;
    else
      std::cout << "Lines do not intersect." << std::endl;

    return 0;
    }

