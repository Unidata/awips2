// ----------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// coordTest.C
// Coord and Domain Class Test Program
//-----------------------------------------------------------------------------

#ifdef IDENT_C
static const char* const coordTest_C_Id =
 "$Id: .coordTest.C__temp27950,v 1.2 2003/05/06 23:11:56 fluke Exp $";
#endif
//-----------------------------------------------------------------------------

// -- module ------------------------------------------------------------------
// The coordTest program fully exercises the floatCoord and intCoord
// classes, as well as the Domain class.
//-----------------------------------------------------------------------------

#include <iostream>
#include <stdlib.h>
#include "commonDefs.h"
#include "CartCoord2D.H"
#include "CartDomain2D.H"

// main test file

bool floatCoordTest();
bool intCoordTest();
bool domainTest();
bool test_union(float x1a, float x2a, float y1a, float y2a,
                float x1b, float x2b, float y1b, float y2b);
bool test_intersect(float x1a, float x2a, float y1a, float y2a,
                    float x1b, float x2b, float y1b, float y2b);
bool test_include(float x1a, float x2a, float y1a, float y2a,
                  float xb, float yb);
bool test_domain(float x1, float x2, float y1, float y2);

// -- global ------------------------------------------------------------------
// main()
// Main driver for testing Coord classes.  Program returns 0 on success of
// all tests.
//-----------------------------------------------------------------------------
int main()
    {
    std::cout << "Coord Test Program " << std::endl;

    std::cout << "Testing Int Coord class" << std::endl;
    if(!intCoordTest())
        {
        std::cout << "..Int Coord test failed" << std::endl;
        return 1;
        }

    std::cout << "Testing Float Coord class" << std::endl;
    if(!floatCoordTest())
        {
        std::cout << "..floatCoord test failed" << std::endl;
        return 1;
        }

    std::cout << "Testing Domain class" << std::endl;
    if(!domainTest())
        {
        std::cout << "..domain test failed" << std::endl;
        return 1;
        }

    std::cout << "Testing Completed Successfully" << std::endl;
    return 0;
    }


// -- global ------------------------------------------------------------------
// floatCoordTest()
// Tests CartCoord2D<float> class.  Returns true if all tests were successfully
// completed.
//-----------------------------------------------------------------------------
bool floatCoordTest()
    {
    CartCoord2D<float> a3(1.0,2.0);
    CartCoord2D<float> b3 = a3 * 10.0;
    if ((b3.x != 10.0) || (b3.y != 20.0))
        {
        std::cout << "Error in * scalar operation" << std::endl;
        return false;
        }

    CartCoord2D<float> a4(10.0,20.0);
    CartCoord2D<float> b4 = a4 / 10.0;
    if ((b4.x != 1.0) || (b4.y != 2.0))
        {
        std::cout << "Error in / scalar operation" << std::endl;
        return false;
        }

    CartCoord2D<float> b7(1.0, 2.0);
    b7 *= 10.0;
    if ((b7.x != 10.0) || (b7.y != 20.0))
        {
        std::cout << "Error in *= scalar operation" << std::endl;
        return false;
        }

    CartCoord2D<float> b8(10.0, 20.0);
    b8 /= 10.0;
    if ((b8.x != 1.0) || (b8.y != 2.0))
        {
        std::cout << "Error in /= scalar operation" << std::endl;
        return false;
        }

    //  Float Coord tests
    CartCoord2D<float> a10(1.0,2.0);
    CartCoord2D<float> b10(3.0,4.0);
    CartCoord2D<float> c10 = a10 + b10;
    if ((c10.x != 4.0) || (c10.y != 6.0))
        {
        std::cout << "Error in + Coord operation" << std::endl;
        return false;
        }

    CartCoord2D<float> a11(1.0,2.0);
    CartCoord2D<float> b11(3.0,7.0);
    CartCoord2D<float> c11 = b11 - a11;
    if ((c11.x != 2.0) || (c11.y != 5.0))
        {
        std::cout << "Error in - Coord operation" << std::endl;
        return false;
        }

    CartCoord2D<float> a12(1.0,2.0);
    CartCoord2D<float> b12(3.0,7.0);
    b12 += a12;
    if ((b12.x != 4.0) || (b12.y != 9.0))
        {
        std::cout << "Error in += Coord operation" << std::endl;
        return false;
        }

    CartCoord2D<float> a13(1.0,2.0);
    CartCoord2D<float> b13(3.0,7.0);
    b13 -= a13;
    if ((b13.x != 2.0) || (b13.y != 5.0))
        {
        std::cout << "Error in -= Coord operation" << std::endl;
        return false;
        }

    CartCoord2D<float> a14(1.0,2.0);
    a14.scale(10.0, 20.0);
    if ((a14.x != 10.0) || (a14.y != 40.0))
        {
        std::cout << "Error in scale function" << std::endl;
        return false;
        }

    CartCoord2D<float> a = CartCoord2D<float>(3.0, 4.0);
    CartCoord2D<float> b = CartCoord2D<float>(11.0, 13.0);
    if (crossProduct(a, b) != -crossProduct(b, a))
        {
        std::cout << "Error in crossProduct calculations" << std::endl;
        return false;
        }

    if (dotProduct(a, b) != dotProduct(b, a))
        {
        std::cout << "Error in dot product calculations" << std::endl;
        return false;
        }

    return true;
    }

// -- global ------------------------------------------------------------------
// intCoordTest()
// Tests CartCoord2D<int> class.  Returns true if all tests were successfully
// completed.
//-----------------------------------------------------------------------------
bool intCoordTest()
    {
    // Scalar tests
    CartCoord2D<int> a3(1,2);
    CartCoord2D<int> b3 = a3 * 10;
    if ((b3.x != 10) || (b3.y != 20))
        {
        std::cout << "Error in * scalar operation" << std::endl;
        return false;
        }

    // output operator test
    std::cout << "CartCoord output test: " << b3 << std::endl;

    CartCoord2D<int> a4(10,20);
    CartCoord2D<int> b4 = a4 / 10;
    if ((b4.x != 1) || (b4.y != 2))
        {
        std::cout << "Error in / scalar operation" << std::endl;
        return false;
        }

    CartCoord2D<int> b7(1, 2);
    b7 *= 10;
    if ((b7.x != 10) || (b7.y != 20))
        {
        std::cout << "Error in *= scalar operation" << std::endl;
        return false;
        }

    CartCoord2D<int> b8(10, 20);
    b8 /= 10;
    if ((b8.x != 1) || (b8.y != 2))
        {
        std::cout << "Error in /= scalar operation" << std::endl;
        return false;
        }

    //  Float Coord tests
    CartCoord2D<int> a10(1,2);
    CartCoord2D<int> b10(3,4);
    CartCoord2D<int> c10 = a10 + b10;
    if ((c10.x != 4) || (c10.y != 6))
        {
        std::cout << "Error in + Coord operation" << std::endl;
        return false;
        }

    CartCoord2D<int> a11(1,2);
    CartCoord2D<int> b11(3,7);
    CartCoord2D<int> c11 = b11 - a11;
    if ((c11.x != 2) || (c11.y != 5))
        {
        std::cout << "Error in - Coord operation" << std::endl;
        return false;
        }

    CartCoord2D<int> a12(1,2);
    CartCoord2D<int> b12(3,7);
    b12 += a12;
    if ((b12.x != 4) || (b12.y != 9))
        {
        std::cout << "Error in += Coord operation" << std::endl;
        return false;
        }

    CartCoord2D<int> a13(1,2);
    CartCoord2D<int> b13(3,7);
    b13 -= a13;
    if ((b13.x != 2) || (b13.y != 5))
        {
        std::cout << "Error in -= Coord operation" << std::endl;
        return false;
        }

    CartCoord2D<int> a14(1,2);
    a14.scale(10.0, 20.0);
    if ((a14.x != 10) || (a14.y != 40))
        {
        std::cout << "Error in scale function" << std::endl;
        return false;
        }

    return true;
    }

// -- global ------------------------------------------------------------------
// domainTest()
// Tests CartDomain2D<float> class.  Returns true if all tests were 
// successfully completed.
//-----------------------------------------------------------------------------
bool domainTest()
    {
    CartCoord2D<float> o(3.0, 4.0);
    CartCoord2D<float> e(5.0, 6.0);
    CartDomain2D<float> d(o, e);
    std::cout << "CartDomain output test: " << d << std::endl;
    
    if (!test_domain(1,2,3,4) || !test_domain(2,1,3,4)
      || !test_domain(1,2,4,3) || !test_domain(2,1,4,3)
      || !test_union(1,2,3,4, 0,0,0,0) || !test_union(0,0,0,0, 3,4,1,2)
      || !test_union(1,2,3,4, 6,7,9,8) || !test_union(1,2,3,4, 6,7,8,9)
      || !test_union(1,7,3,9, 2,6,4,8) || !test_intersect(1,2,3,4, 0,0,0,0)
      || !test_intersect(0,0,0,0, 3,4,1,2) || !test_intersect(1,2,3,4, 6,7,9,8)
      || !test_intersect(1,2,3,4, 6,7,8,9) || !test_intersect(1,3,6,8, 2,4,7,9)
      || !test_intersect(1,7,3,9, 2,6,4,8) || !test_include(0,0,0,0, 3,4)
      || !test_include(3,4,1,2, 2,6))
        return false;

    return true;
    }


// -- global ------------------------------------------------------------------
// test_union()
// Tests the union capability of the domain class. Returns true if successful.
//-----------------------------------------------------------------------------
bool test_union(float x1a, float x2a, float y1a, float y2a,
                float x1b, float x2b, float y1b, float y2b)
    {
    CartDomain2D<float> da;
    if (x1a != x2a || y1a != y2a)
        da = CartDomain2D<float>::createFromCorners(
             CartCoord2D<float>(x1a,y1a), CartCoord2D<float>(x2a,y2a) );

    CartDomain2D<float> db;
    if (x1b != x2b || y1b != y2b)
        db = CartDomain2D<float>::createFromCorners(
             CartCoord2D<float>(x1b,y1b), CartCoord2D<float>(x2b,y2b) );

    CartDomain2D<float> dc = da;
    dc.combineWith(db); 
    float allXMin, allYMin, allXMaxExt, allYMaxExt;

    if (!da.isNull() && !db.isNull() && da.yUp() == db.yUp()
      && da.xRight() == db.xRight())
        {
        allXMin = min(min(x1a,x2a),min(x1b,x2b));
        allYMin = min(min(y1a,y2a),min(y1b,y2b));
        allXMaxExt = max(max(x1a,x2a),max(x1b,x2b)) - allXMin;
        allYMaxExt = max(max(y1a,y2a),max(y1b,y2b)) - allYMin;
        if (dc.origin().x != allXMin || dc.origin().y != allYMin
          || dc.extent().x != allXMaxExt || dc.extent().y != allYMaxExt)
            {
            std::cout << "combineWith() failed" << std::endl;
            std::cout << da << " + " << db << " = " << dc << std::endl;
            return false;
            }
        }
    else if (da.isNull() && !db.isNull() && dc != db)
       {
       std::cout << "combineWith() failed for one NULL domain" << std::endl;
       return false;
       }
    else if (!da.isNull() && db.isNull() && dc != da)
       {
       std::cout << "combineWith() failed for one NULL domain" << std::endl;
       return false;
       }

    da |= db;
    if (da != dc)
        {
        std::cout << "Error with domain |= operator" << std::endl;
        return false;
        }

    return true;
    }

// -- global ------------------------------------------------------------------
// test_intersect()
// Tests the intersect capability of the domain class. Returns true if 
// successful.
//-----------------------------------------------------------------------------
bool test_intersect(float x1a, float x2a, float y1a, float y2a,
                    float x1b, float x2b, float y1b, float y2b)
    {
    CartDomain2D<float> da;
    if (x1a != x2a || y1a != y2a)
        da = CartDomain2D<float>::createFromCorners(
             CartCoord2D<float>(x1a,y1a), CartCoord2D<float>(x2a,y2a) );

    CartDomain2D<float> db;
    if (x1b != x2b || y1b != y2b)
        db = CartDomain2D<float>::createFromCorners(
             CartCoord2D<float>(x1b,y1b), CartCoord2D<float>(x2b,y2b) );

    CartDomain2D<float> dc = da & db;

    if (da.isNull() || db.isNull())
        {
        if (dc.isNull())
            ;
        else
            {
            std::cout << "Intersect (operator &= failed) with NULL domain" << std::endl;
            std::cout << da << " & " << db << " = " << dc << std::endl;
            return false;
            }
        }
    else if (da.yUp() != db.yUp() || da.xRight() != db.xRight())
        {
        if (dc.isNull())
            ;
        else
            {
            std::cout << "Intersect (operator & failed) on different orientation"
              << std::endl;
            return false;
            }
        }
    else
        {
        float oppX, orgX, oppY, orgY;
        oppX = min(max(x1a,x2a), max(x1b,x2b));
        orgX = max(min(x1a,x2a), min(x1b,x2b));
        oppY = min(max(y1a,y2a), max(y1b,y2b));
        orgY = max(min(y1a,y2a), min(y1b,y2b));
        if (oppX < orgX)
            {
            if (!dc.isNull())
                {
                std::cout << "Intersect (operator & failed) for non-intersect1" 
                  << std::endl;
                std::cout << da << " & " << db << " = " << dc << std::endl;
                return false;
                }
            }
        else if (oppY < orgY)
            {
            if (!dc.isNull())
                {
                std::cout << "Intersect (operator & failed) for non-intersect2" 
                  << std::endl;
                std::cout << da << " & " << db << " = " << dc << std::endl;
                return false;
                }
            }
        else if (dc.origin().x != orgX || dc.origin().y != orgY
          || dc.extent().x + dc.origin().x != oppX 
          || dc.extent().y + dc.origin().y != oppY)
            {
            std::cout << "operator& failed" << std::endl;
            std::cout << da << " & " << db << " = " << dc << std::endl;
            return false;
            }
        }

    // now try the &= operator
    da &= db;
    if (da != dc)
        {
        std::cout << "Domain &= operator failed" << std::endl;
        return false;
        }

    return true;
    }

// -- global ------------------------------------------------------------------
// test_include()
// Tests the include capability of the domain class. Returns true if 
// successful.
//-----------------------------------------------------------------------------
bool test_include(float x1a, float x2a, float y1a, float y2a,
                  float xb, float yb)
    {
    CartDomain2D<float> da;
    if (x1a != x2a || y1a != y2a)
        da = CartDomain2D<float>::createFromCorners(
             CartCoord2D<float>(x1a,y1a), CartCoord2D<float>(x2a,y2a) );

    CartCoord2D<float> cb(xb,yb);

    CartDomain2D<float> dc = da;
    dc.expandToInclude(cb); 

    // verify
    if (da.isNull())
        {
        if (dc.origin() != cb)
            {
            std::cout << "expandToInclude() fail on NULL domain" << std::endl;
            std::cout << da << " + " << cb << " = " << dc << std::endl;
            return false;
            }
        }
    else
        {
        CartCoord2D<float> org = da.origin(), ext = da.extent();
        if (xb < min(x1a,x2a))
            {
            org.x = xb;
            ext.x += (min(x1a,x2a) - xb);
            }
        else if (xb > max(x1a,x2a))
            ext.x = (xb - org.x);
        if (yb < min(y1a,y2a))
            {
            org.y = yb;
            ext.y += (min(y1a,y2a) - yb);
            }
        else if (yb > max(y1a,y2a))
            ext.y = (yb - org.y);

        if (org != dc.origin() || ext != dc.extent())
            {
            std::cout << "expandToInclude() fail on good domain" << std::endl;
            std::cout << da << " + " << cb << " = " << dc << std::endl;
            return false;
            }
        }

    CartDomain2D<float> dd = da|cb;
    CartDomain2D<float> de = cb|da;
    da |= cb;
    if (da != dc || da != dd || de != dd)
        {
        std::cout << "Error on expandToInclude or operator|" << std::endl;
        return false;
        }

    
    return true;
    }

// -- global ------------------------------------------------------------------
// test_domain()
// Tests the domain capability of the domain class. Returns true if successful.
//-----------------------------------------------------------------------------
bool test_domain(float x1, float x2, float y1, float y2)
    {
    CartCoord2D<float> p1(x1,y1), p2(x2,y2);
    CartDomain2D<float> d = CartDomain2D<float>::createFromCorners(p1, p2);

    if (x1 != d.left() || x2 != d.right() || y1 != d.bottom()
      || y2 != d.top())
        {
        std::cout << "Problem with left(), right(), bottom(), or top()" << std::endl;
        std::cout << d << " from corners: " << p1 << ' ' << p2 << std::endl;
        return false;
        }

    if ((d.yUp() && y2 < y1) || (!d.yUp() && y2 > y1)
      || (d.xRight() && x2 < x1) || (!d.xRight() && x2 > x1))
        {
        std::cout << "Problem with yUp() or xRight()" << std::endl;
        std::cout << "original points: " << p1 << ' ' << p2 << std::endl;
        std::cout << "xRight()=" << d.xRight() << "  yUp()=" << d.yUp() << std::endl;
        return false;
        }

    if (d.xMin() != min(x1,x2) || d.xMax() != max(x1,x2)
      || d.yMin() != min(y1,y2) || d.yMax() != max(y1,y2))
        {
        std::cout << "Problem with xMin or yMin" << std::endl;
        std::cout << "original points: " << p1 << ' ' << p2 << std::endl;
        std::cout << "Xmin,Xmax,Ymin,Ymax: " << d.xMin() << ' '
          << d.xMax() << ' ' << d.yMin() << ' ' << d.yMax() << std::endl;
        return false;
        }

    return true;
    }

