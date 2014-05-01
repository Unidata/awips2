package ohd.hseb.geomap.test;

import java.util.ArrayList;
import java.util.List;

import ohd.hseb.geomap.contour.RowColToLatLonTranslator;
import ohd.hseb.geomap.contour.RowToLatLonContourer;
import ohd.hseb.geomap.model.LatLonBounds;
import ohd.hseb.geomap.model.LatLonPoint;
import ohd.hseb.geomap.model.LatLonPolyline;
import junit.framework.TestCase;

public class ContouringTest extends TestCase
{

    
    protected void setUp()
    {
        
    }
    
    public void testContouring()
    {
          
         double [][] valueArray = {
                                     {15, 20, 30 },
                                     {5,  15, 25 },
                                     {5,  0, 15 }

                                  };
         
         double northLat = 2;
         double southLat = 0;
         double eastLat = 2;
         double westLat = 0;
         LatLonBounds bounds = new LatLonBounds(northLat, southLat, westLat, eastLat);
         
         RowColToLatLonTranslator translator = new MockRowColToLatLonTranslator(bounds, 1.0, 1.0);
         RowToLatLonContourer contourer = new RowToLatLonContourer(valueArray, translator );
         
         List<LatLonPolyline> polylineList = contourer.contourOneLevel(10);
         for (LatLonPolyline polyline: polylineList)
         {
             System.out.println(polyline + "\n");
         }
         
         
         List<LatLonPolyline> desiredPolylineList = new ArrayList<LatLonPolyline>();
         
         //polyLine1
         LatLonPolyline polyLine = new LatLonPolyline();
         polyLine.addPoint(new LatLonPoint(1, 0.5));
         polyLine.addPoint(new LatLonPoint(0.5, 0));
         
         desiredPolylineList.add(polyLine);
         
         //polyLine2
         polyLine = new LatLonPolyline();
         polyLine.addPoint(new LatLonPoint(1, 0.5));
         polyLine.addPoint(new LatLonPoint(1 + 1.0/3.0, 1.0));
         
         desiredPolylineList.add(polyLine);
         
         //polyLine3
         polyLine = new LatLonPolyline();
         polyLine.addPoint(new LatLonPoint(2, 1 + (2.0/3.0)));
         polyLine.addPoint(new LatLonPoint(1 + (1.0/3.0), 1.0));
         
         desiredPolylineList.add(polyLine);
     
 
         boolean equalLists = equalPolyLineLists(desiredPolylineList, polylineList);
         boolean desiredEquals = true;
         assertEquals(desiredEquals, equalLists);
             
         
    }
    
    
    public boolean equalPolyLineLists(List<LatLonPolyline> polylineList1, List<LatLonPolyline> polylineList2)
    {
        boolean result = false;
        
        if (polylineList1.size() == polylineList2.size())
        {
            result = true;
            for (int i = 0; i < polylineList1.size(); i++)
            {
                LatLonPolyline polyline1 = polylineList1.get(i);
                LatLonPolyline polyline2 = polylineList2.get(i);
                if (! polyline1.equals(polyline2))
                {
                    result = false;
                    System.out.println("polyline1 " + polyline1 + " != polyline 2 " + polyline2);
                    break;
                }
            }
            
        }
        
        return result;
    }
    
}
