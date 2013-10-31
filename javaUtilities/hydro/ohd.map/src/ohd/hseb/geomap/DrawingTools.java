package ohd.hseb.geomap;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.util.List;

import ohd.hseb.geomap.model.LatLonPoint;
import ohd.hseb.geomap.model.LatLonPolyline;
import ohd.hseb.geomap.model.Polygon;
import ohd.hseb.geomap.projection.MapProjection;

public class DrawingTools
{

    public static void drawPoint(Graphics g, MapProjection projection, LatLonPoint latlonPoint)
    {
        Point screenPoint = projection.getScreenPoint(latlonPoint);
        
        drawScreenPoint(g, screenPoint);
    } 
    //-------------------------------------------------------------------------------------------------  
    
    public static void drawLineSegment(Graphics g, MapProjection projection, LatLonPoint point1, LatLonPoint point2)
    {  
        String header = "DrawingTools.drawLineSegment(): ";
          
        Point screenPoint1 = projection.getScreenPoint(point1);
        Point screenPoint2 = projection.getScreenPoint(point2);
        
         
        if (bothPointsAreInRange(screenPoint1, screenPoint2, projection))
        {
           // System.out.println(header + "screenPoint1 = " + screenPoint1 + " screenPoint2 = " + screenPoint2);
            
            drawScreenLine(g, screenPoint1, screenPoint2);
        }
        
        
    }
    
    //-------------------------------------------------------------------------------------------------
    
    private static void drawScreenPoint(Graphics g, Point screenPoint)
    {
        int width = 5;
        int height = 5;
        g.fillOval(screenPoint.x, screenPoint.y, width, height);
    }
    
    //-------------------------------------------------------------------------------------------------
      
    private static void drawScreenLine(Graphics g, Point screenPoint1, Point screenPoint2)
    {
        String header = "DrawingTools.drawScreenLine(): ";
        if ((screenPoint1 != null) && (screenPoint2 != null) ) 
        {

            g.drawLine(screenPoint1.x, screenPoint1.y, screenPoint2.x, screenPoint2.y);

        }
        else
        {
            System.out.println(header + " both screen Points were null");
        }
       
    }
    
    //-------------------------------------------------------------------------------------------------
    private static boolean bothPointsAreInRange(Point screenPoint1, Point screenPoint2, MapProjection projection )
    {
        boolean result = false;
        
        if (pointIsInRange(screenPoint1, projection) && (pointIsInRange(screenPoint2, projection)))
        {
            result = true;
        }
        
        return result;
    }
    //-------------------------------------------------------------------------------------------------
    private static boolean pointIsInRange(Point screenPoint, MapProjection projection)
    {
        boolean result = false;
        
        if ( (screenPoint.x > -1) && (screenPoint.x < projection.getMapWidth()) )
        {
            if ( (screenPoint.y > -1) && (screenPoint.y < projection.getMapHeight()) )
            {
                result = true;
            }
        }
        
        return result;
    }
    //-------------------------------------------------------------------------------------------------
  
    public static void drawPolygon(Graphics g, MapProjection projection, Polygon polygon)
    {
        
         String header = "DrawingTools.drawPolygon(): ";
        // System.out.println(header);
         
         List<LatLonPoint> pointList = polygon.getPointList();
         LatLonPoint previousPoint = null;
         
         boolean firstTime = true;
         
         for (LatLonPoint point : pointList)
         {
             if (firstTime)
             {
                 firstTime = false;   
                 //drawPoint(point);
             }

             else
             {
                 drawLineSegment(g, projection, previousPoint, point);
             }
             
             previousPoint = point;
         }
    }
    
    //-------------------------------------------------------------------------------------------------
    public static void drawPolyline(Graphics g, MapProjection projection, LatLonPolyline polyline)
    {
        
         String header = "DrawingTools.drawPolygon(): ";
        // System.out.println(header);
         
         List<LatLonPoint> pointList = polyline.getPointList();
         LatLonPoint previousPoint = null;
         
         boolean firstTime = true;
         
         for (LatLonPoint point : pointList)
         {
             if (firstTime)
             {
                 firstTime = false;   
                 //drawPoint(point);
             }

             else
             {
                 drawLineSegment(g, projection, previousPoint, point);
             }
             
             previousPoint = point;
         }
    }
    
    //-------------------------------------------------------------------------------------------------
    
    public static void setDrawingColor(Graphics g, Color color)
    {
        String header = "DrawingTools.setDrawingColor(): ";

        g.setColor(color);

        return;    
    }
    
}
