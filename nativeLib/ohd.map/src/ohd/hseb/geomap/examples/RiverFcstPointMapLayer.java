package ohd.hseb.geomap.examples;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;


import java.util.List;

import ohd.hseb.geomap.layer.BaseMapLayer;
import ohd.hseb.geomap.model.LatLonPoint;
import ohd.hseb.geomap.projection.MapProjection;


public class RiverFcstPointMapLayer extends BaseMapLayer
{
    private List<StationPoint> _fcstPointList = null;
    private Color _color = null;
    
    // -----------------------------------------------------------------------------------------------
    
    public RiverFcstPointMapLayer(List<StationPoint> pointList, Color color)
    {
        _fcstPointList = pointList;
        _color = color;
    }
   
    // -----------------------------------------------------------------------------------------------
    
    public void draw(Graphics g, MapProjection projection)
    {   
        String header = "RiverFcstPointMapLayer.draw(): ";
      //  System.out.println(header);
    
        
        for(StationPoint point : getFcstPointList())
        {
          //  System.out.println(header + point);
            drawFcstPoint(g, projection, point);
        }
    }
    
    // -----------------------------------------------------------------------------------------------
    
    private void drawFcstPoint(Graphics g, MapProjection projection, StationPoint fcstPoint)
    {
        String header = "RiverFcstPointMapLayer.drawFcstPoint(): ";
      //  System.out.println(header);

        LatLonPoint latLonPoint = fcstPoint.getLatLonPoint();
    
        int size = 8;
        
        Point screenPoint = projection.getScreenPoint(latLonPoint);
                
        drawTriangle(g, size, _color, screenPoint);
        
        drawLabel(g, fcstPoint.getId(), Color.WHITE, screenPoint, 5, 0);
    }
    
    // -----------------------------------------------------------------------------------------------
  
    public void drawLabel(Graphics g,  String text,
                          Color color, Point screenPoint,
                          int xOffset, int yOffset)
    {
        
        g.setColor(color);
        g.drawString(text, screenPoint.x + xOffset, screenPoint.y + yOffset);
    }

    // -----------------------------------------------------------------------------------------------
    public void drawTriangle(Graphics g, 
                             int size,
                             Color color,
                             Point screenPoint)
    {
        
        java.awt.Polygon p = new java.awt.Polygon();
        
        int x1 = screenPoint.x;
        int y1 = screenPoint.y - size/2;
        
        int x2 = screenPoint.x - size/2;
        int y2 = screenPoint.y + size/2;
        
        int x3 = screenPoint.x + size/2;
        int y3 = screenPoint.y + size/2;
        
        p.addPoint(x1, y1);
        p.addPoint(x2, y2);
        p.addPoint(x3, y3);
        
        g.setColor(color);
        
        g.fillPolygon(p);
        
    }
    // -----------------------------------------------------------------------------------------------
    
    public void setFcstPointList(List<StationPoint> pointList)
    {
        _fcstPointList = pointList;
    }

    public List<StationPoint> getFcstPointList()
    {
        return _fcstPointList;
    }

    // -----------------------------------------------------------------------------------------------
    
}
