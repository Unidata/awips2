package ohd.hseb.geomap.examples;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.util.List;

import ohd.hseb.geomap.layer.BaseMapLayer;
import ohd.hseb.geomap.model.LatLonPoint;
import ohd.hseb.geomap.projection.MapProjection;

public class PrecipMonitorPointMapLayer extends BaseMapLayer
{

    private List<StationPoint> _stationPointList = null;

    // -----------------------------------------------------------------------------------------------

    public PrecipMonitorPointMapLayer(List<StationPoint> pointList )
    {
        _stationPointList = pointList;
    }

    // -----------------------------------------------------------------------------------------------

    public void draw(Graphics g, MapProjection projection)
    {   
        String header = "SshpPointMapLayer.draw(): ";
        //  System.out.println(header);


        for(StationPoint point : getStationPointList())
        {
            //  System.out.println(header + point);
            drawStationPoint(g, projection, point);
        }
    }

    // -----------------------------------------------------------------------------------------------

    private void drawStationPoint(Graphics g, MapProjection projection, StationPoint stationPoint)
    {
        String header = "PrecipMonitorPointMapLayer.drawStationPoint(): ";
        //  System.out.println(header);

        LatLonPoint latLonPoint = stationPoint.getLatLonPoint();

        int size = 2;

        Point screenPoint = projection.getScreenPoint(latLonPoint);

      //  drawTriangle(g, size, Color.GREEN, screenPoint);
        drawCircle(g, size, Color.MAGENTA, screenPoint);

       // drawLabel(g, stationPoint.getId(), Color.WHITE, screenPoint, 5, 0);
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
    
    public void drawCircle(Graphics g, 
                            int size,
                            Color color,
                            Point screenPoint)
    {

        Graphics2D g2 = (Graphics2D) g;
        
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);
  
        g2.setColor(color);
        
        int halfSize = size/2;
          
        g2.fillOval(screenPoint.x - halfSize,
                    screenPoint.y - halfSize,
                    size, size);

        return;
    }
    
    // -----------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------

    public void setStationPointList(List<StationPoint> pointList)
    {
        _stationPointList = pointList;
    }

    public List<StationPoint> getStationPointList()
    {
        return _stationPointList;
    }

    // -----------------------------------------------------------------------------------------------


}
