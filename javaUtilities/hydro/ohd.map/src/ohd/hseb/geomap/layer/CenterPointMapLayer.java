package ohd.hseb.geomap.layer;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;

import ohd.hseb.geomap.model.LatLonPoint;
import ohd.hseb.geomap.projection.MapProjection;

public class CenterPointMapLayer extends BaseMapLayer
{
    
    private int _size = 6;
    private int _halfSize = 3;
    private Color _color = null;
    
    public CenterPointMapLayer(int size, Color color)
    {
        _size = size;
        _halfSize = _size/2;
        _color = color;
    }

    public void draw(Graphics graphics, MapProjection projection)
    {
         Graphics2D g2 = (Graphics2D) graphics;
         LatLonPoint centerLatLonPoint = projection.getCenterLatLon();
         
         Point centerXYPoint = projection.getScreenPoint(centerLatLonPoint);
         g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                 RenderingHints.VALUE_ANTIALIAS_ON);
   
         g2.setColor(_color);
           
         g2.fillOval(centerXYPoint.x - _halfSize,
                     centerXYPoint.y - _halfSize,
                     _size, _size);
 
    }

   
}
