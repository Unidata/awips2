package ohd.hseb.geomap.layer;

import java.awt.Color;
import java.awt.Graphics;

import java.util.List;

import ohd.hseb.geomap.DrawingTools;
import ohd.hseb.geomap.model.Polygon;
import ohd.hseb.geomap.projection.MapProjection;

public class PolygonMapLayer extends BaseMapLayer
{
    List<Polygon> _latLongPolygonList = null;
    
    // -----------------------------------------------------------------------------------------------
    
    public PolygonMapLayer(List<Polygon> latLongPolygonList )
    {
        _latLongPolygonList = latLongPolygonList;
    }
   
    // -----------------------------------------------------------------------------------------------
    
    public void draw(Graphics g, MapProjection projection)
    {   
        String header = "PolygonMapLayer.draw(): ";
      //  System.out.println(header);
    
        
        for(Polygon polygon : _latLongPolygonList)
        {
            drawPolygon(g, projection, polygon);
        }
    }
    
    // -----------------------------------------------------------------------------------------------
    
    private void drawPolygon(Graphics g, MapProjection projection, Polygon polygon)
    {
        String header = "PolygonMapLayer.drawPolygon(): ";
        //System.out.println(header);
        
        Color color = getColor();
        
        g.setColor(color);
       
        DrawingTools.drawPolygon(g, projection, polygon);
    }

    // -----------------------------------------------------------------------------------------------
    
}
