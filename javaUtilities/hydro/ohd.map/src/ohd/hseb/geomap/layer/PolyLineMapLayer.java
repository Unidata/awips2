package ohd.hseb.geomap.layer;

import java.awt.Color;
import java.awt.Graphics;


import java.util.List;

import ohd.hseb.geomap.DrawingTools;
import ohd.hseb.geomap.model.LatLonPoint;
import ohd.hseb.geomap.model.LatLonPolyline;
import ohd.hseb.geomap.projection.MapProjection;



public class PolyLineMapLayer extends BaseMapLayer
{
    List<LatLonPolyline> _latLongLineList = null;
    
    // -----------------------------------------------------------------------------------------------
    
    public PolyLineMapLayer(List<LatLonPolyline> latLongLineList )
    {
        _latLongLineList = latLongLineList;
    }
   
    // -----------------------------------------------------------------------------------------------
    
    public void draw(Graphics g, MapProjection projection)
    {   
        String header = "LineMapLayer.draw(): ";
      //  System.out.println(header);
    
        
        for(LatLonPolyline latLonPolyline : _latLongLineList)
        {
            drawLine(g, projection, latLonPolyline);
        }
    }
    
    // -----------------------------------------------------------------------------------------------
    
    private void drawLine(Graphics g, MapProjection projection, LatLonPolyline latLonPolyline)
    {
        String header = "LineMapLayer.drawLine(): ";
        //System.out.println(header);
        
        Color color = getColor();
        
        g.setColor(color);
       
        List<LatLonPoint> pointList = latLonPolyline.getPointList();
        
        for (int i= 0; i < pointList.size() - 1; i++)
        {
            LatLonPoint point1 = pointList.get(i);
            LatLonPoint point2 = pointList.get(i+1);
            
             
            DrawingTools.drawLineSegment(g, projection, point1, point2);  
        }
        
        
    }

    // -----------------------------------------------------------------------------------------------
    
}
