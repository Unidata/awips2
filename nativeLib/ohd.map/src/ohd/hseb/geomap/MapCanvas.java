package ohd.hseb.geomap;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.util.ArrayList;
import java.util.List;

import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.gui.drawing.BufferedCanvas;
import ohd.hseb.geomap.layer.MapLayer;
import ohd.hseb.geomap.model.LatLonBounds;
import ohd.hseb.geomap.model.LatLonPoint;
import ohd.hseb.geomap.projection.MapProjection;

public class MapCanvas extends BufferedCanvas
{    
    CodeTimer _timer = new CodeTimer();
    
    protected List<MapLayer> _mapLayerList = new ArrayList<MapLayer>();
    private MapProjection _projection  =  null;

    //-------------------------------------------------------------------------------------------------
   
    public MapCanvas(MapProjection projection, int width, int height)
    {
        super(0,0, width, height);
        setProjection(projection);
        
        projection.setMapHeight(height);
        projection.setMapWidth(width);
        
        setBackground(Color.green);
        
        addListeners();
    }
    
    //-------------------------------------------------------------------------------------------------
    private void addListeners()
    {
        addComponentListener(new ResizeListener());
    }
    
    //-------------------------------------------------------------------------------------------------
 
    public void setMapScaleFactor(double mapScaleFactor)
    {
        _projection.setMapScaleFactor(mapScaleFactor);
        repaint();
    }

    //-------------------------------------------------------------------------------------------------

    public double getMapScaleFactor()
    {
        return  _projection.getMapScaleFactor();
    }
    
    //-------------------------------------------------------------------------------------------------
     public void zoom(double zoomFactor)
    {
        double mapScaleFactor = _projection.getMapScaleFactor() * zoomFactor;
        setMapScaleFactor(mapScaleFactor);
    }
     //-------------------------------------------------------------------------------------------------

    public void pan(double northSouthPanProportion, double eastWestProportion)
    {
        
       // String header = "MapCanvas.pan(): ";
       // System.out.println(header + northSouthPanProportion + " " + eastWestProportion);
    
        _projection.panProportionally(northSouthPanProportion, eastWestProportion);

        repaint();
    }
//  -------------------------------------------------------------------------------------------------

    public void setLatLonBounds(LatLonBounds latLonbounds)
    {
        String header = "MapCanvas.setLatLonBounds(): ";
        System.out.println(header);
        _projection.setLatLonBounds(latLonbounds);
        repaint();
    }
    
    //-------------------------------------------------------------------------------------------------
    public void recenterMap(LatLonPoint newCenter)
    {
        String header = "MapCanvas.recenterMap(): ";
        System.out.println(header);
        _projection.setCenterLatLon(newCenter);
          
        repaint();
    }
    //-------------------------------------------------------------------------------------------------
    
    
    public void removeMapLayer(MapLayer mapLayer)
    {
        _mapLayerList.remove(mapLayer);
        setNeedsToRedraw(true);
    }
    
    //-------------------------------------------------------------------------------------------------
    
    
    public void addMapLayer(MapLayer mapLayer, int layerLevel)
    {
        _mapLayerList.add(layerLevel, mapLayer);
        setNeedsToRedraw(true);
    }
    
    //-------------------------------------------------------------------------------------------------
    
    public void addMapLayer(MapLayer mapLayer)
    {
        int index = _mapLayerList.size();
        addMapLayer(mapLayer, index);
    }

    //-------------------------------------------------------------------------------------------------
    
    public void draw(Graphics g)
    {
      String header = "MapCanvas.draw(): ";
        
        _timer.start();
        System.out.println(header);
        clearBufferedImage(g);
        drawAllMapLayers();
        
        _timer.stop(header + "drawing time took ");
    }
     
    //-------------------------------------------------------------------------------------------------
    private void clearBufferedImage(Graphics g)
    {
        g.setColor(Color.black);
        g.fillRect(0, 0, getWidth(), getHeight());
    }
    //  -------------------------------------------------------------------------------------------------
    
    
    private void drawAllMapLayers()
    {
       for (MapLayer mapLayer : _mapLayerList)
       {
           if (mapLayer.shouldDraw() )
           {
               drawMapLayer(mapLayer);
           }    
       }
    }
    
    //-------------------------------------------------------------------------------------------------
    private void drawMapLayer(MapLayer mapLayer)
    {
        String header = "MapCanvas.drawMapLayer(): ";
       // System.out.println(header);
           
        MapCanvas canvas = this;
        mapLayer.draw(getBufferedImageGraphics(), getProjection()); 
        
        return;
    }
    
    //-------------------------------------------------------------------------------------------------
    
    public void setProjection(MapProjection projection)
    {
        _projection = projection;
    }

    //-------------------------------------------------------------------------------------------------

    public MapProjection getProjection()
    {
        return _projection;
    } 
    
    //-------------------------------------------------------------------------------------------------
    public void recenterAtXY(int x, int y)
    {
        Point clickedPoint = new Point(x,y);
        LatLonPoint newCenter = _projection.getLatLonPoint(clickedPoint);
        
        System.out.println("clickedPoint = " + clickedPoint);
        System.out.println("newCenter = " + newCenter);
        
        Point screen2 = _projection.getScreenPoint(newCenter);
        System.out.println("newCenter back to screen point =  " + screen2 );
        
        
        recenterMap(newCenter);
    
        return;
    }
    
    //-------------------------------------------------------------------------------------------------    
    
    private class ResizeListener extends ComponentAdapter
    {
        public void componentResized(ComponentEvent event)
        {
            _projection.setMapWidth(getWidth());
            _projection.setMapHeight(getHeight());
            repaint();
        }
    }
    
 
    
    //-------------------------------------------------------------------------------------------------
   
}
