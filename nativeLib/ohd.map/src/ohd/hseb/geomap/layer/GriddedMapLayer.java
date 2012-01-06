package ohd.hseb.geomap.layer;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.util.List;

import ohd.hseb.geomap.DrawingTools;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.geomap.contour.ContourGrid;
import ohd.hseb.geomap.contour.RowColToLatLonTranslator;
import ohd.hseb.geomap.contour.RowToLatLonContourer;
import ohd.hseb.geomap.model.LatLonPoint;
import ohd.hseb.geomap.model.LatLonPolyline;
import ohd.hseb.geomap.projection.MapProjection;



public class GriddedMapLayer extends BaseMapLayer
{
    private ContourGrid _grid = null;
    private RowColToLatLonTranslator _rowColToLatLonTranslator = null;
    private ColorDeterminer _colorDeterminer = null;
    
    
    private List < List<LatLonPolyline> > _listOfLatLonPolylineLists = null; //the outer list is a list of  a list of polylines for each contour level


    private boolean _drawImage = true;
    
 
    
    private boolean _haveComputedContour = false;
    
    
    //  --------------------------------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------------------------------------  
    public GriddedMapLayer(ContourGrid grid,
                           RowColToLatLonTranslator rowColToLatLonTranslator,
                           ColorDeterminer colorDeterminer,
                           boolean drawImage)
    {
       setGrid(grid);   
       setRowColToLatLonTranslator(rowColToLatLonTranslator);
       setColorDeterminer(colorDeterminer);
       setShouldDrawImage(drawImage);
    }
    
    // -------------------------------------------------------------------------------------------------------

    public void draw(Graphics g, MapProjection projection)
    {
        if (shouldDrawImage())
        {
            drawImage(g, projection);
        }
        else
        {
            drawContour(g, projection);
        }
            
    }
    // -------------------------------------------------------------------------------------------------------
    public void drawImage(Graphics g, MapProjection projection)
    {
        String header = "GriddedMapLayer.drawImage(): ";
        
        int rowCount = _grid.getRowCount();
        int colCount = _grid.getColCount();
         
        LatLonPoint latLonPoint1 = null; 
        LatLonPoint latLonPoint2 = null;
        LatLonPoint latLonPoint3 = null; 
        LatLonPoint latLonPoint4 = null;
        
        CodeTimer latlonTimer = new CodeTimer();
        
        for (int row = 0; row < rowCount; row++)
        {
            for (int col = 0; col < colCount; col++)
            {
                double value =  _grid.getValue(row, col);
                  
                if (value > 0)
                {
                    latlonTimer.restart();
                    latLonPoint1 = _rowColToLatLonTranslator.getLatLonPoint(row, col);
                    latLonPoint2 = _rowColToLatLonTranslator.getLatLonPoint(row, col + 1 );  
                    latLonPoint3 = _rowColToLatLonTranslator.getLatLonPoint(row + 1, col + 1 );
                    latLonPoint4 = _rowColToLatLonTranslator.getLatLonPoint(row + 1, col);               
                    latlonTimer.stop();

                    // System.out.println(header + "latLonPoint1 = " + latLonPoint1);


                    Point screenPoint1 = projection.getScreenPoint(latLonPoint1);
                    Point screenPoint2 = projection.getScreenPoint(latLonPoint2);
                    Point screenPoint3 = projection.getScreenPoint(latLonPoint3);
                    Point screenPoint4 = projection.getScreenPoint(latLonPoint4);

                    int width = (screenPoint2.x - screenPoint1.x) + 1;
                    int height = (screenPoint1.y - screenPoint4.y) + 1;


                    int halfWidth = width/2;
                    int halfHeight = height/2;




                    //  System.out.println(header + "row = " + row+ " col = " + col + "value = " + value);




                    //System.out.println(header + "value = " + value);
                    Color color = getColorFromValue(value);

                    g.setColor(color);

                    // draw the square so that its center is at (screenPoint1.x, screenPoint1.y)
                    int x = screenPoint1.x - halfWidth;
                    int y = screenPoint1.y - halfHeight;

                    //       System.out.println(header + "x = " + x + " y = " + y);

                    //       System.out.println(header + "width = " + width + " height = " + height);

                    // if the x and y values are make it a rectangle, then draw one instead of a polygon
                    if ((screenPoint2.x == screenPoint3.x) && (screenPoint1.y == screenPoint2.y))
                    {
                        g.fillRect(x, y, width, height);
                    }
                    else //it is not a rectangle, so use a polygon
                    {
                        int xPoints[] = {screenPoint1.x,  screenPoint2.x, screenPoint3.x, screenPoint4.x};

                        int yPoints[] = {screenPoint1.y,  screenPoint2.y, screenPoint3.y, screenPoint4.y};

                        g.fillPolygon(xPoints, yPoints, 4);
                    }


                }
            }
        }

        System.out.println(header + "latLon calculations took " + latlonTimer.getElapsedTime() + " millis.");
        
        
    } //end drawImage
    
    
    //  --------------------------------------------------------------------------------------------------
    public void drawImageOld(Graphics g, MapProjection projection)
    {
        String header = "GriddedMapLayer.drawImageOld(): ";
        
        int rowCount = _grid.getRowCount();
        int colCount = _grid.getColCount();
         
        LatLonPoint latLonPoint1 = null; 
        LatLonPoint latLonPoint2 = null;
         
        for (int row = 0; row < rowCount; row++)
        {
            for (int col = 0; col < colCount; col++)
            {
                  
               latLonPoint1 = _rowColToLatLonTranslator.getLatLonPoint(row, col);
               latLonPoint2 = _rowColToLatLonTranslator.getLatLonPoint(row + 1, col+1);  
                
              // System.out.println(header + "latLonPoint1 = " + latLonPoint1);
               
               
               Point screenPoint1 = projection.getScreenPoint(latLonPoint1);
               Point screenPoint2 = projection.getScreenPoint(latLonPoint2);
               
               int width = (screenPoint2.x - screenPoint1.x) + 1;
               int height = (screenPoint1.y - screenPoint2.y) + 1;
                 
               
               int halfWidth = width/2;
               int halfHeight = height/2;
       

                
               double value =  _grid.getValue(row, col);
               
             //  System.out.println(header + "row = " + row+ " col = " + col + "value = " + value);
               
               
               
               if (value > 0)
               {
                   //System.out.println(header + "value = " + value);
                   Color color = getColorFromValue(value);

                   g.setColor(color);

                   // draw the square so that its center is at (screenPoint1.x, screenPoint1.y)
                   int x = screenPoint1.x - halfWidth;
                   int y = screenPoint1.y - halfHeight;

            //       System.out.println(header + "x = " + x + " y = " + y);
                   
            //       System.out.println(header + "width = " + width + " height = " + height);
             
                   
                   g.fillRect(x, y, width, height);
               }
            }
        }

    } //end drawImageOld
    
    
    //  --------------------------------------------------------------------------------------------------
    private void drawContour(Graphics g, MapProjection projection)
    {
        String header = "ElevationMapLayer.drawContour() ";
        
        System.out.println(header);
        
        double[] levelArray = _colorDeterminer.getLevelArray();
        
        if (! haveComputedContour())
        {
            computeContour(levelArray);
            setHaveComputedContour(true);
        }
        //  --------------------------------------------------------------------------------------------------
        
       
        drawContourLines(g, projection, getColorDeterminer(), levelArray, _listOfLatLonPolylineLists);
        
        return;
    }
    
    //  --------------------------------------------------------------------------------------------------
    private void computeContour(double[] levelArray)
    {
        
        RowToLatLonContourer contourer = new RowToLatLonContourer(_grid,
                                                                 getRowColToLatLonTranslator() );
      
        _listOfLatLonPolylineLists = contourer.contour(levelArray);
    }
    //  --------------------------------------------------------------------------------------------------
    private void drawContourLines(Graphics g, MapProjection projection,
                                  ColorDeterminer colorDeterminer,
                                  double[] levelArray,
                                  List < List <LatLonPolyline>> listOfPolylineLists)
    {
        int levelIndex = 0;
        
        for (List <LatLonPolyline>  polyLineList: listOfPolylineLists ) //for each contour level
        {
            Color color = colorDeterminer.getColorByValue(levelArray[levelIndex]);
            
            g.setColor(color);
            for (LatLonPolyline polyline: polyLineList) //for each polyline
            {   
                DrawingTools.drawPolyline(g, projection, polyline);
            }
            levelIndex++;
        }
    }
     
    //  --------------------------------------------------------------------------------------------------
    public void setHaveComputedContour(boolean haveComputedContour)
    {
        _haveComputedContour = haveComputedContour;
    }
    //  --------------------------------------------------------------------------------------------------
    
    
    public boolean haveComputedContour()
    {
        return _haveComputedContour;
    }
    //  --------------------------------------------------------------------------------------------------
    
   
    //  --------------------------------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------------------------------------
    private Color getColorFromValue(double value)
    {
         return  getColorDeterminer().getColorByValue(value);
    }
    
    //  -------------------------------------------------------------------------------------------------------
      
    public void setGrid(ContourGrid grid)
    {
        _grid = grid;
    }
    // -------------------------------------------------------------------------------------------------------
    
    public ContourGrid getGrid()
    {
        return _grid;
    }
    // -------------------------------------------------------------------------------------------------------

    public void setShouldDrawImage(boolean drawImage)
    {
        _drawImage = drawImage;
    }

    public boolean shouldDrawImage()
    {
        return _drawImage;
    }

    private void setRowColToLatLonTranslator(RowColToLatLonTranslator rowColToLatLonTranslator)
    {
        _rowColToLatLonTranslator = rowColToLatLonTranslator;
    }

    private RowColToLatLonTranslator getRowColToLatLonTranslator()
    {
        return _rowColToLatLonTranslator;
    }

    public void setColorDeterminer(ColorDeterminer colorDeterminer)
    {
        _colorDeterminer = colorDeterminer;
    }

    public ColorDeterminer getColorDeterminer()
    {
        return _colorDeterminer;
    }
    
}
