package ohd.hseb.geomap.layer;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import ohd.hseb.geomap.DrawingTools;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.io.RGBTextFileReader;
import ohd.hseb.geomap.model.ElevationMap;
import ohd.hseb.geomap.model.LatLonBounds;
import ohd.hseb.geomap.model.LatLonPoint;
import ohd.hseb.geomap.model.LatLonPolyline;
import ohd.hseb.geomap.projection.MapProjection;
import ohd.hseb.geomap.contour.RowToLatLonContourer;


public class ElevationMapLayer extends BaseMapLayer
{
    private ElevationMap _elevationMap = null;
    
    private List < List<LatLonPolyline> > _listOfLatLonPolylineLists = null; //the outer list is a list of  a list of polylines for each contour level
    
    private boolean _useContouring = false;
    private boolean _haveComputedContour = false;
    
    private short[][] _colorIndexArray = null; //correlates to the map coordinates
    private int _rowCount = 0;
    private int _colCount = 0;
    
    private static Color[] _colorArray = null;
    private static String[] _colorNameArray = { "grey28",
                                                "grey36",
                                                "grey43",
                                                "grey51",
                                                "grey59",
                                                "DarkGrey",
                                                "grey74",
                                                "grey25",
                                                "grey31",
                                                "grey38",
                                                "burlywood4",
                                                "wheat4",
                                                "LightYellow4",
                                                "burlywood3",
                                                "grey19",
                                                "grey25",
                                                "salmon4",
                                                "LightSalmon4",
                                                "burlywood4",
                                                "burlywood4",
                                                "LightSalmon3",
                                                "grey28",
                                                "DarkOliveGreen",
                                                "DarkOliveGreen4",
                                                "LightGoldenrod4",
                                                "khaki4",
                                                "DarkOliveGreen3",
                                                "DarkKhaki",
                                                "DarkGreen",
                                                "DarkGreen",
                                                "DarkGreen",
                                                "green4",
                                                "green4",
                                                "green4",
                                                "green3" };
    
    
  
    static
    {
        
        initColorArray();
      
    }
    
    // --------------------------------------------------------------------------------------------------
    
    public ElevationMapLayer(ElevationMap elevationMap, boolean useContouring)
    {
        setElevationMap(elevationMap);
        
        determineColorsFromMap(elevationMap);
        
        setUseContouring(useContouring);
   
    }
    // --------------------------------------------------------------------------------------------------
    private void determineColorsFromMap(ElevationMap elevationMap)
    {
        int rowCount = elevationMap.getRowCount();
        int colCount = elevationMap.getColCount();
        
        _rowCount = rowCount;
        _colCount = colCount;
        
        _colorIndexArray = new short[rowCount][colCount];
        
        for (int row = 0; row < rowCount; row++ )
        {
            for (int col = 0; col < colCount; col++)
            {
              
                int value = _elevationMap.getValue(row, col);
             
                _colorIndexArray[row][col] = getColorIndexFromValue(value, row, col);
                    
            }
        }
    }
    // --------------------------------------------------------------------------------------------------

    private static void initColorArray()
    {
        
        RGBTextFileReader reader = new RGBTextFileReader();
        String filePath =  "/usr/lib/X11/rgb.txt";
        
        Map<String, Color> map = reader.read(filePath);
       
        int size = _colorNameArray.length;
        
        _colorArray = new Color[size];
        
        for (int i = 0; i < _colorNameArray.length; i++)
        {
            String colorName = _colorNameArray[i].toUpperCase();
            
            Color color = map.get(colorName);
            _colorArray[i] = color;
         //   System.out.println("color = " +  colorName + " " + color);
        }
        
    }
    //  --------------------------------------------------------------------------------------------------
    private static void initColorArrayOld()
    {
        final int colorCount = 12;
        final int intensityIncrement = 20;
        _colorArray = new Color[colorCount];
        int intensity = 20;
        
        
        for (int i = 0; i < colorCount; i++)
        {
          
            Color color = new Color (intensity, intensity, intensity);
            
            _colorArray[i] = color;
            
            intensity += intensityIncrement;
            
        }
        
    }
    
    //  --------------------------------------------------------------------------------------------------
    public void draw(Graphics g, MapProjection projection)
    {
        //drawImage(g, projection);
        
        if (useContouring())
        {
            drawContour(g, projection);
        }
        else
        {
            drawImageByLatLon(g, projection);
        }
    }
  
    //  --------------------------------------------------------------------------------------------------
    private void drawContour(Graphics g, MapProjection projection)
    {
        String header = "ElevationMapLayer.drawContour() ";
        
        System.out.println(header);
        
        if (! haveComputedContour())
        {
            computeContour();
            setHaveComputedContour(true);
        }
        
       
        Color colorArray[] = {Color.red, Color.yellow, Color.blue, Color.green, Color.white };
        drawContourLines(g, projection, colorArray, _listOfLatLonPolylineLists);
        
        return;
    }
    
    //  --------------------------------------------------------------------------------------------------
    private void computeContour()
    {
        RowToLatLonContourer contourer = new RowToLatLonContourer(_elevationMap.getValueArray(), _elevationMap);
        double[] levelArray = {80.0, 160.0, 240.0, 320.0 };
        _listOfLatLonPolylineLists = contourer.contour(levelArray);
    }
    //  --------------------------------------------------------------------------------------------------
    private void drawContourLines(Graphics g, MapProjection projection, Color[] colorArray,  List < List <LatLonPolyline>> listOfPolylineLists)
    {
        int colorIndex = 0;
        
        for (List <LatLonPolyline>  polyLineList: listOfPolylineLists ) //for each contour level
        {
            g.setColor(colorArray[colorIndex]);
            for (LatLonPolyline polyline: polyLineList) //for each polyline
            {   
                DrawingTools.drawPolyline(g, projection, polyline);
            }
            colorIndex++;
        }
    }
    //  --------------------------------------------------------------------------------------------------
    
    
    private void drawImageByLatLon(Graphics g, MapProjection projection)
    {
        
        String header = "ElevationMapLayer.draw(): ";
        System.out.println(header);
        
        LatLonBounds latLonBounds = _elevationMap.getLatLonBounds();
        double origLatRes = _elevationMap.getLatResInDegrees();
        double origLonRes = _elevationMap.getLonResInDegrees();
        
        
        Point point1 = null;
        Point point2 = null;
         
        int width = 0;
        int height = 0;
        int halfWidth = 0;
        int halfHeight = 0; 
        
        int rectangleCount = 0;
        int virtualRectangleCount = 0;
        
        CodeTimer drawLoopTimer = new CodeTimer();
        CodeTimer drawOnlyTimer = new CodeTimer();
        CodeTimer coordinateTimer = new CodeTimer();
        CodeTimer colorSettingTimer = new CodeTimer();
        CodeTimer adjacentTimer = new CodeTimer();
        CodeTimer timerTimer = new CodeTimer();
        
        
        drawLoopTimer.start();
        
        double northLat = projection.getLatLonBounds().getNorthLat();
        double southLat = projection.getLatLonBounds().getSouthLat();
        double eastLon = projection.getLatLonBounds().getEastLon();
        double westLon = projection.getLatLonBounds().getWestLon();
        
        List listResolutionFactors = determineResolutionFactor(northLat,westLon, origLatRes, origLonRes, projection);
        
        double latResFactor = (Integer) listResolutionFactors.get(0);        
        double lonResFactor = (Integer) listResolutionFactors.get(1);
        
        double latRes = origLatRes * latResFactor;
        
        double lonRes = origLonRes * lonResFactor;
        
        
        for (double lat = northLat; lat >= southLat; lat -= latRes)
        {
            for (double lon = westLon; lon <= eastLon; lon += lonRes)
            {
            //    timerTimer.restart();
                coordinateTimer.restart();
            //    timerTimer.stop();
                
                point1  = projection.getScreenPoint(lat, lon);
                point2  = projection.getScreenPoint(lat - latRes, lon + lonRes);
                
                height = (point2.y - point1.y) + 1;            
                halfHeight = height/2;
                
                width = (point2.x - point1.x) + 1;
                halfWidth = width/2;
                 
                
                int x = point1.x - halfWidth;
                int y = point1.y - halfHeight;
                              
                int row = _elevationMap.getRow(lat);
                int col = _elevationMap.getCol(lon);
                
                
             //   timerTimer.restart();
                coordinateTimer.stop();
             //   timerTimer.stop();
                
             //   System.out.println(header + "lat = " + lat + " lon = " + lon + " row = " + row + " col = " + col);

                colorSettingTimer.restart();
                
               // int value = _elevationMap.getValue(row, col);

               // int colorIndex = getColorIndexFromValue(value, row, col);

                int colorIndex = getColorIndexFromStoredValues(row, col);
                Color color = getColorFromColorIndex(colorIndex);
                g.setColor(color);
                
                colorSettingTimer.stop();

                
        
              //  adjacentTimer.restart();
             //   
              //  int adjacentRectangleCount = getSameColorAdjacentRectanglesInSameRow(point1, width,  row, colorIndex,  projection);
            //    adjacentTimer.stop();
              int adjacentRectangleCount = 0;

                drawOnlyTimer.restart();

                g.fillRect(x, y,  (adjacentRectangleCount + 1) * width, height);

                //g.fillRect(x, y, width, height);
                drawOnlyTimer.stop();

                col += adjacentRectangleCount; //need to increment the column

                rectangleCount++;
                virtualRectangleCount += 1 + adjacentRectangleCount;

            
            } //end for col
         } //end for row
        
        
         drawLoopTimer.stop(header + " nested for loop took ");
         System.out.println(header + " the elapsed time of the drawing itself was " + drawOnlyTimer.getElapsedTime() + " in millis. ");
         System.out.println(header + " The elapsed time for screen coordinate determination was " + coordinateTimer.getElapsedTime() + " in millis. ");
       //  System.out.println(header + " The elapsed time for the adjacent pixels determination was " + adjacentTimer.getElapsedTime() + " in millis. ");
         System.out.println(header + " The elapsed time for color determination and setting was " + colorSettingTimer.getElapsedTime() + " in millis. ");
     //    System.out.println(header + " The elapsed time for a single timer was " + timerTimer.getElapsedTime() + " in millis. ");
         
          
         
         System.out.println(header + "drew " + rectangleCount + " actual rectangles.");
         System.out.println(header + "drew " + virtualRectangleCount + " virtual rectangles.");
        
        
    }
    
    //  --------------------------------------------------------------------------------------------------
    private List determineResolutionFactor(double lat, double lon, double latRes, double lonRes, MapProjection projection)
    {
        String header = "ElevationMapLayer.determineLatResolutionFactor()";
        
        
        double newLatRes = latRes;
        double newLonRes = lonRes;
        
        int latFactor = 1;
        int lonFactor = 1;
        
       // double latRes = _elevationMap.getLatResInDegrees();
       // double lonRes = _elevationMap.getLonResInDegrees();
       
        
        Point point1 = null;
        Point point2 = null;
         
        int width = 0;
        int height = 0;
        int halfWidth = 0;
        int halfHeight = 0; 
        
        boolean done = false;
        
        while (! done)
        {
            point1  = projection.getScreenPoint(lat, lon);
            point2  = projection.getScreenPoint(lat - newLatRes, lon + newLonRes);
            
            height = (point2.y - point1.y) + 1;            
            
            width = (point2.x - point1.x) + 1;
            
            System.out.println(header + " height =  " + height + " width = " + width);
            
            if (height <= 1)
            {
                newLatRes *= 2;
                latFactor *= 2;
                
            }
            
            if (width == 1)
            {
                newLonRes *= 2;
                lonFactor *= 2;
            }
            
            if ((height > 1) && (width > 1 ))
            {
                done = true;
            }
        }
        
        System.out.println(header + " latFactor = " + latFactor + " lonFactor =   " + lonFactor);
        
        
        List resultList = new ArrayList();
        resultList.add(latFactor);
        resultList.add(lonFactor);
        
        return resultList;       
    }
    //  --------------------------------------------------------------------------------------------------

    public void drawImage(Graphics g, MapProjection projection)
    {
        
        String header = "ElevationMapLayer.drawImage2(): ";
     //   System.out.println(header);
        
        LatLonBounds latLonBounds = _elevationMap.getLatLonBounds();
        double latRes = _elevationMap.getLatResInDegrees();
        double lonRes = _elevationMap.getLonResInDegrees();
   
        Point point1 = null;
        Point point2 = null;
         
        int width = 0;
        int height = 0;
        int halfWidth = 0;
        int halfHeight = 0; 
        
        int rectangleCount = 0;
        int virtualRectangleCount = 0;
        
        CodeTimer drawLoopTimer = new CodeTimer();
        CodeTimer drawOnlyTimer = new CodeTimer();
        
        CodeTimer coordinateTimer = new CodeTimer();
        
        drawLoopTimer.start();
        
        
        for (int row = 0; row < _elevationMap.getRowCount(); row++)
        {
            double lat = _elevationMap.getLat(row);
                       
            for (int col = 0; col < _elevationMap.getColCount(); col++)
            {
                
                coordinateTimer.restart();
                double lon = _elevationMap.getLon(col);

              //  System.out.println(header + "lat = " + lat + " lon = " + lon);
                
                //    System.out.println(header + "In range ! lat = " + lat + " lon = " + lon);
                    point1  = projection.getScreenPoint(lat, lon);
                    point2  = projection.getScreenPoint(lat - latRes, lon + lonRes);

                    height = (point2.y - point1.y) + 1;            
                    halfHeight = height/2;

                    width = (point2.x - point1.x) + 1;
                    halfWidth = width/2;

                    int x = point1.x - halfWidth;
                    int y = point1.y - halfHeight;

                    coordinateTimer.stop();
                    
                    boolean isInRange = isInRange(x,y, projection);
            

                    if (isInRange)
                    {

                        int value = _elevationMap.getValue(row, col);     
                        int colorIndex = getColorIndexFromValue(value, row, col);


                        Color color = getColorFromColorIndex(colorIndex);

                        g.setColor(color);

                        int adjacentRectangleCount = getSameColorAdjacentRectanglesInSameRow(point1, width,  row, colorIndex,  projection);
                        //int adjacentRectangleCount = 0;

                        drawOnlyTimer.restart();

                        // (colorIndex < 3)
                        {
                            g.fillRect(x, y,  (adjacentRectangleCount + 1) * width, height);
                            rectangleCount++;
                        }
                    
                    //g.fillRect(x, y, width, height);
                    drawOnlyTimer.stop();
                  
                    col += adjacentRectangleCount; //need to increment the column
                    
                   // rectangleCount++;
                    virtualRectangleCount += 1 + adjacentRectangleCount;
                }
            
            } //end for col
         } //end for row
        
        
        
         drawLoopTimer.stop(header + " nested for loop took ");
         System.out.println(header + " The elapsed time of the drawing itself was " + drawOnlyTimer.getElapsedTime() + " in millis. ");
         System.out.println(header + " The elapsed time for screen coordinate determination was " + coordinateTimer.getElapsedTime() + " in millis. ");
         
         
         
         System.out.println(header + "drew " + rectangleCount + " actual rectangles.");
         System.out.println(header + "drew " + virtualRectangleCount + " virtual rectangles.");
        
        
    }
    // --------------------------------------------------------------------------------------------------
    private int  getSameColorAdjacentRectanglesInSameRow(Point point, int width, int origRow, int origColorIndex, MapProjection projection)
    {
        String header = "ElevationMapLayer.getSameColorAdjacentRectanglesInSameColumn() ";
        int count = 0;
        int y = point.y;
        int colorIndex = 0;
        
        //given an x-y point, column, and a projection, determine if there are adjacent points of the same color  that can now be drawn
        for (int x = point.x + width ; x < projection.getMapWidth(); x += width)
        {
            LatLonPoint latLonPoint = projection.getLatLonPoint(x, y);
            
            int row = _elevationMap.getRow(latLonPoint.getLat());
            int col = _elevationMap.getCol(latLonPoint.getLon());
            
            double lat2 = _elevationMap.getLat(row);
            int row2 = _elevationMap.getRow(lat2);
   
            if (row == origRow)
            {

                int value = _elevationMap.getValue(row, col);

                colorIndex = getColorIndexFromValue(value, row, col);
                
                if (colorIndex == origColorIndex)
                {
                    count++;
                }
                else
                {
                    break;
                }
            }
            else
            {
                break;
            }
    
        }
        /*
        if (count > 0)
        {
           System.out.println(header + "count = " + count);
        }
        */
        
        return count;
    }
    // --------------------------------------------------------------------------------------------------
    
    private boolean sameColor(Color color1, Color color2)
    {
        boolean result = false;
        
        if ( (color1.getRed() == color2.getRed()) &&
           (color1.getBlue() == color2.getBlue()) &&
           (color1.getGreen() == color2.getGreen())
           )
        {
            result = true;
        }
        
        return result;
    }
    // --------------------------------------------------------------------------------------------------
    
    private boolean isInRange(double lat, double lon, MapProjection projection)
    {
        boolean result = false;
        
        if (projection.isInRange(lat, lon))
        {
            result = true;
        }
        
        /*
        if ((x > 0) && (x < projection.getMapWidth()) &&
           (y > 0) && (y < projection.getMapHeight()) )
        {
            result = true;
        }
        */
        
        return result;
    }
    private boolean isInRange(int x, int y, MapProjection projection)
    {
        boolean result = false;
     
        if ((x > 0) && (x < projection.getMapWidth()) &&
           (y > 0) && (y < projection.getMapHeight()) )
        {
            result = true;
        }
        
        return result;
    }
    
    // --------------------------------------------------------------------------------------------------
    
    private short getColorIndexFromStoredValues(int row, int col)
    {
        short result = 0;
        
        if ((row > -1 ) && (row < _rowCount) &&
             (col > -1) && (col < _colCount))
        {
           result = _colorIndexArray[row][col]; 
        }
        
        return result;
    }
    
    private short getColorIndexFromValue(int value, int row, int col)
    {
        short colorIndex = 0;

        if (value < 80 )
            colorIndex = 3;

        else if (value < 160 )
            colorIndex = 10;

        else if (value < 240 )
            colorIndex = 17;

        else if (value < 320 )
            colorIndex = 24;

        else
            colorIndex = 31;


        //shading

        int colorIndexDelta = _elevationMap.getValue(row + 1, col) / 3 -     //value to south
        ( _elevationMap.getValue(row, col + 1) / 3);  //value to east


        if (colorIndexDelta > 3)
            colorIndexDelta = 3;

        if (colorIndexDelta < -3)
            colorIndexDelta = -3;

        // Adjust the color to give the illusion of illumination from
        //   the southwest.
        colorIndex += colorIndexDelta;

        if (colorIndex > (short) _colorArray.length -1)
        {
            colorIndex = (short) (_colorArray.length -1);
        }
        else if (colorIndex < 0)
        {
            colorIndex = 0;
        }
        
        return colorIndex;
    }
    
    private Color getColorFromColorIndex(int colorIndex)
    {
        return _colorArray[colorIndex];          
    }
    
    private Color getColorFromValue(int value, int row, int col)
    {
        int colorIndex = getColorIndexFromValue(value, row, col);
        return getColorFromColorIndex(colorIndex);
            
    }
    // --------------------------------------------------------------------------------------------------
    public void setElevationMap(ElevationMap elevationMap)
    {
        _elevationMap = elevationMap;
    }
    // --------------------------------------------------------------------------------------------------
    
    public ElevationMap getElevationMap()
    {
        return _elevationMap;
    }
    // --------------------------------------------------------------------------------------------------
    public void setUseContouring(boolean useContouring)
    {
        _useContouring = useContouring;
    }
    // --------------------------------------------------------------------------------------------------
   
    public boolean useContouring()
    {
        return  _useContouring;
    }
    // --------------------------------------------------------------------------------------------------
    public void setHaveComputedContour(boolean haveComputedContour)
    {
        _haveComputedContour = haveComputedContour;
    }
    public boolean haveComputedContour()
    {
        return _haveComputedContour;
    }
    private void setListOfLatLonPolylineLists(List < List<LatLonPolyline> > listOfLatLonPolylineLists)
    {
        _listOfLatLonPolylineLists = listOfLatLonPolylineLists;
    }
    private List < List<LatLonPolyline> > getListOfLatLonPolylineLists()
    {
        return _listOfLatLonPolylineLists;
    }
 
}
