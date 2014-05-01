package ohd.hseb.geomap.projection;

import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

import ohd.hseb.geomap.model.LatLonBounds;
import ohd.hseb.geomap.model.LatLonPoint;


public abstract class BaseMapProjection implements MapProjection
{   
    protected static double RADIANS_PER_DEGREE = 0.017453292519943295 ;
    protected static double DEGREES_PER_RADIAN = 1.0/RADIANS_PER_DEGREE ;
    protected static double NAUTICAL_MILES_PER_DEGREE = 60;
 
     
    private int _mapWidth = 0;
    private int _mapHeight = 0;
     
    
    private LatLonBounds _latLonBounds = null;
    private double _mapScaleFactor = 90; //degrees per/ pixel
    private LatLonPoint _centerLatLon = null;
    
    // -----------------------------------------------------------------------------------
    
    protected int getOffsetX()
    {
        return getMapWidth() / 2;
    }
    
    protected int getOffsetY()
    {
        return getMapHeight() / 2;
    }
    
    /*
   
   
float zoom_multiple = 1.0 ;
float original_zoom_multiple = 1.0 ;
long orig_scale_factor;
long init_scale_factor;
long map_scale_factor;
static int MAXX = 0 ;
static int MAXY = 0 ;
int offsetx,offsety;
int polar_offsety = 0 ;   // Used for keeping track of the y-offset of the
                          //   HRAP and Polar Stereographic map projection. 
int polar_offsetx = 0 ;   // Used for keeping track of the x-offset of the
                          //   HRAP projection. 
int projection=M_FLAT;
int prev_projection=M_FLAT;
static int XOR = 0 ;
static int YOR = 0 ;
int zoom_scale = 1;

float map_center_lat=33.0;
float map_center_lon=-114.0;
float original_center_lat ;
float original_center_lon ;
float top_lat,left_lon;
float width_in_nmi = 0.0 ;
float dtr = 0.017453292519943295 ;
float init_top_lat , init_width ;
int init_offsetx , init_zoom ;
float region_llat = -999.0 ,
      region_llon = -999.0 ,
      region_rlat = -999.0 ,
      region_rlon = -999.0 ;

static MotifCallback pan_routine = NULL ;
static MotifCallback zoom_routine = NULL ;

   
     */
    
    abstract public LatLonPoint getLatLonPoint(Point screenPoint);
    abstract public LatLonPoint getLatLonPoint(int x, int y);
    
    
    abstract public Point getScreenPoint(LatLonPoint latlonPoint);
    abstract public Point getScreenPoint(double lat, double lon);
    
    public boolean isInRange(double lat, double lon)
    {
        String header = "BaseMapProjection.isInRange()";
        boolean result = false;
        LatLonBounds bounds = getLatLonBounds();
        
        if (bounds.getNorthLat() == bounds.getSouthLat())
        {
            System.out.println(header + " north and sound bounds are the same.");
        }
        
        if  (
                
                (lat <= bounds.getNorthLat())  &&
                (lat >= bounds.getSouthLat())  &&
                (lon >= bounds.getEastLon())   &&
                (lon <= bounds.getWestLon())
            )
        {
            result = true;
            System.out.println(header + "finally, something is in range to be drawn");
        }
        
        return result;
    }
    
    
     // ----------------------------------------------------------------
     
    public List<LatLonPoint> getLatLonPointList(List<Point> screenCoordList)
    {
        List<LatLonPoint> latLonPointList = new ArrayList<LatLonPoint>();
        
        for (Point screenPoint : screenCoordList)
        {
            LatLonPoint latLonPoint = getLatLonPoint(screenPoint);
            latLonPointList.add(latLonPoint);
        }
     
        return null;
    }
   
    // ----------------------------------------------------------------
     
    public List<Point> getScreenPointList(List<LatLonPoint> latLongCoordinateList)
    {
        List<Point> screenPointList = new ArrayList<Point>();
        
        for (LatLonPoint latLonPoint : latLongCoordinateList)
        {
            Point screenPoint = getScreenPoint(latLonPoint);
            screenPointList.add(screenPoint);
        }
        
        return screenPointList;
    }

    //-------------------------------------------------------------------------------------------------

    
    
    protected void init(float displayWidthInNauticalMiles)
    {
        double degreesLongitude ;
  
        int offsetx = getOffsetX();
        int offsety = getOffsetY();
        
        double mapCenterLat = getCenterLatLon().getLat();

        degreesLongitude = displayWidthInNauticalMiles / 
                           ( NAUTICAL_MILES_PER_DEGREE * Math.cos ( mapCenterLat * RADIANS_PER_DEGREE ) ) ;
        
        double map_scale_factor = ( long ) ( 2.0 * ( float ) getOffsetX() / degreesLongitude ) ;

        //Compute the top latitude and the left longitude. 
        LatLonPoint northWestLatLong = getLatLonPoint(new Point(0,0));
        _latLonBounds.setNorthLat(northWestLatLong.getLat());
        _latLonBounds.setWestLon(northWestLatLong.getLon());
          
    /*    
        init_scale_factor = map_scale_factor ;
        orig_scale_factor = init_scale_factor ;
        init_top_lat = top_lat ;
        init_width = width_in_nmi ;
        init_offsetx = offsetx ;
        */
    }
    
    // ----------------------------------------------------------------
    /*
    
    void _init_map_variables ( float nmi )
  {
    float degrees_lon ;
    projection = M_FLAT ;
    width_in_nmi = nmi ;

    offsetx = _get_map_width ( 0 ) / 2 ;
    offsety = _get_map_height ( 0 ) / 2 ;

    // New logic added by Bryon Lawrence on January 15, 2002.  This is an
    //   attempt to incorporate an initial measurement in nautical miles into
    //   the initial computation of the scale factor.
    degrees_lon = width_in_nmi / 
                ( ( float ) NUM_NMI_PER_DEGREE * cos ( map_center_lat * dtr ) ) ;
    map_scale_factor = ( long ) ( 2.0 * ( float ) offsetx / degrees_lon ) ;

    //Compute the top latitude and the left longitude. 
     
    mConvertXY2LatLon(0,0,&top_lat,&left_lon) ;
    init_scale_factor = map_scale_factor ;
    orig_scale_factor = init_scale_factor ;
    init_top_lat = top_lat ;
    init_width = width_in_nmi ;
    init_offsetx = offsetx ;
  }

       */
    
    // ----------------------------------------------------------------
    
    public void setLatLonBounds(LatLonBounds latLonBounds)
    {
        String header = "BaseMapProjection.setLatLonBounds(): ";
        System.out.println(header);
       
        _latLonBounds = new LatLonBounds(latLonBounds);
         
        setCenterInternally();
    }
    
    private void setCenterInternally()
    {
        String header = "BaseMapProjection.setCenterInternally(): ";
        System.out.println(header);
        LatLonBounds bounds = getLatLonBounds();
        
        double lat = (bounds.getNorthLat() + bounds.getSouthLat())/2.0 ;
        double lon =  (bounds.getEastLon() + bounds.getWestLon())/2.0 ;
        
        _centerLatLon = new LatLonPoint(lat, lon);
     }
    
    private void setLatLonBoundsInternally()
    {
        String header = "BaseMapProjection.setLatLonBoundsInternally(): ";
        
        LatLonPoint centerPoint = getCenterLatLon();
        double pixelsPerDegree = getMapScaleFactor();
        
        double latOffset =  getOffsetY() / pixelsPerDegree;
        double lonOffset =  getOffsetX() / pixelsPerDegree;
        
        System.out.println(header + " latOffset = " + latOffset);
        System.out.println(header + " lonOffset = " + lonOffset);
        
        double northLat = centerPoint.getLat() + latOffset;
        double southLat = centerPoint.getLat() - latOffset;
        
        double westLat = centerPoint.getLon() - lonOffset;
        double eastLat = centerPoint.getLon() + lonOffset;
     
        _latLonBounds = new LatLonBounds(northLat,southLat, westLat, eastLat);
        
        System.out.println(header + " latLonBounds = " + _latLonBounds);
        
    }
    
    // ----------------------------------------------------------------
    public LatLonBounds getLatLonBounds()
    {
        return _latLonBounds;
    }
    // ----------------------------------------------------------------
    
    public void setMapHeight(int mapHeight)
    {
        _mapHeight = mapHeight;
        setLatLonBoundsInternally();
    }
    // ----------------------------------------------------------------
    
    public int getMapHeight()
    {
        return _mapHeight;
       
    }
  
    //-------------------------------------------------------------------------------------------------
     
    public void panProportionally(double northSouthPanProportion, double eastWestPanProportion)
    {
        String header = "BaseMapProjection.panProportionally(): ";
      //  System.out.println(header + "northSouthPanProportion = " + northSouthPanProportion +
     //                      " eastWestPanProportion = " + eastWestPanProportion);
        
        LatLonBounds latLonBounds = getLatLonBounds();
        double panLatAmount = northSouthPanProportion * latLonBounds.getLatDifference();
        double panLonAmount = eastWestPanProportion * latLonBounds.getLonDifference();
        
          System.out.println(header + "latLonBounds.getLatDifference() = " + latLonBounds.getLatDifference() +
                              " latLonBounds.getLonDifference() = " + latLonBounds.getLonDifference());
       
        
        changeCenter(panLatAmount, panLonAmount);
    
        return;
    }
    
    //-------------------------------------------------------------------------------------------------
    
    public void changeCenter(double changeLatAmount, double changeLonAmount)
    {
        
        String header = "BaseMapProjection.changeCenter(): ";
        System.out.println(header + "changeLatAmount " + changeLatAmount);
        System.out.println(header + "changeLonAmount " + changeLonAmount);
        
        _centerLatLon.changeLat(changeLatAmount);
        _centerLatLon.changeLon(changeLonAmount);
        
        System.out.println(header + "new center = " + _centerLatLon);
        System.out.println(header + "mapScaleFactor = " + getMapScaleFactor());
        
        
        setLatLonBoundsInternally();
    }
    //-------------------------------------------------------------------------------------------------
        
    public LatLonPoint getCenterLatLon()
    {
            return _centerLatLon;
    }
    
    //-------------------------------------------------------------------------------------------------
        
    public void setCenterLatLon(LatLonPoint newCenter)
    {
        String header = "BaseMapProjection.setCenterLatLon(): ";
        
        LatLonPoint oldCenter = getCenterLatLon();
        
        _centerLatLon = newCenter;
        
        System.out.println(header + "oldCenter = " + oldCenter + " newCenter = " + newCenter);
        System.out.println(header + "width " + getMapWidth() + " height = " + getMapHeight());

        
        setLatLonBoundsInternally();
        
         return;
    }
    
    // ----------------------------------------------------------------
    
    public void setMapScaleFactor(double mapScaleFactor)
    {
        _mapScaleFactor = mapScaleFactor;
    }
    // ----------------------------------------------------------------
    
    public double getMapScaleFactor()
    {
        return _mapScaleFactor;
    }
    
    // ----------------------------------------------------------------
    public void setMapWidthAndHeight(int mapWidth, int mapHeight)
    {
        _mapWidth = mapWidth;
        _mapHeight = mapHeight;
        setLatLonBoundsInternally();
    }
    
    public void setMapWidth(int mapWidth)
    {
        _mapWidth = mapWidth;
        setLatLonBoundsInternally();
    }
    
    // ----------------------------------------------------------------
    
    public int getMapWidth()
    {
        return _mapWidth;
    }

 
  
   
}
