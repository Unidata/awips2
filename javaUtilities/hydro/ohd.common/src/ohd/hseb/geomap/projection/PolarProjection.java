package ohd.hseb.geomap.projection;

import java.awt.Point;

import ohd.hseb.geomap.model.LatLonPoint;

public class PolarProjection extends BaseMapProjection
{

    
    public PolarProjection(LatLonPoint centerLatLonPoint, double mapScaleFactor)
    {
        setCenterLatLon(centerLatLonPoint);
        setMapScaleFactor(mapScaleFactor);
    }
    
    @Override
    public LatLonPoint getLatLonPoint(Point screenPoint)
    {
        return getLatLonPoint(screenPoint.x, screenPoint.y);
    }

    @Override
    public LatLonPoint getLatLonPoint(int x, int y)
    {

        double c;
        double py;
        double px;
        double p;

        /* Be sure to take into account any panning/recentering effects. */
        x = x - getOffsetX();
        y = (-1 * y) + getOffsetY();

        py = (double)y;
        px = (double)x;

        double lon = ((getCenterLatLon().getLon() * RADIANS_PER_DEGREE) + Math.atan2(py,px)) * DEGREES_PER_RADIAN + 90.0;

        p = Math.pow(((px*px) + (py*py)),0.5);
        c = 2.0 * Math.atan(p/(2.0 * ( double ) getMapScaleFactor()));

        double lat = Math.asin(Math.cos(c) * Math.sin((90.0 *RADIANS_PER_DEGREE)) +
                (py * Math.sin(c) * Math.cos((90.0 * RADIANS_PER_DEGREE))/p)) * DEGREES_PER_RADIAN;




        return new LatLonPoint(lat, lon);
    }

    @Override
    public Point getScreenPoint(LatLonPoint latLonPoint)
    { 
        return getScreenPoint(latLonPoint.getLat(), latLonPoint.getLon());
    }

    @Override
    public Point getScreenPoint(double lat, double lon)
    {
        double k;
        double temp;
        double centerLon;
        
        int x = 0;
        int y = 0;

        lat = lat * RADIANS_PER_DEGREE;
        lon = lon * RADIANS_PER_DEGREE;
        centerLon = getCenterLatLon().getLon() * RADIANS_PER_DEGREE;
        
        temp =  1 + Math.sin(lat);
        
        if (temp == 0.0)
        {
            System.out.println("Error\n");
        }
        else
        {
            k = getMapScaleFactor();
            
            double tangentTerm = 2.0 * k * Math.tan((Math.PI /4.0) - (lat / 2.0)) ;

            temp = tangentTerm * Math.sin(lon - centerLon);
            x = (int)temp + getOffsetX();

            temp = -tangentTerm * Math.cos(lon - centerLon);
            y = getOffsetY() - (int)temp;
        }

        return new Point(x, y);
    }

}
