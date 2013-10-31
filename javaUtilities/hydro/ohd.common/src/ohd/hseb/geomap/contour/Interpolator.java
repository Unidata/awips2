package ohd.hseb.geomap.contour;

public class Interpolator
{
//  -----------------------------------------------------------------------------------------
    public Interpolator()
    {
        
    }
//  -----------------------------------------------------------------------------------------
    public double interpolate(double targetX, double x1, double y1, double x2, double y2)
    {
        double slope = (y2 - y1) / (x2 - x1);
        double intercept = (y1) - (slope*x1);

        double y = (slope*targetX) + intercept; 
        
        return y;
    }
    
    // -----------------------------------------------------------------------------------------

}
