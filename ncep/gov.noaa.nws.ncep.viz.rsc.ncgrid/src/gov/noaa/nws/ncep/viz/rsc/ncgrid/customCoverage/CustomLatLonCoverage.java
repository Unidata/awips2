package gov.noaa.nws.ncep.viz.rsc.ncgrid.customCoverage;

import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.geotools.referencing.datum.DefaultGeodeticDatum;
import org.geotools.referencing.datum.DefaultPrimeMeridian;

import com.raytheon.uf.common.geospatial.MapUtil;

public class CustomLatLonCoverage extends CustomCoverage {
	private static final long serialVersionUID = 8371251040172233074L;
	
	private static final DefaultGeographicCRS WGS84 = DefaultGeographicCRS.WGS84;
	
	/** Number of points along a parallel */
	private Integer nx;
	
	/** Number of points along a meridian */
	private Integer ny;
	
	/** Latitude of first grid point */
    private double la1;

    /** Longitude of the first grid point */
    private double lo1;

    /** Latitude of the last grid point */
    private double la2;

    /** Longitude of the last grid point */
    private double lo2;

    /** I direction increment */
    private double dx;

    /** J direction increment */
    private double dy;

    /** Spacing unit of dx and dy */
    private String spacingUnit;

    /**  build custom lat/lon coverage*/
    public boolean build()  {
        double maxLon;
        double minLat = MapUtil.correctLat(la1);
        double maxLat = MapUtil.correctLat(la2);
        double minLon = MapUtil.correctLon(lo1);
        if (lo2 >= 360.0) {
            maxLon = lo2;
        } else {
            maxLon = MapUtil.correctLon(lo2);
        }
        if (maxLon < minLon) {
            maxLon += 360.0;
        }
        if (maxLon > 180) {
            crs = new DefaultGeographicCRS(new DefaultGeodeticDatum("WGS84",
                    WGS84.getDatum().getEllipsoid(), new DefaultPrimeMeridian(
                            "DateLine", 180.0)), WGS84.getCoordinateSystem());
        } else {
            crs = WGS84;
        }
        try {
            double xOffset = dx * 0.5;
            double yOffset = dy * 0.5;
            geometry = MapUtil.createGeometry(minLat + yOffset, minLon
                    - xOffset, maxLat - yOffset, maxLon + xOffset);
        } catch (Exception e) {
            return false;
        }
        return true;
    }

    public Integer getNx() {
        return nx;
    }

    public Integer getNy() {
        return ny;
    }
    
    /**
     * Sets nx
     * 
     * @param nx
     *            The nx value
     */
    public void setNx(Integer nx) {
        this.nx = nx;
    }

    /**
     * Sets ny
     * 
     * @param ny
     *            The ny value
     */
    public void setNy(Integer ny) {
        this.ny = ny;
    }

    /**
     * Gets la1
     * 
     * @return The la1 latitude value
     */
    public double getLa1() {
        return la1;
    }

    /**
     * Sets la1
     * 
     * @param la1
     *            The la1 latitude value
     */
    public void setLa1(double la1) {
        this.la1 = la1;
    }

    /**
     * Gets lo1
     * 
     * @return The lo1 longitude value
     */
    public double getLo1() {
        return lo1;
    }

    /**
     * Sets lo1
     * 
     * @param lo1
     *            The lo1 longitude value
     */
    public void setLo1(double lo1) {
        this.lo1 = lo1;
    }

    /**
     * Gets la2
     * 
     * @return The la2 latitude value
     */
    public double getLa2() {
        return la2;
    }

    /**
     * Sets la1
     * 
     * @param la2
     *            The la2 latitude value
     */
    public void setLa2(double la2) {
        this.la2 = la2;
    }

    /**
     * Gets lo2
     * 
     * @return The lo2 longitude value
     */
    public double getLo2() {
        return lo2;
    }

    /**
     * Sets lo2
     * 
     * @param lo2
     *            The lo2 longitude value
     */
    public void setLo2(double lo2) {
        this.lo2 = lo2;
    }

    /**
     * Gets dx
     * 
     * @return The dx value
     */
    public double getDx() {
        return dx;
    }

    /**
     * Sets dx
     * 
     * @param dx
     *            The dx value
     */
    public void setDx(double dx) {
        this.dx = dx;
    }

    /**
     * Gets dy
     * 
     * @return The dy value
     */
    public double getDy() {
        return dy;
    }

    /**
     * Sets dy
     * 
     * @param dy
     *            The dy value
     */
    public void setDy(double dy) {
        this.dy = dy;
    }

    /**
     * Gets the dx/dy spacing unit
     * 
     * @return The dx/dy spacing unit
     */
    public String getSpacingUnit() {
        return spacingUnit;
    }

    /**
     * Sets the dx/dy spacing unit
     * 
     * @param spacingUnit
     */
    public void setSpacingUnit(String spacingUnit) {
        this.spacingUnit = spacingUnit;
    }

}
