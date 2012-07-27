package gov.noaa.nws.ncep.viz.rsc.ncgrid.customCoverage;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.geospatial.MapUtil;


public class CustomMercatorCoverage extends CustomCoverage {

	private static final long serialVersionUID = 3140441023975157052L;
	
	/** The minor axis of the earth */
    private double minorAxis;
    
    /** The major axis of the earth */
    private double majorAxis;
    
    /** Number of points along a parallel */
    private Integer nx;

    /** Number of points along a meridian */
    private Integer ny;
    
    /** Latitude of the first grid point */
    private double la1;

    /** Longitude of the first grid point */
    private double lo1;
    
    /**
     * latitude at which the Mercator projection intersects the Earth (Latitude
     * where Di and Dj are specified)
     */
    private double latin;
    
    /** Latitude of the last grid point */
    private double la2;

    /** Longitude of the last grid point */
    private double lo2;
    
    /** Longitudinal direction grid length */
    private double dx;

    /** Latitudinal direction grid length */
    private double dy;
    
    /** Spacing unit of dx and dy */
    private String spacingUnit;
    
    public boolean build() {
        double meridian = (lo1 + lo2) / 2;
        if (lo2 < lo1) {
            meridian += 180;
        }
        if (meridian > 180) {
            meridian -= 360;
        }
        crs = MapUtil.constructMercator(majorAxis, minorAxis, latin, meridian);
        try {
            // geometry = MapUtil.createGeometry(la1, lo1, la2, lo2);
            Unit<?> spacingUnitObj = Unit.valueOf(spacingUnit);
            if (spacingUnitObj.isCompatible(SI.METRE)) {
                UnitConverter converter = spacingUnitObj
                        .getConverterTo(SI.METRE);
                geometry = MapUtil.createGeometry(crs, la1, lo1,
                        converter.convert(dx), converter.convert(dy), nx, ny);
            } else {
                return false;
            }

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
     * Set ny
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
     * Set la1
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
     * @return The lo1 value
     */
    public double getLo1() {
        return lo1;
    }

    /**
     * Sets lo1
     * 
     * @param lo1
     *            The l1 value
     */
    public void setLo1(double lo1) {
        this.lo1 = lo1;
    }

    /**
     * Gets latin
     * 
     * @return The latin latitude value
     */
    public double getLatin() {
        return latin;
    }
    
    /**
     * Sets latin
     * 
     * @param latin
     *            The latin latitude value
     */
    public void setLatin(double latin) {
        this.latin = latin;
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
     * Sets la2
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
     * @return The lo2 latitude value
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
     *            The dx/dy spacing unit
     */
    public void setSpacingUnit(String spacingUnit) {
        this.spacingUnit = spacingUnit;
    }

    /**
     * Gets the minor axis
     * 
     * @return The minor axis value
     */
    public double getMinorAxis() {
        return minorAxis;
    }

    /**
     * Sets the minor axis
     * 
     * @param minorAxis
     *            The minor axis value
     */
    public void setMinorAxis(double minorAxis) {
        this.minorAxis = minorAxis;
    }

    /**
     * Gets the major axis
     * 
     * @return The major axis value
     */
    public double getMajorAxis() {
        return majorAxis;
    }

    /**
     * Sets the major axis
     * 
     * @param majorAxis
     *            The major axis value
     */
    public void setMajorAxis(double majorAxis) {
        this.majorAxis = majorAxis;
    }
}
