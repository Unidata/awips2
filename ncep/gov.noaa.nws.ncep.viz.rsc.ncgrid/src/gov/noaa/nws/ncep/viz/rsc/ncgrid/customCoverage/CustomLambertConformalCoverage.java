package gov.noaa.nws.ncep.viz.rsc.ncgrid.customCoverage;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.geospatial.MapUtil;


public class CustomLambertConformalCoverage extends CustomCoverage {
	private static final long serialVersionUID = 5113332463602932317L;
	
	/** The minor axis of the Earth */
    private double minorAxis;

    /** The major axis of the Earth */
    private double majorAxis;

    /** Number of points along the x-axis */
    private Integer nx;

    /** Number of points along the y-axis */
    private Integer ny;

    /** Latitude of the first grid point */
    private double la1;

    /** Longitude of the first grid point */
    private double lo1;

    /**
     * Longitude of meridian parallel to y-axis along which latitude increases
     * as the y-coordinate increases
     */
    private double lov;

    /** X-direction grid length in units specified by spacingUnit **/
    private double dx;

    /** Y-direction grid length in units specified by spacingUnit **/
    private double dy;

    /** Spacing unit of dx and dy */
    private String spacingUnit;

    /** First latitude from the pole at which the secant cone cuts the sphere **/
    private double latin1;

    /** Second latitude from the pole at which the secant cone cuts the sphere **/
    private double latin2;

    public boolean build() {
        crs = MapUtil.constructLambertConformal(majorAxis, minorAxis, latin1,
                latin2, lov);
        try {
            Unit<?> spacingUnitObj = Unit.valueOf(spacingUnit);
            if (spacingUnitObj.isCompatible(SI.METRE)) {
                UnitConverter converter = spacingUnitObj
                        .getConverterTo(SI.METRE);
                geometry = MapUtil.createGeometry(crs, la1, lo1, converter
                        .convert(dx), converter.convert(dy), nx, ny);
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
     * Gets the minor axis
     * 
     * @return The minor axis
     */
    public double getMinorAxis() {
        return minorAxis;
    }

    /**
     * Sets the minor axis
     * 
     * @param minorAxis
     *            The minor axis
     */
    public void setMinorAxis(double minorAxis) {
        this.minorAxis = minorAxis;
    }

    /**
     * Gets the major axis
     * 
     * @return The major axis
     */
    public double getMajorAxis() {
        return majorAxis;
    }

    /**
     * Sets the major axis
     * 
     * @param majorAxis
     *            The major axis
     */
    public void setMajorAxis(double majorAxis) {
        this.majorAxis = majorAxis;
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
     * @return la1 latitude value
     */
    public double getLa1() {
        return la1;
    }

    /**
     * Sets la1
     * 
     * @param la1
     *            la1 latitude value
     */
    public void setLa1(double la1) {
        this.la1 = la1;
    }

    /**
     * Gets lo1
     * 
     * @return lo1 longitude value
     */
    public double getLo1() {
        return lo1;
    }

    /**
     * Sets lo1
     * 
     * @param lo1
     *            lo1 longitude value
     */
    public void setLo1(double lo1) {
        this.lo1 = lo1;
    }

    /**
     * Gets lov
     * 
     * @return Lov longitude value
     */
    public double getLov() {
        return lov;
    }

    /**
     * Sets lov
     * 
     * @param lov
     *            lov longitude value
     */
    public void setLov(double lov) {
        this.lov = lov;
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
     * Gets latin1
     * 
     * @return Gets the latin1 latitude value
     */
    public double getLatin1() {
        return latin1;
    }

    /**
     * Sets latin1
     * 
     * @param latin1
     *            The latin1 latitude value
     */
    public void setLatin1(double latin1) {
        this.latin1 = latin1;
    }

    /**
     * Gets latin2
     * 
     * @return The latin2 latitude value
     */
    public double getLatin2() {
        return latin2;
    }

    /**
     * Sets latin2
     * 
     * @param latin2
     *            The latin2 latitude value
     */
    public void setLatin2(double latin2) {
        this.latin2 = latin2;
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
}
