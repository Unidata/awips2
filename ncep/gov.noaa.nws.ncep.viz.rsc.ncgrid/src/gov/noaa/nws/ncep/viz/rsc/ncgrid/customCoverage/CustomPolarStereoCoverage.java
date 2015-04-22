package gov.noaa.nws.ncep.viz.rsc.ncgrid.customCoverage;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.geospatial.MapUtil;

public class CustomPolarStereoCoverage extends CustomCoverage {
	private static final long serialVersionUID = 2640862310607194072L;
	
    /** The minor axis of the earth */
    private double minorAxis;

    /** The major axis of the earth */
    private double majorAxis;

    /** Number of points along the x-axis */
    private Integer nx;

    /** Number of points along the y-axis */
    private Integer ny;

    /** Latitude of the first grid point */
    private double la1;

    /** Longitude of the first grid point */
    private double lo1;

    /** Orientation of the grid */
    private double lov;

    /** X-direction grid length */
    private double dx;

    /** Y-direction grid length */
    private double dy;

    /** Spacing unit of dx and dy */
    private String spacingUnit;

    public boolean build () {
        crs = MapUtil
                .constructNorthPolarStereo(majorAxis, minorAxis, 60.0, lov);

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

	public double getMinorAxis() {
		return minorAxis;
	}

	public void setMinorAxis(double minorAxis) {
		this.minorAxis = minorAxis;
	}

	public double getMajorAxis() {
		return majorAxis;
	}

	public void setMajorAxis(double majorAxis) {
		this.majorAxis = majorAxis;
	}

	public Integer getNx() {
		return nx;
	}

	public void setNx(Integer nx) {
		this.nx = nx;
	}

	public Integer getNy() {
		return ny;
	}

	public void setNy(Integer ny) {
		this.ny = ny;
	}

	public double getLa1() {
		return la1;
	}

	public void setLa1(double la1) {
		this.la1 = la1;
	}

	public double getLo1() {
		return lo1;
	}

	public void setLo1(double lo1) {
		this.lo1 = lo1;
	}

	public double getLov() {
		return lov;
	}

	public void setLov(double lov) {
		this.lov = lov;
	}

	public double getDx() {
		return dx;
	}

	public void setDx(double dx) {
		this.dx = dx;
	}

	public double getDy() {
		return dy;
	}

	public void setDy(double dy) {
		this.dy = dy;
	}

	public String getSpacingUnit() {
		return spacingUnit;
	}

	public void setSpacingUnit(String spacingUnit) {
		this.spacingUnit = spacingUnit;
	}
}
