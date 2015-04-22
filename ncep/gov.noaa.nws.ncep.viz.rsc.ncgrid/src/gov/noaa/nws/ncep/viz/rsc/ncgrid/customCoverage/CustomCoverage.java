package gov.noaa.nws.ncep.viz.rsc.ncgrid.customCoverage;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;

public abstract class CustomCoverage implements ISpatialObject{

	private static final long serialVersionUID = -1355232934065074837L;
	
	protected Polygon geometry;
	
	protected CoordinateReferenceSystem crs;
	
	protected transient GridGeometry2D gridGeometry;
	
	protected CustomCoverage() {

    }
	
	public Polygon getGeometry() {
        return geometry;
    }
	
	public CoordinateReferenceSystem getCrs() {
        return crs;
    }
	
    /**
     * Sets the geometry
     * 
     * @param geometry
     *            The geometry
     */
    public void setGeometry(Geometry geometry) {
        this.geometry = (Polygon) geometry;
    }
    
    /**
     * Sets the CRS object
     * 
     * @param crs
     *            The crs object
     */
    public void setCrs(CoordinateReferenceSystem crs) {
        this.crs = crs;
    }
    
    public GridGeometry2D getGridGeometry() {
    	if (gridGeometry == null) {
            gridGeometry = MapUtil.getGridGeometry(this);
        }
        return gridGeometry;
    }

    public void setGridGeometry(GridGeometry2D gridGeometry) {
        this.gridGeometry = gridGeometry;
    }
}
