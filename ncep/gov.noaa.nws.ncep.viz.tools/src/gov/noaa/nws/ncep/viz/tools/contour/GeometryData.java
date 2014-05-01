/*
 * GeometryData
 * 
 * Date created 21 JUNE 2010
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.tools.contour;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Holds the area and centroid of a polygon.  Implements the Comparable interface so
 * that polygons can be ordered by area.  The centroid is used to distinguish between
 * two polygons with the same area.
 * @author sgilbert
 *
 */
public class GeometryData implements Comparable<GeometryData> {

	Double area;
	Coordinate centroid;
	
	GeometryData ( Geometry geom ) {
		area = geom.getArea();
		centroid = geom.getCentroid().getCoordinate();
	}
	
	
	public Double getArea() {
		return area;
	}


	public Coordinate getCentroid() {
		return centroid;
	}

	@Override
	public boolean equals(Object obj) {

		if ( ! (obj instanceof GeometryData) ) return false;
		
		GeometryData gd = (GeometryData)obj;
		
		return ( this.area.equals(gd.getArea()) && this.centroid.equals2D(gd.getCentroid()) );
	}

	@Override
	public int compareTo(GeometryData o) {

		if ( ! this.area.equals(o.getArea()) ) {
			return this.area.compareTo( o.getArea() );
		}
		else {
			return this.centroid.compareTo( o.getCentroid() );
		}
		
	}

}
