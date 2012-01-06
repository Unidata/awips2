/*
 * SymbolCirclePart
 * 
 * Date created: 18 MAY 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.serialization.adapters.CoordAdapter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * This class defines a circle part of a symbol pattern. The circle area is defined
 * by the center coordinate and the radius.  The actual coordinate points that make up the 
 * circle's path are calculated from the center point and radius.
 * The coordinates used for the pattern assume that the center of the symbol is at coordinate (0,0).
 * @author sgilbert
 *
 */
@XmlAccessorType(XmlAccessType.NONE)
public class SymbolCirclePart extends SymbolPart {

	/**
	 * Array of coordinates defining the arc of the circle
	 */
	private Coordinate[] path = null;
	private boolean pathNeedsUpdate = true;

	/**
	 * Center point of the circle
	 */
    @XmlJavaTypeAdapter(value = CoordAdapter.class)
	@XmlAttribute(name="center")
	private Coordinate center = null;
	
    /**
     * radius of the circle
     */
	@XmlAttribute(name="radius")
    private double radius = 0.0;
    
	/**
	 * Indicates whether area defined by path should be filled in.
	 */
	@XmlAttribute(name="isFilled")
	private boolean filled;

	/**
	 * default no-arg constructor
	 */
	public SymbolCirclePart() {
		
	}
	
	/**
	 * Constructor used to create a symbol part representing a dot (or circle) at
	 * the specified center with the specified radius
	 * @param center center point of the dot/circle
	 * @param radius radius of the dot/circle
	 * @param filled flag indicating whether part shuold be filled
	 */
	public SymbolCirclePart(Coordinate center, double radius, boolean filled) {

		this.center = center;
		this.radius = radius;
		this.filled = filled;
		
		updatePath();
		
	}

	/**
	 * Gets the coordinates defining the line path
	 * @return the line path
	 */
	public Coordinate[] getPath() {
		if ( pathNeedsUpdate ) updatePath();
		return path;
	}

	/**
	 * Gets whether area defined by line path should be filled
	 * @return the filled flag
	 */
	public boolean isFilled() {
		return filled;
	}

	/**
	 * Sets whether area defined by line path should be filled
	 * @param filled the filled flag to set
	 */
	public void setFilled(boolean filled) {
		this.filled = filled;
	}

	
	/**
	 * @return the center
	 */
	public Coordinate getCenter() {
		return center;
	}

	/**
	 * @param center the center to set
	 */
	public void setCenter(Coordinate center) {
		this.center = center;
		pathNeedsUpdate = true;
	}

	/**
	 * @return the radius
	 */
	public double getRadius() {
		return radius;
	}

	/**
	 * @param radius the radius to set
	 */
	public void setRadius(double radius) {
		this.radius = radius;
		pathNeedsUpdate = true ;
	}

	/**
	 * Calculates the coordinates defining the arc of the circle
	 * from the center point and radius.
	 */
	private void updatePath() {
		
		if ( (center == null) || (radius==0.0) ) return;
		
		double x,y;
		int numpts = 16;
		path = new Coordinate[numpts];
		
		double increment = 360.0 / (double)numpts;
		double angle = 0.0;
		for (int j=0; j<numpts; j++) {
			x = center.x + ( radius * Math.cos(Math.toRadians(angle)) );
			y = center.y + ( radius * Math.sin(Math.toRadians(angle)) );
			path[j] = new Coordinate(x,y);
			angle += increment;
		}
		pathNeedsUpdate = false;
	}
	
}
