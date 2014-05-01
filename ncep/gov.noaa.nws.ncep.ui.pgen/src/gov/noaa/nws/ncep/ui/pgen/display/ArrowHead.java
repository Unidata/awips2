/*
 * ArrowHead
 * 
 * Date created: 18 DECEMBER 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * This class 
 * @author sgilbert
 *
 */
public class ArrowHead {
	
	/**
	 * Define the possible arrow head types that can be added to the end of a line path.
	 * @author sgilbert
	 *
	 */
	public static enum ArrowHeadType { OPEN, FILLED };
	
	/**
	 * The coordinate location of the point of the arrow
	 */
	private Coordinate location;
	
	/**
	 * The angle of the point of the arrow defining how wide or narrow the arrow head is.
	 */
	private double pointAngle;
	
	/**
	 * direction the arrow head should point
	 */
	private double direction;
	
	/**
	 * distance the arrow head side points should be from the center line of the arrow head
	 */
	private double extent;
	
	/**
	 * Type of arrow head.  See enum ArrowHeadType
	 */
	private ArrowHeadType type;
	
	/**
	 * Creates a arrow head object using the given info.
	 * @param location coordinates of the point of the arrow head
	 * @param pointAngle The internal angle, in degrees, defining the narrowness of the arrow head
	 * @param direction Direction the arrow head points
	 * @param extent Distance the side points should be from the center line
	 * @param type Arrow head type. OPEN or FILLED
	 */
	public ArrowHead(Coordinate location, double pointAngle, double direction,
			double extent, ArrowHeadType type) {
		this.location = location;
		this.pointAngle = pointAngle;
		this.direction = direction;
		this.extent = extent;
		this.type = type;
	}

	/**
	 * Calculates the points that define an arrow head 
	 * @return Array of coordinate points defining the arrow head.
	 */
    public Coordinate[] getArrowHeadShape() {
    	
    	Coordinate[] ahead = null;
    	double halfAngle = pointAngle * 0.5;
    	
    	/*
    	 * Calculate length of arrow head sides adjacent to the point given
    	 * the specified height
    	 */
    	double length = extent / Math.sin(Math.toRadians(halfAngle));
    	double supp = 180.0 - halfAngle;
    	
    	/*
    	 * Calculate one end of arrow head
    	 */
    	double radang = Math.toRadians(direction + supp);
    	Coordinate side1 = new Coordinate( location.x + (length * Math.cos(radang)),
    			                   location.y + (length * Math.sin(radang)) );
    	
    	/*
    	 * Calculate the other end of arrow head
    	 */
    	radang = Math.toRadians(direction - supp);
    	Coordinate side2 = new Coordinate( location.x + (length * Math.cos(radang)),
    			                   location.y + (length * Math.sin(radang)) );

    	/*
    	 * Return arrow head coordinates
    	 */
    	switch ( type ) {
    	case OPEN:
        	ahead = new Coordinate[] { side1, location, side2 };
    		break;
    	case FILLED:
            ahead = new Coordinate[] { side1, location, side2, side1 };
    		break;
    	}
    	
    	return ahead;
    }

    /**
     * Gets the length of the arrow head from the center of the base to the point.
     * @return length of the arrow head
     */
    public double getLength() {
    	
    	return extent / Math.tan(Math.toRadians(pointAngle*0.5));
    	
    }

}
