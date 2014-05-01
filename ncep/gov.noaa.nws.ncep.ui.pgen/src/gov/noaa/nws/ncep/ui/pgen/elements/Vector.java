/*
 * Vector
 * 
 * Date created: May 7th, 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.elements;

import java.awt.Color;

import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;

import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.annotation.ElementOperations;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.IVector;

/**
 * Class to represent a symbol element.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/09					J. Wu   	Initial Creation.
 * 04/11		#?			B. Yin		Re-factor IAttribute
 *
 * </pre>
 * 
 * @author	J. Wu
 * @version	0.0.1
 */
@ElementOperations ( {Operation.COPY_MOVE, Operation.EXTRAPOLATE, Operation.ROTATE} )
public class Vector extends SinglePointElement implements IVector {
    
	private VectorType vectorType;
	private double	speed;
	private double  direction;
	private double  arrowHeadSize;	
	private boolean directionOnly;
	
	/**
	 *  Default constructor
	 */
	public Vector() {
	}

	/**
	 * @param deleted
	 * @param range
	 * @param colors
	 * @param lineWidth
	 * @param sizeScale
	 * @param clear
	 * @param location
	 * @param type
	 */
	public Vector( Coordinate[] range, Color[] colors,
			float lineWidth, double sizeScale, Boolean clear,
			Coordinate location, VectorType vc,
			double speed, double direction, double arrowHeadSize, 
			boolean directionOnly, String pgenCategory, String pgenType ) {

		super( range, colors, lineWidth, sizeScale, clear, location, pgenCategory, pgenType );

		this.vectorType = vc;
		this.speed = speed;
		this.direction = direction;
		this.arrowHeadSize = arrowHeadSize;
		this.directionOnly = directionOnly;
		
	}	
	
	/**
	 * @param vectorType the vectorType to set
	 */
	public void setVectorType(VectorType vectorType) {
		this.vectorType = vectorType;
	}

	/**
	 * @return the vectorType
	 */
	public VectorType getVectorType() {
		return vectorType;
	}

	/**
	 * @return the first color
	 */
	public Color getColor() {
		return colors[0];
	}
	
	/**
	 * @param speed the speed to set
	 */
	public void setSpeed(double speed) {
		if ( !(new Double(speed).isNaN())){
			this.speed = speed;
		}
	}

	/**
	 * @return the speed
	 */
	public double getSpeed() {
		return speed;
	}

	/**
	 * @param direction the direction to set
	 */
	public void setDirection(double direction) {
		if ( !(new Double(direction).isNaN())){
			this.direction = direction;
		}
	}

	/**
	 * @return the direction
	 */
	public double getDirection() {
		return direction;
	}

	/**
	 * @param arrowHeadSize the arrowHeadSize to set
	 */
	public void setArrowHeadSize(double arrowHeadSize) {
		if ( !(new Double(arrowHeadSize).isNaN())){
			this.arrowHeadSize = arrowHeadSize;
		}
	}

	/**
	 * @return the arrowHeadSize
	 */
	public double getArrowHeadSize() {
		return arrowHeadSize;
	}

	/**
	 * @param directionOnly the directionOnly to set
	 */
	public void setDirectionOnly(boolean directionOnly) {
		this.directionOnly = directionOnly;
	}

	/**
	 * @return the directionOnly
	 */
	public boolean hasDirectionOnly() {
		return directionOnly;
	}	

	/**
	 * @return the background mask (clear)
	 */
	public Boolean hasBackgroundMask() {
		return clear;
	}	

	/**
	 *  update attributes for the object
	 */	
	@Override
	public void update( IAttribute iattr ) {
		
		if ( iattr instanceof IVector ){
			super.update( iattr );

			IVector attr = (IVector) iattr;
			//Not using setSpeed because it triggers snap when updating jet barbs.
			double spd = attr.getSpeed();
			if ( !(new Double(spd).isNaN()) ){
				this.speed = spd;
			}

			this.setDirection(attr.getDirection());
			this.setArrowHeadSize(attr.getArrowHeadSize());
			this.setClear( attr.hasBackgroundMask() );
		}
        
	}

	/**
	 * @return the string
	 */
	public String toString() {
		StringBuilder	result = new StringBuilder( getClass().getSimpleName());

        result.append( "\nCategory:\t" + pgenCategory + "\n" );
        result.append( "Type:\t" + pgenType + "\n" );

        result.append("Location:\t" + location.y + "\t" + location.x + "\n");
        result.append("Color:\t" + colors[0] + "\n");
        result.append("LineWidth:\t" + lineWidth + "\n");
        result.append("SizeScale:\t" + sizeScale + "\n");
        result.append("Clear:\t" + clear + "\n");
        result.append("VectorType:\t" + vectorType + "\n");
        result.append("Speed:\t" + speed + "\n");
        result.append("Direction:\t" + direction + "\n");
        result.append("Directional:\t" + directionOnly + "\n");
        result.append("ArrowHeadSize:\t" + arrowHeadSize + "\n");
            	 	       		
		return result.toString();
	}

	/**
	 * Creates a copy of this object.  This is a deep copy and new objects are
	 * created so that we are not just copying references of objects
	 */
	@Override
	public DrawableElement copy() {

		/*
		 * create a new Vector object and initially set its attributes to this one's
		 */
		Vector newVector = new Vector();
		newVector.update( this );
		
		/*
		 * Set new Color, Strings and Coordinate so that we don't just set references to this
		 * object's attributes.
		 */
		newVector.setColors(new Color[] { new Color(this.getColors()[0].getRed(),
										            this.getColors()[0].getGreen(),
										            this.getColors()[0].getBlue())   });
		newVector.setLocation( new Coordinate( this.getLocation() ) );

		newVector.setPgenCategory( new String(this.getPgenCategory()) );
		newVector.setPgenType( new String(this.getPgenType()) );

		newVector.setVectorType( this.getVectorType() );
		newVector.setParent(this.getParent());
		
		return newVector;
		
	}

	/**
	 * Calculates the angle of a directional line (p1->p2) relative to the North. 
	 * @param point1 - The starting point in Lat/Lon coordinates
	 * @param point2 - The ending point in Lat/Lon coordinates
	 * @return The angle of line point1->point2 relative to the North
	 */
	public static double northRotationAngle( Coordinate point1, Coordinate point2 ) {

		/*
		 *  Note - Orientation will be clockwise as following: 
		 *  North 0; East 90; South 180; West 270;
		 */		        
		GeodeticCalculator gc = new GeodeticCalculator( DefaultEllipsoid.WGS84 );
		gc.setStartingGeographicPoint( point1.x , point1.y );
		gc.setDestinationGeographicPoint( point2.x , point2.y  );		
        
		double azimuth = gc.getAzimuth();		
		double angle = azimuth;
		if ( angle < 0.0 ) {
			angle += 360.0;		
		}
		
		return angle;
	}
	
	/**
	 * Calculates the vector direction given an arrow from point 1 to point 2, which
	 * also depends on the type of vectors.
	 * @param point1 - The starting point in Lat/Lon coordinates
	 * @param point2 - The ending point in Lat/Lon coordinates
	 * @return The vector direction for the arrow from point1 to point2
	 */
	public double vectorDirection( Coordinate point1, Coordinate point2 ) {

		/*
		 *  Note - Orientation will be as following: 
		 *  Arrow/Directional: North 180; East 270; South 0; West 90;
		 *  Barb: North 0; East 90; South 180; West 270;
		 *  Hash: North 90; East 0; South 270; West 180;
		 *  
		 *  Increment by every 5 degrees.
		 */		        
		double dir = Vector.northRotationAngle( point1,  point2 );
	   	
		if ( pgenType.equalsIgnoreCase( "Hash" ) ) {
	         
			dir = 90.0 - dir;
	         if ( dir < 0.0 ) dir += 360;
		
		}
		else if ( pgenType.equalsIgnoreCase( "Barb" ) ) {
			
		}
		else { // "Arrow" & "Directional"
		    
			dir += 180.0;
		    if ( dir > 360.0 ) {
			    dir -= 360.0;
		    }
		    
		}				
		
		double direction = ( (int)(dir + 3) ) / 5 * 5;
		
		return direction;
	}
}

