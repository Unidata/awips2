/*
 * Arc
 * 
 * Date created: 27 April 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.elements;

import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.annotation.ElementOperations;
import gov.noaa.nws.ncep.ui.pgen.display.IArc;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;

import java.awt.Color;
import java.util.ArrayList;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Class to represent an Arc element.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 04/09		#89			J. Wu   	Initial Creation.
 * 05/09        #42         S. Gilbert  Added pgenType and pgenCategory to constructors and copy()
 * 04/11		#?			B. Yin		Re-factor IAttribute
 *
 * </pre>
 * 
 * @author	J. Wu
 * @version	0.1
 */
@ElementOperations ( {Operation.COPY_MOVE, Operation.EXTRAPOLATE} )
public class Arc extends Line implements IArc {
    
	private double axisRatio;
	private double startAngle;
	private double endAngle;
	
	/**
	 *  Default constructor
	 */
	public Arc() {
		axisRatio = 1.0;
		startAngle = 0.0;
		endAngle = 360.0;	    
	}

	
	/**
	 * @param range
	 * @param color
	 * @param lineWidth
	 * @param sizeScale
	 * @param closed
	 * @param filled
	 * @param linePoints
	 * @param smoothFactor
	 * @param fillPattern
	 * @param pgenType
	 * @param centerPoint
	 * @param circumfencePoint
	 * @param pgenCategory
	 * @param axisRatio
	 * @param startAngle
	 * @param endAngle
	 */
	public Arc( Coordinate[] range, Color color,
			float lineWidth, double sizeScale, boolean closed, boolean filled,
			int smoothFactor, FillPattern fillPattern, String pgenType, 
			Coordinate centerPoint, Coordinate circumfencePoint, String pgenCategory, 
			double axisRatio, double startAngle, double endAngle ) {
		
		super( range, new Color[]{color}, lineWidth, sizeScale, closed, filled,
			   new ArrayList<Coordinate>(), smoothFactor, fillPattern, pgenCategory, pgenType );
		
		this.axisRatio = axisRatio;
		this.startAngle= startAngle;
		this.endAngle = endAngle;

		this.setCenterPoint( centerPoint );
		this.setCircumferencePoint( circumfencePoint );		
				
	}

	/**
	 * @return the centerPoint
	 */
	@Override
	public Coordinate getCenterPoint() {
		return linePoints.get( 0 );
	}
	
	/**
	 * @return the circumferencePoint
    */
	@Override
	public Coordinate getCircumferencePoint() {
		return linePoints.get( 1 );
	}
	
	/**
	 * @return the axisRatio
	 */
	@Override
	public double getAxisRatio() {
		return axisRatio;
	}
	
	/**
	 * @return the startAngle
	 */
	@Override
	public double getStartAngle() {
		return startAngle;
	}
	
	/**
	 * @return the endAngle
	 */
	@Override
	public double getEndAngle() {
		return endAngle;
	}
		
	/**
	 * @param centerPoint the centerPoint to set
	 */
	public void setCenterPoint(Coordinate centerPoint) {

		if ( linePoints != null ) {
			linePoints.clear();
		}
				
		linePoints.add( 0, centerPoint );
		
	}
	
	/**
	 * @param circumferencePoint the circumferencePoint to set
	 */
	public void setCircumferencePoint(Coordinate circumferencePoint) {
		
		if ( linePoints != null && linePoints.size() > 1 ) {
			linePoints.remove( 1 );
		}
		
		linePoints.add( 1, circumferencePoint );

	}
	
	/**
	 * @param axisRatio the axisRatio to set
	 */
	public void setAxisRatio(double axisRatio) {
		
		if (!(new Double(axisRatio).isNaN())){
			this.axisRatio = axisRatio;
		}
	}
	
	/**
	 * @param startAngle the startAngle to set
	 */
	public void setStartAngle(double startAngle) {
		
		if (!(new Double(startAngle).isNaN())){

			this.startAngle = startAngle;
		}
	}
	
	/**
	 * @param endAngle the endAngle to set
	 */
	public void setEndAngle(double endAngle) {
		
		if (!(new Double(endAngle).isNaN())){

			this.endAngle = endAngle;
		}
	}

	/**
	 * Update the attributes for the object
	 */	
	@Override
	public void update(IAttribute iattr) {

		if ( iattr instanceof IArc ){
			IArc attr = (IArc) iattr;
			this.setAxisRatio(attr.getAxisRatio());
			this.setStartAngle(attr.getStartAngle());
			this.setEndAngle(attr.getEndAngle());
			this.setColors(attr.getColors() );
			this.setLineWidth(attr.getLineWidth());	    
			this.setSizeScale(attr.getSizeScale());
		}
	    		
	}

	/**
	 * Creates a copy of this object.  This is a deep copy and new objects are
	 * created so that we are not just copying references of objects
	 */
	@Override
	public DrawableElement copy() {
 		

		/*
		 * create a new Line object and initially set its attributes to this one's
		 */
		Arc newArc = new Arc();
		newArc.update( this );
		
		/*
		 * new Coordinates points are created and set, so we don't just set 
		 * references
		 */
		ArrayList<Coordinate> ptsCopy = new ArrayList<Coordinate>();
		for (int i=0; i < this.getPoints().size(); i++) {
			ptsCopy.add(new Coordinate(this.getPoints().get(i)));
		}
		newArc.setPoints(ptsCopy);
		
		/*
		 * new colors are created and set, so we don't just set 
		 * references
		 */
		Color[] colorCopy = new Color[ this.getColors().length ];
		for (int i = 0; i < this.getColors().length; i++) {
			colorCopy[ i ] = new Color(this.getColors()[ i ].getRed(),
					                 this.getColors()[ i ].getGreen(),
					                 this.getColors()[ i ].getBlue() );
		}
		
		newArc.setColors( colorCopy) ;
		
		/*
		 * new Strings are created for Type and LinePattern
		 */
		newArc.setPgenCategory( new String( this.getPgenCategory() ) );
		newArc.setPgenType( new String( this.getPgenType() ) );
		
		newArc.setParent(this.getParent());
		return newArc;

	}
	
	/**
	 * @return the string
	 */
	@Override
	public String toString() {
		StringBuilder	result = new StringBuilder( getClass().getSimpleName());

        result.append("Category:\t" + pgenCategory + "\n");        
        result.append("Type:\t" + pgenType + "\n");        
        result.append("Color:\t" + this.getColors()[0] + "\n");
        result.append("LineWidth:\t" + lineWidth + "\n");
        result.append("SizeScale:\t" + sizeScale + "\n");
        result.append("Closed:\t" + closed + "\n");
        result.append("Filled:\t" + filled + "\n");
        result.append("FillPattern:\t" + fillPattern + "\n");
        result.append("smoothLevel:\t" + smoothFactor + "\n");
        result.append("AxisRatio:\t" + axisRatio + "\n");
        result.append("StartAngle:\t" + startAngle + "\n");
        result.append("EndAngle:\t" + endAngle + "\n");
        if ( this.getCenterPoint() != null ) {
            result.append("CenterPoint:\t" + this.getCenterPoint().y + "\t" + this.getCenterPoint().x + "\n" ); 
        }
        else {
            result.append("CenterPoint:\tnot defined\n" );         	
        }
        
        if ( this.getCircumferencePoint() != null ) {       
            result.append("CircumfencePoint:\t" + this.getCircumferencePoint().y +  "\t" +
        		                              this.getCircumferencePoint().x + "\n" ); 
        }
        else {
            result.append("CircumfencePoint:\tnot defined\n" );         	
        }
  	 	       		
		return result.toString();
	}	




}
