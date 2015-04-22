/*
 * SinglePointElement
 * 
 * Date created: 15 January 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.elements;

import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.ISinglePoint;

import java.awt.Color;
import java.util.ArrayList;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Class to represent a single point element.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/09					J. Wu   	Initial Creation.
 * 05/09        #42         S. Gilbert  Added pgenType and pgenCategory to constructors
 * 04/11		#?			B. Yin		Re-factor IAttribute
 *
 * </pre>
 * 
 * @author	J. Wu
 * @version	0.0.1
 */
public abstract class SinglePointElement extends DrawableElement implements ISinglePoint{

	Color[]		colors;
	float 		lineWidth;
	double		sizeScale;
	Boolean		clear;
	Coordinate	location;	
	
	
	/**
	 *  Default constructor
	 */
	protected SinglePointElement() {
		colors = new Color[]{Color.red};
		lineWidth = (float)1.0;
		sizeScale = 1.0;
		clear = false;
		location = null;
	}

	/**
	 * @param deleted
	 * @param range
	 * @param colors
	 * @param lineWidth
	 * @param sizeScale
	 * @param clear
	 * @param location
	 */
	protected SinglePointElement(Coordinate[] range,
			Color[] colors, float lineWidth, double sizeScale, Boolean clear,
			Coordinate location, String pgenCategory, String pgenType) {
		super(range, pgenCategory, pgenType);
		this.colors = colors;
		this.lineWidth = lineWidth;
		this.sizeScale = sizeScale;
		this.clear = clear;
		this.location = location;
	}

	/**
	 * Gets the Lat/lon location of the object
	 * @return Lat/lon coordinate
	 */
	@Override
	public Coordinate getLocation() {
		return location;
	}
	
	/**
	 * Gets array of colors associated with the object
	 * @return Color array
	 */
	@Override
	public Color[] getColors() {
		return colors;
	}
	
	/**
	 * Gets the width of the line pattern
	 * @return line width
	 */
	@Override
	public float getLineWidth() {
		return lineWidth;
	}
	
	/**
	 * Checks whether the background of the object should be cleared.
	 * @return true, if background should be cleared
	 */
	@Override
	public Boolean isClear() {
		return clear;
	}
	
	/**
	 * Gets the size scale factor for the object
	 * @return size scale factor
	 */
	@Override
	public double getSizeScale() {
		return sizeScale;
	}
	
	/**
	 * Sets the Lat/lon location of the object
	 */
	public void setLocation(Coordinate location) {
		setLocationOnly(location);
	}
	
	public void setLocationOnly(Coordinate location) {
		this.location = location;
	}	
	/**
	 * Sets the color list associated with the object
	 */
	public void setColors(Color[] colors) {
		if ( colors != null ){
			this.colors = colors;
		}
	}
	
	/**
	 * Sets the width of the line pattern
	 */
	public void setLineWidth(float lineWidth) {
		if ( !(new Float(lineWidth).isNaN() )){
			this.lineWidth = lineWidth;
		}
	}
	
	/**
	 * Sets whether the background of the object should be cleared.
	 */
	public void setClear(Boolean clear) {
		if ( clear != null ){
			this.clear = clear;
		}
	}
	
	/**
	 * Sets the size scale factor for the object
	 */
	public void setSizeScale(double sizeScale) {
		if ( !(new Double(sizeScale).isNaN())){
			this.sizeScale = sizeScale;
		}
	}

	/**
	 * Update the attributes
	 */	
	public void update(IAttribute iattr) {
		if ( iattr instanceof ISinglePoint ){
			ISinglePoint attr = (ISinglePoint)iattr; 
			this.setClear(attr.isClear());
			this.setColors(attr.getColors());
			this.setLineWidth(attr.getLineWidth());	    
			this.setSizeScale(attr.getSizeScale());
		}
	}
	
	public ArrayList<Coordinate> getPoints(){
		
		ArrayList<Coordinate> pts = new ArrayList<Coordinate>();
		pts.add(getLocation());
		return pts;
		
	}
	
	public void setPointsOnly( ArrayList<Coordinate> pts ){
		
		setLocationOnly(pts.get(0));
		
	}
	
	public String getPatternName(){
		return null;
	}

}

