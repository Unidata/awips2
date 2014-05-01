/*
 * PatternSegment
 * 
 * Date created: 24 NOVEMBER 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * A segment of a LinePattern.  Repeat PatternSegments can be applied to a line path
 * to create a pattern.
 * @author sgilbert
 *
 */
@XmlAccessorType(XmlAccessType.NONE)
public class PatternSegment implements ISerializableObject {
	
	/**
	 * Valid patterns that can be specified in a PatternSegment
	 * @author sgilbert
	 *
	 */
	public static enum PatternType { 
		BLANK, LINE, 
		CIRCLE, CIRCLE_FILLED, ARC_180_DEGREE, ARC_180_DEGREE_FILLED, 
		ARC_180_DEGREE_CLOSED, ARC_90_DEGREE, ARC_270_DEGREE, ARC_270_DEGREE_WITH_LINE,
		BOX, BOX_FILLED, X_PATTERN, Z_PATTERN, DOUBLE_LINE, TICK, ARROW_HEAD
		};

	/**
	 * Length of the pattern segment in pixels
	 */	
	@XmlAttribute
	private double length;
	
	/**
	 * Type of pattern
	 * @see gov.noaa.nws.ncep.ui.pgen.display.PatternSegment.PatternType
	 */
	@XmlAttribute(name="type")
	private PatternType attribute;
	
	/**
	 * An index in a list of colors 
	 */
	@XmlAttribute
	private int colorLocation;
	
	/**
	 * The number of individual segments to calculate around the circumference of an arc.  Used in
	 * the ARC Patterns.
	 */
	@XmlAttribute
	private int numberInArc;
	
	/**
	 * Pattern size (or number of pixels) to the left and right of the center line segment.
	 * Used in patterns such as BOX, X, Z, Tick, and Double Line.
	 */
	@XmlAttribute
	private int offsetSize;
	
	/** 
	 * By default, patterns are applied to the left side of the line path.  reverseSide set
	 * to true, applies patterns to the right side.
	 */	
	 @XmlAttribute
	private boolean reverseSide;
	
	public PatternSegment() {
		
	}
	
	/**
	 * Constructor for a new Pattern Segment
	 * @param length Length of pattern segment.
	 * @param attribute Type of pattern segment.
	 * @param colorLocation Index of color to use with segment.
	 * @param numberInArc Number of arc segemnts to use in ARC and CIRCLE Patterns.
	 * @param offsetSize Number of pixels for pattern left and right of center line.
	 * @param reverseSide Apply pattern to right side of line instead of the default left.
	 */
	public PatternSegment(double length, PatternType attribute, int colorLocation,
			int numberInArc, int offset, boolean reverseSide) {
		super();
		this.length = length;
		this.attribute = attribute;
		this.colorLocation = colorLocation;
		this.numberInArc = numberInArc;
		offsetSize = offset;
		this.reverseSide = reverseSide;
	}
	
	/**
	 * Gets length of segment
	 * @return length of segment
	 */
	public double getLength() {
		return length;
	}
	
	/**
	 * Set length of segment
	 * @param length
	 */
	public void setLength(double length) {
		this.length = length;
	}
	
	/**
	 * Gets segment pattern type
	 * @return Line pattern type
	 */
	public PatternType getPatternType() {
		return attribute;
	}
	
	/**
	 * Set line pattern type
	 * @param attr
	 */
	public void setPatternType(PatternType attr) {
		this.attribute = attr;
	}
	
	/**
	 * Gets index of color value
	 * @return color index
	 */
	public int getColorLocation() {
		return colorLocation;
	}
	
	/**
	 * Set index of color used in this segment
	 * @param colorLocation
	 */
	public void setColorLocation(int colorLocation) {
		this.colorLocation = colorLocation;
	}
	
	/**
	 * Gets number of segments in ARC
	 * @return number of segments to calculate in any ARC or CIRCLE
	 */
	public int getNumberInArc() {
		return numberInArc;
	}
	
	/**
	 * Set number of segments to calculate in any ARC or CIRCLE
	 * @param numberInArc
	 */
	public void setNumberInArc(int numberInArc) {
		this.numberInArc = numberInArc;
	}
	
	/**
	 * Gets size of pattern to left and right of line path
	 * @return  Size of pattern to left and right of line path
	 */
	public int getOffsetSize() {
		return offsetSize;
	}
	
	/**
	 * Set size of pattern to the left and right of the line path.
	 * @param offset
	 */
	public void setOffsetSize(int offset) {
		offsetSize = offset;
	}
	
	/**
	 * Gets reverse pattern side flag
	 * @return true, if pattern should be applied to right side of line path.
	 * Otherwise, return false to apply pattern to the default left side.
	 */
	public boolean isReverseSide() {
		return reverseSide;
	}
	
	/**
	 * Set whether pattern should be applied to right side (true) of line path or left side (false).
	 * @param reverseSide
	 */
	public void setReverseSide(boolean reverseSide) {
		this.reverseSide = reverseSide;
	}
		
	public PatternSegment copy() {

		return new PatternSegment( length, attribute, colorLocation,
				numberInArc, offsetSize, reverseSide);
	}
	
}
