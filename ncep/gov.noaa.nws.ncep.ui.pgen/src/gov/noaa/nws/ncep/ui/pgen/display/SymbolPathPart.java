/*
 * SymbolPart
 * 
 * Date created: 10 DECEMBER 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * This class defines a single part of a symbol pattern. It basically contains a
 * line path and a flag indicating whether the area defined by the path should be filled.
 * The coordinates used for
 * the pattern assume that the center of the symbol is at coordinate (0,0).
 * @author sgilbert
 *
 */
@XmlAccessorType(XmlAccessType.NONE)
public class SymbolPathPart extends SymbolPart {

	/**
	 * Array of coordinates defining the line path
	 */
	@XmlElement(name="path")
    @XmlJavaTypeAdapter(value = CoordinateArrayAdapter.class)
	private Coordinate[] path;
	
	/**
	 * Indicates whether area defined by path should be filled in.
	 */
	@XmlAttribute(name="isFilled")
	private boolean filled;

	/**
	 * default no-arg constructor
	 */
	public SymbolPathPart() {
		
	}
	
	/**
	 * Constructor to set path and filled flag.
	 * @param path Line path
	 * @param filled flag indicating fill
	 */
	public SymbolPathPart(Coordinate[] path, boolean filled) {
		this.path = path;
		this.filled = filled;
	}
	
	/**
	 * Gets the coordinates defining the line path
	 * @return the line path
	 */
	public Coordinate[] getPath() {
		return path;
	}

	/**
	 * Sets the coordinates defining the line path
	 * @param path the line path to set
	 */
	public void setPath(Coordinate[] path) {
		this.path = path;
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
	
}
