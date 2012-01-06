/*
 * SymbolPattern
 * 
 * Date created: 10 DECEMBER 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Defines a symbol pattern that can be used to create a displayable symbol.
 * The symbol pattern is made up of one or more symbols parts, which are a sequence
 * of points whose line segments are part of the symbol.  The coordinates used for
 * the pattern assume that the center of the symbol is at coordinate (0,0).
 * @author sgilbert
 *
 */
@XmlAccessorType(XmlAccessType.NONE)
public class SymbolPattern {

	/**
	 * Name given to this symbol pattern
	 */
	@XmlAttribute
	private String name;
	
	/**
	 * List of SymbolParts that make up this pattern
	 */
	@XmlElements({
        @XmlElement(name = "pathPart", type = SymbolPathPart.class),
        @XmlElement(name = "circlePart", type = SymbolCirclePart.class) })
	private ArrayList<SymbolPart> parts;

	public SymbolPattern() {
		
	}
	
	/**
	 * Creates a new SymbolPattern with the given name
	 * @param name Name of this Symbol pattern
	 */
	public SymbolPattern(String name) {
		this.name = name;
		parts = new ArrayList<SymbolPart>();
	}
	
	/**
	 * Creates a new SymbolPattern with the given name and list of SymbolParts
	 * @param name Name of this symbol pattern
	 * @param parts - List of symbol parts
	 */
	public SymbolPattern(String name, ArrayList<SymbolPart> parts) {
		this.name = name;
		this.parts = parts;
	}

	/**
	 * Gets the name of this symbol pattern
	 * @return the pattern name
	 */
	public String getName() {
		return name;
	}

	/**
	 * Sets the name of this symbol pattern
	 * @param name the pattern name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * Gets the list of Symbol parts for this pattern
	 * @return the list of symbol parts
	 */
	public ArrayList<SymbolPart> getParts() {
		return parts;
	}

	/**
	 * Sets a list of symbol parts for this pattern
	 * @param parts the list of symbol parts to set
	 */
	public void setParts(ArrayList<SymbolPart> parts) {
		this.parts = parts;
	}
	
	/**
	 * Add a coordinate sequence (line path) as a new part for this symbol pattern.
	 * The fill flag is set to false by default.
	 * @param path The coordinate sequence defining the line path
	 */
	public void addPath(Coordinate[] path) {
		this.parts.add( new SymbolPathPart(path, false) );
	}
	
	/**
	 * Add a coordinate sequence (line path) as a new part for this symbol pattern,
	 * specify if the fill flag is set.
	 * @param path The coordinate sequence defining the line path
	 * @param fill flag specifying whether line path should be filled.
	 */
	public void addPath(Coordinate[] path, boolean fill) {
		this.parts.add( new SymbolPathPart(path, fill) );
	}
	
	/**
	 * Add a filled "dot" as a new part for this symbol pattern
	 * @param center Coordinate of the center of the dot
	 * @param radius Radius specifying the size of the dot
	 */
	public void addDot(Coordinate center, double radius) {
		this.parts.add( new SymbolCirclePart(center,radius,true));
	}

	/**
	 * Add an open "dot" (circle) as a new part for this symbol pattern
	 * @param center Coordinate of the center of the circle
	 * @param radius Radius specifying the size of the circle
	 */
	public void addCircle(Coordinate center, double radius) {
		this.parts.add( new SymbolCirclePart(center,radius,false));
	}
	
}
