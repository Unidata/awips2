/*
 * SymbolPatternMapEntry
 * 
 * Date created: 13 MAY 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 * This class represents one SymbolPattern entry in a hashMap in that it contains both the 
 * "key" and "value" as is stored in the SymbolPatternManager.
 *  An object of this class is used as an intermediate class by JAXB when marshaling/unmarshaling a 
 *  list of SymbolPatterns to/from an XML file.
 * @author sgilbert
 *
 */
@XmlAccessorType(XmlAccessType.NONE)
public class SymbolPatternMapEntry {

	/**
	 * A symbol pattern identifier
	 */
	@XmlElement
	private String patternId;
	
	/**
	 * A symbol pattern definition
	 */
	@XmlElement(name="symbolPattern")
	private SymbolPattern pattern;
	
	/**
	 * Default no-arg constructor
	 */
	public SymbolPatternMapEntry() {
		
	}
	
	/**
	 * Constructor specifying a LinePattern and its identifier
	 * @param patternId Line pattern identifier
	 * @param pattern Line pattern definition
	 */
	public SymbolPatternMapEntry(String patternId, SymbolPattern pattern) {
		this.patternId = patternId;
		this.pattern = pattern;
	}
	
	/**
	 * @return the patternId
	 */
	public String getPatternId() {
		return patternId;
	}
	/**
	 * @param patternId the patternId to set
	 */
	public void setPatternId(String patternId) {
		this.patternId = patternId;
	}
	/**
	 * @return the pattern
	 */
	public SymbolPattern getPattern() {
		return pattern;
	}
	/**
	 * @param pattern the pattern to set
	 */
	public void setPattern(SymbolPattern pattern) {
		this.pattern = pattern;
	}
	
	
}
