/*
 * SymbolPatternList
 * 
 * Date created: 13 MAY 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import java.util.ArrayList;
import java.util.HashMap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * This object contains a list of defined Symbol Patterns that can be applied to
 * a single-point drawable.  The symbol patterns are stored as a list of SymbolPatternMapEntry
 * objects that each contain both the "key" and "value" of a HashMap holding SymbolPatterns.
 * <P>
 * An object of this class is used by JAXB when marshaling/unmarshaling a list of SymbolPatterns
 * to/from an XML file.
 * @author sgilbert
 *
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class SymbolPatternList implements ISerializableObject {

	/**
	 * A list of the available Line Patterns. 
	 */
	@XmlElement(name="patternEntry")
	private ArrayList<SymbolPatternMapEntry> patternList;
	
	/**
	 * Default constructor.
	 */
	public SymbolPatternList() {
		
	}
	
	/**
	 * Constructor used with existing pattern map
	 * @param patternMap
	 */
	public SymbolPatternList(HashMap<String, SymbolPattern> patternMap) {
		patternList = new ArrayList<SymbolPatternMapEntry>();
		for ( String key : patternMap.keySet() ) {
			patternList.add( new SymbolPatternMapEntry( key, patternMap.get(key) ) );
		}
	}

	/**
	 * Gets the list of symbol patterns
	 * @return the patternList
	 */
	public ArrayList<SymbolPatternMapEntry> getPatternList() {
		return patternList;
	}
	
}
