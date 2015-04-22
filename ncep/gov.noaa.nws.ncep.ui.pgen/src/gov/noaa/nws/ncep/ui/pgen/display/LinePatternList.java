/*
 * LinePatternist
 * 
 * Date created: 01 MAY 2009
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
 * This object contains a list of defined Line Patterns that can be applied to
 * a multi-point line path.  The line patterns are stored as a list of LinePatternMapEntry
 * objects that each contain both the "key" and "value" of a HashMap holding LinePatterns.
 * <P>
 * An object of this class is used by JAXB when marshaling/unmarshaling a list of LinePatterns
 * to/from an XML file.
 * @author sgilbert
 *
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class LinePatternList implements ISerializableObject {

	/**
	 * A list of the available Line Patterns. 
	 */
	@XmlElement(name="patternEntry")
	private ArrayList<LinePatternMapEntry> patternList;
	
	/**
	 * Default constructor.
	 */
	public LinePatternList() {
		
	}
	
	/**
	 * Constructor used with existing pattern map
	 * @param patternMap
	 */
	public LinePatternList(HashMap<String, LinePattern> patternMap) {
		patternList = new ArrayList<LinePatternMapEntry>();
		for ( String key : patternMap.keySet() ) {
			patternList.add( new LinePatternMapEntry( key, patternMap.get(key) ) );
		}
	}

	/**
	 * @return the patternList
	 */
	public ArrayList<LinePatternMapEntry> getPatternList() {
		return patternList;
	}


	
}
