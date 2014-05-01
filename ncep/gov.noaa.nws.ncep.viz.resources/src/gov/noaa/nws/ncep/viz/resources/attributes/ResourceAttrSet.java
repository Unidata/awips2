/**
 * 
 */
package gov.noaa.nws.ncep.viz.resources.attributes;

import java.util.HashMap;
import java.util.Set;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;

/**
 * <pre>
 *
 *   This class stores the attributes for a Natl Cntrs resource. These may be retrieved from a given 
 *   attribute set name (ex. 'default') or from the resource itself. In the latter case, BeanUtils methods 
 *   are used so the attribute name must follow Java Beans conventions and correspond to methods in the resource.
 *   
 *   TODO : It would be a good idea to add to the nc-resources extention point to include a list of the expected attributes for
 *   each resource. Then we could ensure that the ResourceAttrSet would have an entry for each attribute even if it is not in the prm file.
 *   
 *   Currently this handles both parameters (values which are substituted from the .prm files into the bndl.xml files
 *   but which are not considered attributes by the resource and which are not edited. The parameters are stored here
 *   in rscAttrsAsStrings. 
 *   TODO : It would be a good idea to separated this out at a later date.
 *   
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 29 May 2009     #115      Greg Hull    Initial Creation.
 * 05 Jan 2010               Greg Hull    added NC-ResourceAttribute ext point
 * 17 Nov 2010     307       Greg Hull    implemented equals()
 * 
 * </pre>
 *
 * @author ghull
 * @version 1
 */
public class ResourceAttrSet {

	public static class RscAttrValue {
		public String attrName = null;
		private Object attrValue = null;
		private Object dfltValue = null; // not implemented
		private Class<?> clazz = null;
	
		RscAttrValue( String n, Object v ) {
			attrName = n;    // field name in the resource
			attrValue = v;
			clazz = attrValue.getClass();
		}
		public Object getAttrValue() {
			return attrValue;
		}
		public void setAttrValue( Object v ) {
			attrValue = v;
		}
		public Class<?> getAttrClass() {
			return clazz;
		}
	}
	
	// To start with this is the name of the .prm file but this can be changed to lookup values from 
	// some other location
	String attrSetName = null;
	
	private HashMap<String,RscAttrValue> rscAttrValues = null;

	public ResourceAttrSet( String name/*, INatlCntrsResourceData ncRscData */) {
		rscAttrValues = new HashMap< String, RscAttrValue >();
		attrSetName = name;
	}
	
	// clone the ResourceAttrSet
	public ResourceAttrSet( ResourceAttrSet rscAttrSet ) {
		attrSetName = rscAttrSet.getRscAttrSetName();
		rscAttrValues = new HashMap< String, RscAttrValue >();
		
		for( RscAttrValue attrVal : rscAttrSet.rscAttrValues.values() ) {
			rscAttrValues.put( new String( attrVal.attrName ), 
					new RscAttrValue( attrVal.attrName, attrVal.attrValue ) );
		}
	}

	public Set<String> getAttrNames() {
		return rscAttrValues.keySet();
	}
	
	
	public void setAttrValue( String attrName, Object attrValue ) {
		
		RscAttrValue rscAttr = rscAttrValues.get( attrName ); 
		
		if( rscAttr == null ) {
			rscAttrValues.put( attrName, new RscAttrValue(attrName, attrValue) );
		}
		else {
			rscAttr.attrValue = attrValue;
		}		
	}

	public String getRscAttrSetName() {
		return attrSetName;
	}
	
	public boolean hasAttrName( String attrName ) {
		return rscAttrValues.containsKey( attrName );
	}
	
	public RscAttrValue getRscAttr( String name ) {
		return rscAttrValues.get( name );
	}
	
	// TODO: Add support for the ColorBar, PlotModel attributes (and ArrowStyle and other enums) 
	// This was implemented mainly as a convienience for the equals() for the 
	// AbstractNatlCntrsResourceData
    @Override
    public boolean equals(Object obj) {
    	// first the sanity checks
    	if( !(obj instanceof ResourceAttrSet) ) {
    		return false;
    	}
    	
    	ResourceAttrSet other = (ResourceAttrSet) obj;
    	HashMap<String,RscAttrValue> otherRscAttrValues = other.rscAttrValues;

    	if( this.rscAttrValues.size() != otherRscAttrValues.size() ) {
    		return false;
    	}
    	
    	for( String key : this.rscAttrValues.keySet() ) {
    		if( !otherRscAttrValues.containsKey( key ) ) {
    			return false;
    		}
    		
    		Object thisAttrVal  = this.rscAttrValues.get( key ).attrValue;
    		Object otherAttrVal = otherRscAttrValues.get( key ).attrValue;
    		
    		if( !thisAttrVal.getClass().getName().equals( 
    			                   otherAttrVal.getClass().getName() ) ) {
    			return false;
    		}
    		
    		else if( thisAttrVal.getClass() == Boolean.class ) {
    			if( !((Boolean)thisAttrVal).equals( (Boolean)otherAttrVal )) {
    				return false;
    			}
    		}
    		else if( thisAttrVal.getClass() == String.class ) {
    			if( !((String)thisAttrVal).equals( (String)otherAttrVal ) ) {
    				return false;
    			}
    		}
    		else if( thisAttrVal.getClass() == Double.class ) {
    			if( !((Double)thisAttrVal).equals( (Double)otherAttrVal )) {
    				return false;
    			}
    		}
    		else if( thisAttrVal.getClass() == Float.class ) {
    			if( !((Float)thisAttrVal).equals( (Float)otherAttrVal )) {
    				return false;
    			}
    		}
    		else if( thisAttrVal.getClass() == Long.class ) {
    			if( !((Long)thisAttrVal).equals( (Long)otherAttrVal )) {
    				return false;
    			}
    		}
    		else if( thisAttrVal.getClass() == Integer.class ) {
    			if( !((Integer)thisAttrVal).equals( (Integer)otherAttrVal )) {
    				return false;
    			}
    		}
    		else if( thisAttrVal.getClass() == Short.class ) {
    			if( !((Short)thisAttrVal).equals( (Short)otherAttrVal )) {
    				return false;
    			}
    		}
    		else if( thisAttrVal.getClass() == RGB.class ) {
    			if( !((RGB)thisAttrVal).toString().equals(
    					   ((RGB)otherAttrVal).toString()) ) {
    				return false;
    			}
    		}
    		else if( thisAttrVal.getClass() == LineStyle.class ) {
    			if( !((LineStyle)thisAttrVal).equals( (LineStyle)otherAttrVal )) {
    				return false;
    			}
    		}
    		// TODO : add support for PlotModel, ColorBar and  
    		// ArrowStyle, MarkerTextSize, MarkerState, MarkerType, PlotModel ColorBar
//    		else if( thisAttrVal.getClass() == .class ) {
//    			if( (LineStyle)thisAttrVal != (LineStyle)otherAttrVal ) {
//    				return false;
//    			}
//    		}
    	}
    	
    	return true;
    }
}