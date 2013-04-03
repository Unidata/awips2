package gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter;

import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Implementation of a conditional filter
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  04/2012      #615        S. Gurung   Initial Creation.
 *  04/2012      #606        Greg Hull   
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1
 */
@XmlRootElement(name = "conditionalFilter")
@XmlAccessorType(XmlAccessType.NONE)
public class ConditionalFilter extends AbstractConditionalFilter implements ISerializableObject {

    @XmlElements({ @XmlElement(name = "ConditionalFilterElement", type = ConditionalFilterElement.class) })
    List<ConditionalFilterElement> conditionalFilterElements;

    @XmlAttribute
    private String name;
    
    @XmlAttribute
    private String description = null;

    @XmlAttribute
    protected String plugin;  // the plugin or db table name
    
    // This is only set if created from a saved file as opposed
    // to being edited.
    protected LocalizationFile lFile;
    
    /**
     * Constructor used by JiBX
     */
    public ConditionalFilter() {
        this.conditionalFilterElements = new ArrayList<ConditionalFilterElement>();
    }

    public ConditionalFilter( ConditionalFilter cf ) {
        conditionalFilterElements = new ArrayList<ConditionalFilterElement>();
    	name = new String(cf.name );
    	plugin = new String(cf.plugin );
    	description = new String(cf.description);
    	
        for( ConditionalFilterElement cfe : cf.getConditionalFilterElements()) {
        	ConditionalFilterElement newCfe = new ConditionalFilterElement( );
        	newCfe.setParamName( cfe.getParamName() );
        	newCfe.setConstraintType(cfe.getConstraintType());
        	newCfe.setValue(cfe.getValue());
        	conditionalFilterElements.add( newCfe );
    	}    	
        
        //setConditionalFilterMap();
        
        lFile = cf.lFile;
    }   
       
    public ArrayList<String> getPlotParamNames() {
    	ArrayList<String> retList = new ArrayList<String>();
    	for( ConditionalFilterElement cfe : getConditionalFilterElements() ) {
    		if( cfe.getParamName() != null && !cfe.getParamName().isEmpty() ) {
        			retList.add( cfe.getParamName() );   
    		}
    	}
    	return retList;
    }
    
    public String createLocalizationFilename() {
    		return NcPathConstants.CONDITIONAL_FILTERS_DIR + File.separator + 
    							     plugin + File.separator+name + ".xml";
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalFilter.IConditionalFilter#getPlotParamName()
     */
    public String[] getParamNames() {
        int condNum = conditionalFilterElements.size();
        String[] plotParamName = new String[condNum];
        for (int i = 0; i < condNum; ++i) {
           plotParamName[i] = conditionalFilterElements.get(i).getParamName();
        }
        return plotParamName;
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalFilter.IConditionalFilter#getConstraintType()
     */
    public String[] getConstraintTypes() {
        int condNum = conditionalFilterElements.size();
        String[] plotParamName = new String[condNum];
        for (int i = 0; i < condNum; ++i) {
           plotParamName[i] = conditionalFilterElements.get(i).getConstraintType();
        }
        return plotParamName;
    }
   
   /*
    * (non-Javadoc)
    * 
    * @see gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalFilter.IConditionalFilter#getValue()
    */
    public String[] getValues() {
       int condNum = conditionalFilterElements.size();
       String[] plotParamName = new String[condNum];
       for (int i = 0; i < condNum; ++i) {
          plotParamName[i] = conditionalFilterElements.get(i).getValue();
       }
       return plotParamName;
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalFilter.IConditionalFilter#getSize()
     */
    public int getSize() {
        return conditionalFilterElements.size();
    }
   
    /*
     * (non-Javadoc)
     * 
     * @see gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalFilter.IConditionalFilter#getName()
     */
    public String getName() {
        return name;
    }
    
    public String getDescription() {
        return description;
    }

    public String getPlugin() {
        return plugin;
    }

    public void setPlugin(String value) {
        this.plugin = value;
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
    	StringBuffer cfstr = new StringBuffer( getName() );
    	if( !getDescription().isEmpty() ) {
    		cfstr.append( ","+getDescription() );
    	}

    	cfstr.append( " {"+ getFilterAsString() + "}" );
        
    	return cfstr.toString().trim();
    }

    public String getFilterAsString() {
    	StringBuffer cfstr = new StringBuffer();

    	for( ConditionalFilterElement cfe : getConditionalFilterElements() ) {
    		cfstr.append( 
    				(cfstr.length() != 0 ? " AND ":"") + cfe.toString() );
    	}
        return cfstr.toString().trim();
    }

    public void setName(String aName) {
        name = aName;
    }

    public void setDescription(String desc) {
        description = desc;
    }
    
    public LocalizationFile getLocalizationFile() {
		return lFile;
	}

	public void setLocalizationFile(LocalizationFile lFile) {
		this.lFile = lFile;
	}
	
	/**
     * Gets the value of the conditionalFilterElements property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the conditionalFilterElements property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getConditionalFilterElements().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ConditionalFilterElement }
     * 
     * 
     */
    public List<ConditionalFilterElement> getConditionalFilterElements() {
        if (conditionalFilterElements == null) {
        	conditionalFilterElements = new ArrayList<ConditionalFilterElement>();
        }
        return this.conditionalFilterElements;
    }
	
    @Override
    public ConditionalFilter clone() {
        return new ConditionalFilter(this);
    }

	public ConditionalFilterElement getConditionalFilterElement(int index) {
		 if (conditionalFilterElements != null) {
	        	return conditionalFilterElements.get(index);
	     }
		 return null;
	}
	
	 public HashMap<String, RequestConstraint> getConditionalFilterMap() {
			
		HashMap<String, RequestConstraint> condFilterMap = new HashMap<String, RequestConstraint>();
			
		if (conditionalFilterElements != null) {			
			for (ConditionalFilterElement cfe : conditionalFilterElements) {
				
				String paramName = cfe.getParamName();
				String constraintType = cfe.getConstraintType();
				String value = cfe.getValue();
				
				if (constraintType.equals(ConstraintType.NOT_EQUALS.toString())) {
					RequestConstraint req = new RequestConstraint(value, ConstraintType.NOT_EQUALS);
	                condFilterMap.put( paramName, req );
				} 
				if (constraintType.equals(ConstraintType.GREATER_THAN_EQUALS.toString())) {
					RequestConstraint req = new RequestConstraint(value, ConstraintType.GREATER_THAN_EQUALS);
	                condFilterMap.put( paramName, req );
				} 
				if (constraintType.equals(ConstraintType.LESS_THAN_EQUALS.toString())) {
					RequestConstraint req = new RequestConstraint(value, ConstraintType.LESS_THAN_EQUALS);
	                condFilterMap.put( paramName, req );
				} 
				if (constraintType.equals(ConstraintType.EQUALS.toString())) {
					RequestConstraint req = new RequestConstraint(value, ConstraintType.EQUALS);
	                condFilterMap.put( paramName, req );
				} 
				if (constraintType.equals(ConstraintType.GREATER_THAN.toString())) {
					RequestConstraint req = new RequestConstraint(value, ConstraintType.GREATER_THAN);
	                condFilterMap.put( paramName, req );
				} 
				if (constraintType.equals(ConstraintType.LESS_THAN.toString())) {
					RequestConstraint req = new RequestConstraint(value, ConstraintType.LESS_THAN);
	                condFilterMap.put( paramName, req );
				}
				if (constraintType.equals(ConstraintType.IN.toString())) {
					RequestConstraint req = new RequestConstraint(value, ConstraintType.IN);
	                condFilterMap.put( paramName, req );
				}
				if (constraintType.equals(ConstraintType.BETWEEN.toString())) {
					RequestConstraint req = new RequestConstraint(value, ConstraintType.BETWEEN);
	                condFilterMap.put( paramName, req );
				}
				/*
				if (constraintType.equals(ConstraintType.LIKE.toString())) {
					RequestConstraint req = new RequestConstraint(value, ConstraintType.LIKE);
	                condFilterMap.put( paramName, req );
				}
				if (constraintType.equals(ConstraintType.ILIKE.toString())) {
					RequestConstraint req = new RequestConstraint(value, ConstraintType.ILIKE);
	                condFilterMap.put( paramName, req );
				}
				if (constraintType.equals(ConstraintType.ISNULL.toString())) {
					RequestConstraint req = new RequestConstraint(value, ConstraintType.ISNULL);
	                condFilterMap.put( paramName, req );
				}*/				
			}
		}			

		System.out.println(" condFilterMap = " + condFilterMap);	
		return condFilterMap;
	}

}
