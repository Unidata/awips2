package gov.noaa.nws.ncep.viz.rsc.plotdata.parameters;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 *
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/09  		172    	   	M. Li       Initial Creation
 *                           
 * 07/31/11     450         G. Hull     Add plugin and moved methods from NcPathManager       
 * 06/01/12     654         S. Gurung   Added method getSkyCoverageParams()          
 *                       
 * </pre>
 * 
 * @author mli
 * @version 1
 */
@XmlRootElement(name = "PlotParameterDefns")
@XmlAccessorType(XmlAccessType.NONE)
public class PlotParameterDefns implements ISerializableObject {

    @XmlElement
    protected String plugin;

	@XmlElement(name = "PlotParameterDefn")
    protected List<PlotParameterDefn> plotParameterDefn;

    public String getPlugin() {
		return plugin;
	}

	public void setPlugin(String plugin) {
		this.plugin = plugin;
	}


    /**
     * Gets the value of the parameter property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the parameter property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getParameter().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link PlotParameterDefn }
     * 
     * 
     */
    public List<PlotParameterDefn> getParameterDefns() {
        if (plotParameterDefn == null) {
        	plotParameterDefn = new ArrayList<PlotParameterDefn>();
        }
        return this.plotParameterDefn;
    }

    public PlotParameterDefn getPlotParamDefn( String plotParmName ) {
    	if (plotParameterDefn == null || plotParameterDefn.isEmpty()) 
    		return null;

    	for (PlotParameterDefn p : plotParameterDefn){
    		if (p.getPlotParamName().equalsIgnoreCase(plotParmName)) {
    			return p;
    		}
    	}

    	return null;
    }	

	public String[] getAllParameterNames( boolean includeSkyC, boolean includeWndBrb ) {
		List<String> list = new ArrayList<String>();
		
		for(PlotParameterDefn p : plotParameterDefn ){
			
			if( p.getPlotMode() != null && p.getPlotMode().equals( "barb" ) ) {
				if( includeWndBrb ) {
					list.add( p.getPlotParamName() );
				}
			}
			else if( p.getPlotMode() != null && p.getPlotMode().equals( "table" ) &&
				 	 p.getSymbolFont() != null    && p.getSymbolFont().equals("SpecialSymbolFont") ) {					
				if( includeSkyC ) {
					list.add( p.getPlotParamName() );
				}
			}
			else {
				list.add( p.getPlotParamName() );
			}

		}		
		
		return list.toArray(new String[]{});

	}

	public ArrayList<String> getWindBarbParams(){ 
		ArrayList<String> list = new ArrayList<String>();
		
		for(PlotParameterDefn p : plotParameterDefn ){
			if( p.getPlotMode() != null && p.getPlotMode().equals( "barb" ) &&
				p.getPlotParamName() != null ) {
				
				list.add( p.getPlotParamName() );				
			}
		}		
		
		return list;
	}
	
	public ArrayList<String> getSpecialTableParams() {
		ArrayList<String> list = new ArrayList<String>();
		
		for(PlotParameterDefn p : plotParameterDefn ){
			if( p.getPlotMode() != null && p.getPlotMode().equals( "table" ) &&
				p.getSymbolFont() != null    && p.getSymbolFont().equals("SpecialSymbolFont") &&
				p.getPlotParamName() != null ) {
				
				list.add( p.getPlotParamName() );
			}
		}		
		
		return list;
	}

	public ArrayList<String> getSkyCoverageParams() {
		ArrayList<String> list = new ArrayList<String>();
		
		for(PlotParameterDefn p : plotParameterDefn ){
			if( p.getPlotMode() != null && p.getPlotMode().equals( "table" ) &&
				p.getSymbolFont() != null    && p.getSymbolFont().equals("WxSymbolFont") &&
				p.getPlotParamName() != null && p.getPlotParamName().endsWith("C")) {
				
				list.add( p.getPlotParamName() );
			}
		}		
		
		return list;
	}

	// return  a list of all the Defns with the given metParameter.
	public ArrayList<PlotParameterDefn> getPlotParamDefnsForMetParam( String metParam ) {
		ArrayList<PlotParameterDefn> retList = new ArrayList<PlotParameterDefn>();
		for(PlotParameterDefn pd : plotParameterDefn ){
			if( pd.getMetParamName().equals( metParam ) ) {
				retList.add( pd );				
			}
		}		
		return  retList;
	}


    
}
