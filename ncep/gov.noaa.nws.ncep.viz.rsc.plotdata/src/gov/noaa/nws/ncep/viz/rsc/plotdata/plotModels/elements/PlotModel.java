package gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements;

import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.LocalizationFile;

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
 * 08/21/11     450         G. Hull     add LocalizationFile                 
 * 05/02/12     778         Q. Zhou     Changed symbol size form int to double       
 * 10/18/2012   431         S. Gurung   Added support for ConditionalParameter and ConditionalColorBar       
 *  
 * </pre>
 * 
 * @author mli
 * @version 1
 */

@XmlRootElement(name = "plotModel")
@XmlAccessorType(XmlAccessType.NONE)
public class PlotModel {

    @XmlElement(name = "PlotModelElement", required = true)
    protected List<PlotModelElement> plotModelElement;

    @XmlAttribute
    protected String name;
    
    @XmlAttribute
    protected String plugin;  // the plugin or db table name
    
    @XmlAttribute
    protected String svgTemplate;
    
    // This is only set if created from a saved file as opposed
    // to being an 'edited' attribute of a PlotResource.
    protected LocalizationFile lFile;

	/**
     * Gets the value of the plotModelElement property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the plotModelElement property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getPlotModelElement().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link PlotModelElement }
     * 
     * 
     */
    public List<PlotModelElement> getAllPlotModelElements() {
        if (plotModelElement == null) {
            plotModelElement = new ArrayList<PlotModelElement>();
        }
        return this.plotModelElement;
    }

    public ArrayList<String> getPlotParamNames( boolean includeWindParam ) {
    	ArrayList<String> retList = new ArrayList<String>();
    	for( PlotModelElement pme : getAllPlotModelElements() ) {
    		if( pme.paramName != null && !pme.paramName.isEmpty() ) {
    			if( includeWindParam || !pme.getPosition().equals("WD" ) ) {
        			retList.add( pme.paramName );    				
    			}
    		}
    	}
    	return retList;
    }
    
    public String createLocalizationFilename() {
    		return NcPathConstants.PLOT_MODELS_DIR + File.separator + 
    		        plugin + File.separator + name + ".xml";
    }
    
    public String getName() {
        return name;
    }

    public LocalizationFile getLocalizationFile() {
		return lFile;
	}

	public void setLocalizationFile(LocalizationFile lFile) {
		this.lFile = lFile;
	}

	public void setName(String value) {
        this.name = value;
    }

    public String getPlugin() {
        return plugin;
    }

    public void setPlugin(String value) {
        this.plugin = value;
    }

    public String getSvgTemplate() {
        return svgTemplate;
    }

    public void setSvgTemplate(String value) {
        this.svgTemplate = value;
    }
    
    //
    public PlotModelElement getPlotModelElement( String position ) {
    	for( PlotModelElement e : plotModelElement ) {
    		if( e.getPosition().equalsIgnoreCase( position ) ){
    			return e;
    		}
    	}
    	return null;
    }
    
    public PlotModelElement getSkyCoverageElement() {
    	for( PlotModelElement e : plotModelElement ) {
    		if( e.getPosition().equalsIgnoreCase( "SC" ) ) {
    			return e;
    		}
    	}
    	return null;
    }
    
    public PlotModelElement getWindBarbElement() {
    	for( PlotModelElement e : plotModelElement ) {
    		if( e.getPosition().equalsIgnoreCase( "WD" ) ) {
    			return e;
    		}
    	}
    	return null;
    }

    public boolean removePlotModelElement( PlotModelElement pme ) {
    	for( PlotModelElement e : plotModelElement ) {
    		if( e.getPosition().equalsIgnoreCase( pme.getPosition() ) ){
    			plotModelElement.remove( e );
    			return true;
    		}
    	}
    	return false;
    }
    
    // find this position and either replace, add, or update the element
    //
//    public void updatePlotModelElement( PlotModelElement pme ) {
//    	if( pme == null || pme.getPosition() == null ) {
//    		return;
//    	}
//    	
//    	if( pme.getParamName() == null || pme.getParamName().isEmpty() ) {
//    		removePlotModelElement( pme.getPosition() );
//    	}
//    	else if( getPlotModelElement( pme.getPosition() ) == null ) {
//    		putPlotModelElement( pme );
//    	}
//    }
    
    public void putPlotModelElement( PlotModelElement pme ) {
    	int i=0;
    	for( PlotModelElement e : plotModelElement ) {
    		if( e.getPosition().equalsIgnoreCase( pme.getPosition() ) ){
    			plotModelElement.set( i, pme );
    			return;
    		}
    		i++;
    	}
    	plotModelElement.add( pme );
    }
    
    public PlotModel( ) {
    }

    public PlotModel( PlotModel pm ) {
        plotModelElement = new ArrayList<PlotModelElement>();
    	name = new String( pm.name );
    	plugin = new String( pm.plugin );
    	svgTemplate = new String( pm.svgTemplate );
    	
        for( PlotModelElement pme : pm.getAllPlotModelElements() ) {
        	PlotModelElement newPlotModelElement = new PlotModelElement( );
        	newPlotModelElement.setParamName( pme.getParamName() );
        	newPlotModelElement.setPosition( pme.getPosition() );
//        	newPlotModelElement.setEnable( pme.getEnable() );
        	if( pme.getSymbolSize() != null ) {
        		newPlotModelElement.setSymbolSize( pme.getSymbolSize()); //(new Double( pme.getSymbolSize()) ) );
        	}       	
        	if( pme.getTextFont() != null ) {
        		newPlotModelElement.setTextFont( pme.getTextFont() );
        	}
        	if( pme.getTextSize() != null ) {
        		newPlotModelElement.setTextSize( pme.getTextSize() );
        	}
        	if( pme.getTextStyle() != null ) {
        		newPlotModelElement.setTextStyle( pme.getTextStyle() );
        	}
        	if( pme.getColor() != null ) {
        		Color newColor = new Color();
        		newColor.setRed(pme.getColor().getRed());
        		newColor.setGreen(pme.getColor().getGreen());
        		newColor.setBlue(pme.getColor().getBlue());
        		newPlotModelElement.setColor( newColor );
        	}
        	if( pme.getConditionalParameter() != null ) {
        		newPlotModelElement.setConditionalParameter( pme.getConditionalParameter() );
        	}
        	if( pme.getConditionalColorBar() != null ) {
        		newPlotModelElement.setConditionalColorBar( pme.getConditionalColorBar() );
        	}
        	
        	plotModelElement.add( newPlotModelElement );
    	}    	
        
        lFile = pm.lFile;
    }   
    
    public boolean hasAdvancedSettings() {    	
    	
    	 for( PlotModelElement pme : plotModelElement ) {
    		if (pme.hasAdvancedSettings()) 
    			return true;
    	 }
    	
    	return false;
    }
}
