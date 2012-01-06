/*
 * gov.noaa.nws.ncep.viz.rsc.mosaic.rsc.RadarResourceData
 * 
 * 03-04-2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */


package gov.noaa.nws.ncep.viz.rsc.mosaic.rsc;

import java.io.File;

import gov.noaa.nws.ncep.viz.localization.impl.LocalizationManager;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationResourcePathConstants;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.rsc.mosaic.util.LegendNameGenerator;
import gov.noaa.nws.ncep.viz.ui.display.ColorBarFromColormap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.util.RadarInfoDict;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.radar.RadarTimeRecord;

/**
 * Provide Radar raster rendering support 
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer     Description
 *  ------------ ----------  -----------  --------------------------
 *  02/08/11	    	 	  Greg Hull    Created shell. No Display. Just for the Resource Manager
 *  03/04/11				  G. Zhang	   Modified for Local Radar
 *                                         
 * </pre>
 * 
 * @author mli
 * @version 1
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-RadarResourceData")
public class RadarResourceData extends AbstractNatlCntrsRequestableResourceData {
	
	private RadarRadialResource rrr;// for DEG only: 2011-03-14
	
	//-------------------------------------------from raytheon's	
    @XmlAttribute
    protected String pointID = "";

    // This might be better as an enumeration, currently "CZ-Pg" triggers
    // Composite Reflectivity to display as graphics rather than image and
    // "SRM" causes Velocity to do SRM stuff
    @XmlAttribute
    protected String mode = "";

    // Will only display the most recently ingested tilt for any time
    @XmlAttribute
    protected boolean latest = false;

    @XmlAttribute
    protected boolean rangeRings = true;
//---------------------------------------------end of raytheon's    
 
	@XmlElement
    private String colorMapName;

	@XmlElement
    private ColorBarFromColormap colorBar;

	// Both the productCode and the productName should be given in the metadataMap
	// but only one is really needed. 
	// TODO write code to make only one of these necessary and set productCode from
	// the productName if not set.
    private Integer productCode=0;

    private String productName;
    
    private static RadarInfoDict infoDict = null;
    
//    private static MosaicInfoDict infoDict = null;
    
    // The legend is set from MosaicInfoDict based on 
    // the product code which is set in the metadataMap.
    private String legendString = null; 
    
//	private static void loadRadarInfo() {
//    	String radarDirname = LocalizationManager.getInstance().getLocalizationFileDirectoryName(LocalizationResourcePathConstants.RADAR_RESOURCES_DIR); 
//    	infoDict = MosaicInfoDict.getInstance(radarDirname);
//    }

    public RadarResourceData() {
        this.nameGenerator = new AbstractNameGenerator() {

        	// This method is not called since MosaicResource is overriding
        	// getName()
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
            
                	legendString = "LOCAL radar-testing";
                	
                	if( infoDict == null ) {
                		loadRadarInfo();
                    	if( infoDict == null ) {	
                    		legendString = new String( "LOCAL radar-testing"+//"Radar Mosaic: Product Code "+
                    				Integer.toString( getProductCode()) );
                    		return legendString;
                    	}
                	}
                	
                	return LegendNameGenerator.generateName(rrr, ""+getProductCode() );  

            }

        };
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
    	rrr = new RadarRadialResource(this, loadProperties);
    	return rrr;
    	
    }

    public Integer getProductCode() {
    	if( productCode == 0 && 
    		metadataMap.containsKey("productCode") ) {
    		RequestConstraint reqCon = metadataMap.get("productCode");
    		productCode = Integer.parseInt( reqCon.getConstraintValue() ); 
    	}
		return productCode;
	}

	public void setProductCode(Integer productCode) {
		this.productCode = productCode;
	}

    @Override
    public boolean equals(Object obj) {
        // TODO Auto-generated method stub
        if (!super.equals(obj)) {
            return false;
        }

        if (obj instanceof RadarResourceData == false) {
            return false;
        }

        RadarResourceData other = (RadarResourceData) obj;
        
        if (this.getProductCode() != other.getProductCode() ) {
            return false;
        } 
        
        return true;
    }

	public String getColorMapName() {
		return colorMapName;
	}

	public void setColorMapName(String colorMapName) {
		this.colorMapName = colorMapName;
	}
	
	
    public ColorBarFromColormap getColorBar() {
		return colorBar;
	}

	public void setColorBar(ColorBarFromColormap cBar) {
		this.colorBar = cBar;
	}	
	
	
	private static void loadRadarInfo() {
    	/*
    	 * The directory is the one under base level of our localization only at this time. 
    	 */
		/*
		 * Start of M. Gao's change
		 */
//    	String radarDirname = LocalizationManager.getInstance().getLocalizationFileDirectoryName(LocalizationResourcePathConstants.RADAR_RESOURCES_DIR,
//    			LocalizationConstants.LOCALIZATION_BASE_LEVEL); 
    	String radarDirname = LocalizationManager.getInstance().getLocalizationFileDirectoryName(LocalizationResourcePathConstants.RADAR_RESOURCES_DIR); 
		/*
		 * End of M. Gao's change
		 */
    	infoDict = RadarInfoDict.getInstance(radarDirname);
    }
}
