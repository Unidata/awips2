/*
 * gov.noaa.nws.ncep.viz.rsc.mosaic.rsc.RadarResourceData
 * 
 * 03-04-2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */


package gov.noaa.nws.ncep.viz.rsc.mosaic.rsc;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.TimeZone;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.ui.display.ColorBarFromColormap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.util.RadarInfo;
import com.raytheon.uf.common.dataplugin.radar.util.RadarInfoDict;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

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
 *  07/2011        450        Greg Hull    NcPathManager
 *  11/11/11                  Greg Hull    rm productName, add getRadarName(), use radarInfo.txt to create legend
 *                                         
 * </pre>
 * 
 * @author mli
 * @version 1
 */

@XmlAccessorType(XmlAccessType.NONE)
//@XmlType(name="NC-RadarResourceData")
public class RadarResourceData extends AbstractNatlCntrsRequestableResourceData {
	
	private RadarRadialResource rrr;// for DEG only: 2011-03-14
	
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
 
	@XmlElement
    private String colorMapName;

	@XmlElement
    private ColorBarFromColormap colorBar;
	
	@XmlElement
	private Float alpha;

	@XmlElement
	private Float brightness;
	
	@XmlElement
	private Float contrast;

    public Float getAlpha() {
		return alpha;
	}

	public void setAlpha(Float alpha) {
		this.alpha = alpha;
	}

	public Float getBrightness() {
		return brightness;
	}

	public void setBrightness(Float brightness) {
		this.brightness = brightness;
	}

	public Float getContrast() {
		return contrast;
	}

	public void setContrast(Float contrast) {
		this.contrast = contrast;
	}

	private static RadarInfo rdrInfo = null;
    
    // The legend w/o the time prefix
    private String legendStr = null; 
    
    public RadarResourceData() {
        this.nameGenerator = new AbstractNameGenerator() {

        	// This method is not called since MosaicResource is overriding
        	// getName()
        	@Override
        	public String getName(AbstractVizResource<?, ?> resource) {

        		if( rrr == null || rrr.displayedDate == null ) {
        			return "";
        		}
        		
        		// only create this once and then pre-pend the time to it
        		// the legend is the time+icao+resolution+productname+tilt
        		//
        		if( legendStr == null ) {

        			// get the elevation as a string

        			String tiltStr = " "+rrr.getTrueElevText();

        			if( rdrInfo == null ) {
        				String radarDirname = 
        					NcPathManager.getInstance().getStaticFile(
        							NcPathConstants.RADAR_INFO ).getParent();

        				RadarInfoDict infoDict = RadarInfoDict.getInstance( radarDirname );

        				if( infoDict != null ) {	
        					rdrInfo = infoDict.getInfo( getProductCode() );	
        				}
        			}

        			if( rdrInfo == null ) {
        				legendStr = " "+getRadarName().toUpperCase()+" -product " + getProductCode() + tiltStr;
        			}
        			else {  
        				// else get the resolution and productName from the radarInfo file

        				// This is also in the data. we could get it from there.
        				String resKmStr; // in km from meters.
        				
        				if( rdrInfo.getResolution() % 1000 == 0 ) {
        					resKmStr = " "+ new Integer( rdrInfo.getResolution()/1000)+"km ";
        				}
        				else {
        					resKmStr = " " + new Float( rdrInfo.getResolution()/1000.0)+"km ";
        				}
        				
        				legendStr = " " + getRadarName().toUpperCase() + resKmStr + 
        							   rdrInfo.getName() + tiltStr;
        			}
        		}
            	
            	// TODO : this is the frame time. We will need to change this 
            	// when the code is fixed to be able to display radar as non-dominant resource.
            	//	
            	if( rrr.displayedDate == null ) {
            		String retLegStr = "No Data " + legendStr; 
            		legendStr = null; // no data so tilt is not set so recompute next time.
            		return retLegStr;
            	}
            	else {
            		
            		return NmapCommon.getTimeStringFromDataTime( 
            			rrr.displayedDate, "/") + " " + legendStr;
            	}
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
    	if( metadataMap.containsKey("productCode") ) {
    		RequestConstraint reqCon = metadataMap.get("productCode");
    		return Integer.parseInt( reqCon.getConstraintValue() ); 
    	}
		return -1;
	}

    public String getRadarName() {
    	if( metadataMap.containsKey("icao") ) {
    		RequestConstraint reqCon = metadataMap.get("icao");
    		return reqCon.getConstraintValue(); 
    	}
		return "Unknown ICAO";
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
}
