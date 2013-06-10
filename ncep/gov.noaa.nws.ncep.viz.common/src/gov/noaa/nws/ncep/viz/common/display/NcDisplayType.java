package gov.noaa.nws.ncep.viz.common.display;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;


/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 *  02/10/13      #972        Greg Hull   Created
 *
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
// an RBD must be one of these display types and all resources in the RBD must be compatible 
// with the display type. 
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public enum NcDisplayType {	
	// could make this a class and populate from an extension point. 
	
	NMAP_DISPLAY( "NC-MAP",
			// the NcPaneManager knows the display type handles differences appropriately 
			"gov.noaa.nws.ncep.viz.ui.display.NCPaneManager", 
			"BasicWX_US",
			"OVERLAY/GeoPolitical/default", 
			true),
	NTRANS_DISPLAY( "NTRANS",
			"gov.noaa.nws.ncep.viz.ui.display.NCPaneManager",
			"DefaultCanvas",
			"", 
			true),
	SOLAR_DISPLAY( "SOLAR",
			"gov.noaa.nws.ncep.viz.ui.display.NCPaneManager",
			"DefaultCanvas",
			"", 
			true),
			
// CURRENTLY NOT USED,
	NSHARP_DISPLAY( "NSHARP",
			"gov.noaa.nws.ncep.ui.nsharp.display.NcNsharpPaneManager",
			"BasicWX_US",
			"OVERLAY/GeoPolitical/default",
			false );
//	NC_TIME_SERIES_DISPLAY,
//	NC_CROSS_SECTION_DISPLAY;
	

	private String dispType;
	private String paneManager;
	private String defaultArea; // ie the gridGeom/CRS, zoomlevel, mapcenter.
	private String baseResource; // if applicable, null if not
	private Boolean isSavable;  // can this be saved to an rbd.
	
	private NcDisplayType( String dtstr, String pMngr, String area, String baseOvrly, Boolean canSave ) {
		dispType = dtstr;
		paneManager = pMngr;
		defaultArea = area;
		baseResource = baseOvrly;
		isSavable = canSave; 
	}
	
	public String getName() {
		return dispType;
	}
	
	public static NcDisplayType getDisplayType( String dts ) {
		for( NcDisplayType dt : NcDisplayType.values() ) {
			if( dts.equalsIgnoreCase( dt.getName() ) ) {
				return dt;
			}
		}
		return null; 
	}
	
	public static List<NcDisplayType> getRbdSavableDisplayTypes() {
		List<NcDisplayType> dtl = new ArrayList<NcDisplayType>();
		for( NcDisplayType dt : values() ) {
			if( dt.isSavable ) {
				dtl.add( dt );
			}
		}
		return dtl;
	}
		
    public String getPaneManager() {
		return paneManager;
	}

	public void setPaneManager(String paneManager) {
		this.paneManager = paneManager;
	}

	public String getDefaultMap() {
        return defaultArea;
    }

    public String getBaseResource() {
        return baseResource;
    }

	public Boolean getIsRbdSavable() {
		return isSavable;
	}
}
