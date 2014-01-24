package gov.noaa.nws.ncep.viz.common.area;



import gov.noaa.nws.ncep.viz.common.area.AreaName.AreaSource;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.GridGeometryAdapter;

/**
 * This is used as the initial area for a MapRenderableDisplay and is 
 * also serialized to files under localization control.
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    02/20/10       #226       ghull       added Pane layout info to Bundle class.
 *    11/18/12       #630       ghull       changed to not wrap NcRenderableDisplay
 *    11/28/12       #630       ghull       create for Resource and Display defined areas
 *    02/22/13       #972       ghull       ncDisplayType
 *    04/16/13       #863       ghull       moved from display to common project       
 *    11/21/13       #1066      ghull       NcGridGeometryAdapter to handle Native CRSs
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class PredefinedArea implements ISerializableObject, IGridGeometryProvider {

	@XmlElement
	protected NcDisplayType ncDisplayType=NcDisplayType.NMAP_DISPLAY;
	
	@XmlAttribute
	protected String areaName = "";
		
	// the initial source used to create this predefined area. 
	@XmlElement
	protected String areaSource = AreaSource.PREDEFINED_AREA.toString();
	
    /** Center point of the map */
	@XmlAttribute
    protected double[] mapCenter={0.0,0.0};

    // a double from 0 to 1.0 or 'FitToScreen' or 'SizeOfImage'
    //
    @XmlElement
    protected String zoomLevel = "1.0";

    // The spatial grid for the descriptor 
    private GeneralGridGeometry gridGeometry;
    
    // Metadata. is the area a state, country.... (currently not used)
    @XmlElement
    protected String description="N/A";
        
    public PredefinedArea( AreaSource type, 
    		String n, GeneralGridGeometry g, double[] c, String z, NcDisplayType dt ) {
    	areaSource = type.toString();
    	areaName = n;
    	gridGeometry = g;    
    	mapCenter = c;
    	zoomLevel = z;
    	ncDisplayType = dt;
    }
    
    @XmlElement
    @XmlJavaTypeAdapter(value = NcGridGeometryAdapter.class)
    public GeneralGridGeometry getGridGeometry() {
        return gridGeometry;
    }
    
    public void setGridGeometry( GeneralGridGeometry geom ) {
    	gridGeometry = geom;
    }

	public String getAreaName() {
		return areaName;
	}

	public void setAreaName(String predefinedAreaName) {
		areaName = predefinedAreaName;
	}
	
    public String getAreaSource() {
		return AreaSource.getAreaSource( areaSource ).toString();
	}

	public void setAreaSource( String srcStr) {
		this.areaSource = AreaSource.createAreaSource( srcStr ).toString();
	}

	@Override
	public double[] getMapCenter() {
		return mapCenter;
	}

	public void setMapCenter(double[] mapCenter) {
		this.mapCenter = mapCenter;
	}
	
	@Override
	public String getZoomLevel() {
		return zoomLevel;
	}

	@Override
	public void setZoomLevel(String zoomLevel) {
		this.zoomLevel = zoomLevel;
	}

    public PredefinedArea() {
    }

    @Override
    public AreaSource getSource() {
    	return AreaSource.getAreaSource( areaSource );
    }
    
	@Override
	public String getProviderName() {
		return getAreaName();
	}
	
	public NcDisplayType getNcDisplayType() {
		return ncDisplayType;
	}

	public void setNcDisplayType(NcDisplayType ncDisplayType) {
		this.ncDisplayType = ncDisplayType;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

//	public Boolean getAvailableAsDisplayArea() {
//		return availableAsDisplayArea;
//	}
//
//	public void setAvailableAsDisplayArea(Boolean availableAsDisplayArea) {
//		this.availableAsDisplayArea = availableAsDisplayArea;
//	}	
	
	public static boolean areAreasEqual( PredefinedArea p1, PredefinedArea p2 ) {
		if( p1 == null || p2 == null ) {
			return false;
		}
		
		String p1zlStr = p1.getZoomLevel();
		String p2zlStr = p2.getZoomLevel();
		double p1zlvl, p2zlvl;

		try {
			if( p1zlStr.equals( IGridGeometryProvider.ZoomLevelStrings.FitToScreen.toString() ) ) {
				p1zlvl = -1.0;
			}
			else if( p1zlStr.equals( IGridGeometryProvider.ZoomLevelStrings.SizeOfImage.toString() ) ) {
				p1zlvl = -2.0;
			}
			else {
				p1zlvl = Double.parseDouble( p1zlStr );
			}

			if( p2zlStr.equals( IGridGeometryProvider.ZoomLevelStrings.FitToScreen.toString() ) ) {
				p2zlvl = -1.0;
			}
			else if( p2zlStr.equals( IGridGeometryProvider.ZoomLevelStrings.SizeOfImage.toString() ) ) {
				p2zlvl = -2.0;
			}
			else {
				p2zlvl = Double.parseDouble( p2zlStr );
			}
		} 
		catch ( NumberFormatException nfe ) {
			System.out.println("bad zoom level string???");
			return false;
		}
		
		if( Math.abs( p1zlvl - p2zlvl ) > .00001 ) {
			return false;
		}
		
		if( p1.getMapCenter().length != p2.getMapCenter().length ) {
			return false;
		}
		
		for( int i=0 ; i<p1.getMapCenter().length ; i++ ) {
			if( Math.abs( p1.getMapCenter()[i]-p2.getMapCenter()[i] ) > .00001 ) {
				return false;
			}			
		}
		
		return p1.getGridGeometry().equals( p2.getGridGeometry() );
	}
}
