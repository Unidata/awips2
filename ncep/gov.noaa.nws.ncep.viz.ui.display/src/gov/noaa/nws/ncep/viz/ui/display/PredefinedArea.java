/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package gov.noaa.nws.ncep.viz.ui.display;


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
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class PredefinedArea implements ISerializableObject, IGridGeometryProvider {

	public static enum AreaSource {
		PREDEFINED_AREA,   // saved as a PredefinedArea 
		RESOURCE_DEFINED,  // from a satellite or other resource that can specify an area
		DISPLAY_AREA       // the current area of a renderable display
	}
	
	@XmlAttribute
	protected String areaName = "";
	
	// the 'code' for areas converted from the GEMPAK geog.tbl
	// NOT IMPLEMENTED
	@XmlAttribute
	protected String areaAbbrev = "";
	
	@XmlElement
	protected AreaSource areaSource = AreaSource.PREDEFINED_AREA;
	
    /** Center point of the map */
	@XmlAttribute
    protected double[] mapCenter={0.0,0.0};

    // a double from 0 to 1.0 or 'FitToScreen' or 'SizeOfImage'
    //
	@XmlElement
    protected String zoomLevel = "1.0";

    /** The spatial grid for the descriptor */
    private GeneralGridGeometry gridGeometry;
    
    // Metadata. is the area a state, country.... (currently not used)
    @XmlElement
    protected String description="N/A";
    
    // if true this is made available to the user as a display area of an RBD.
    @XmlElement
    protected Boolean availableAsDisplayArea=true;
    
    public PredefinedArea( AreaSource type, 
    		String n, GeneralGridGeometry g, double[] c, String z ) {
    	areaSource = type;
    	areaName = n;
    	gridGeometry = g;    
    	mapCenter = c;
    	zoomLevel = z;
	}

    @XmlElement
    @XmlJavaTypeAdapter(value = GridGeometryAdapter.class)
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
	
    public AreaSource getAreaSource() {
		return areaSource;
	}

	public void setAreaSource(AreaSource areaType) {
		this.areaSource = areaType;
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
	public String getProviderName() {
		return getAreaName();
	}
	
    public String getAreaAbbrev() {
		return areaAbbrev;
	}

	public void setAreaAbbrev(String areaAbbrev) {
		this.areaAbbrev = areaAbbrev;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Boolean getAvailableAsDisplayArea() {
		return availableAsDisplayArea;
	}

	public void setAvailableAsDisplayArea(Boolean availableAsDisplayArea) {
		this.availableAsDisplayArea = availableAsDisplayArea;
	}	
	
	public static boolean areAreasEqual( PredefinedArea p1, PredefinedArea p2 ) {
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
