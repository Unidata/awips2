package gov.noaa.nws.ncep.viz.ui.display;

import gov.noaa.nws.ncep.viz.common.EditorManager;
import gov.noaa.nws.ncep.viz.ui.display.IGridGeometryProvider.ZoomLevelStrings;
import gov.noaa.nws.ncep.viz.ui.display.PredefinedArea.AreaSource;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.maps.display.MapRenderableDisplay;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.ResourceList;

/**
 * MapRenderableDisplay for NatlCntrs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/19/09                ghull        Initial creation
 * 01/28/10                ghull        Add predefinedAreaName
 * 04/01/10      238,239   archana      Altered the overloaded 
 *                                      constructor to accept the 
 *                                      NCMapDescriptor as its input
 *                                      parameter.  
 *  02/10/2011              Chin Chen   handle multiple editor copies dispose issue    
 *  03/07/2011   migration  ghull       call customizeResourceList
 *  11/18/2012   #630       ghull       construct from areaProvider
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "NC-MapRenderableDisplay")
@XmlRootElement
public class NCMapRenderableDisplay extends MapRenderableDisplay implements
        /*IGridGeometryProvider,*/ ISerializableObject {

    public static final GenericResourceData legendRscData = new GenericResourceData(
            NCLegendResource.class);

    public static final GenericResourceData selectedRscData = new GenericResourceData(
            NcSelectedPaneResource.class);

    @XmlElement
    private PaneID paneId;
    
    private String paneName; // the rbd/displayName + the paneId if multipane

    // the initial area that the display is set to. This is used for the unzoom.
    // after the display is loaded the user may pan/zoom in which case the current
    // area(gridGeometry,zoom,mapcenter) will be different than the initial area.
    //
    @XmlElement
    private PredefinedArea initialArea;

    public NCMapRenderableDisplay() {
        super();
    }

    // set the initial gridGeometry, mapCenter and zoomLevel from the areaProvider
    // 
    public NCMapRenderableDisplay( PaneID pid, 
    		PredefinedArea geomPrvdr ) throws VizException {
    	super();
    	setPaneId( pid );
    	
		setDescriptor( new NCMapDescriptor() );

		setInitialArea( geomPrvdr );
    }

    public NCMapRenderableDisplay( PaneID pid, NCMapDescriptor desc) {
        super(desc);
        this.setPaneId( pid );
//        setInitialArea( PredefinedAreasMngr )
    }

    public void setInitialArea( PredefinedArea ia ) {
    	initialArea = ia;
    	try {
			setPredefinedArea( initialArea );
			
			if( initialArea.getMapCenter() == null ) {
				initialArea.setMapCenter( getMapCenter() );
			}
			
//			if( initialArea.getAreaSource() == AreaSource.RESOURCE_DEFINED &&
//				initialArea.getZoomLevel() == -1.0 ) {
//				
//				IGridGeometryProvider areaProv = findAreaProviderResource( initialArea.getProviderName() );
//				double zlvl = 1.0;
//				
//				if( areaProv == null ) {
//			    	System.out.println(  "Can't find resource, " + initialArea.getProviderName() +", to zoom to???");
//				}
//				else {
//					//zlvl = 
//				}
//				initialArea.setZoomLevel( zlvl );
//				
//	            ((NCMapDescriptor)getDescriptor()).setSuspendZoom( true );
//			}

		} catch (VizException e) {
			System.out.println("Error setting initial area of renderable display:"+e.getMessage() );
		}
    }

    public IGridGeometryProvider findAreaProviderResource( String areaProviderName ) {
    	    	
    	ResourceList rList = getDescriptor().getResourceList();
    	IGridGeometryProvider zoomRsc = null;

    	for( ResourcePair rp : rList ) {
    		if( rp.getResourceData() instanceof IGridGeometryProvider ) {
    			
    			if( areaProviderName.equals( ((IGridGeometryProvider)rp.getResourceData()).getProviderName() ) ) {    				 
    				return (IGridGeometryProvider)rp.getResourceData();
    			}
    		}
    	}

    	return null;
    }
    
    // set the current area to the given predefined area
    //
    public void setPredefinedArea( PredefinedArea pArea ) throws VizException {

    	// can throw exception 
    	getDescriptor().setGridGeometry( pArea.getGridGeometry() );
 
    	// if a map center is given set it, otherwise compute from 
    	// the gridgeometry
    	//
    	if( pArea.getMapCenter() != null ) {
    	   	setMapCenter( pArea.getMapCenter() );   		
    	}
    	else {
    		int xdimArea = getDescriptor().getGridGeometry().getGridRange().getSpan(0);
    		int ydimArea = getDescriptor().getGridGeometry().getGridRange().getSpan(1);
    		
    		setMapCenter( getDescriptor().pixelToWorld(  new double[] { xdimArea/2, ydimArea/2, 0. }) );
    	}
 
    	// set the zoom level. if this is a sentinal for a resource-defined 
    	// zoom level then compute the double value and set it 
    	// 
    	String zlStr = pArea.getZoomLevel();
    	
    	try {
    		double zl = Double.parseDouble( zlStr );
    		setZoomLevel( zl );    		
    	}
    	catch ( NumberFormatException nfe ) {
    		
    		if( zlStr.equals( ZoomLevelStrings.FitToScreen.toString() ) ) {
        		setZoomLevel( 1.0 );
    		}
    		else if( zlStr.equals( ZoomLevelStrings.SizeOfImage.toString() ) ) {
    			
    			if( getContainer() != null ) {
    				setExtent( getView().getExtent() );
    				//calcPixelExtent( getBounds() );
    			}
    			else {
    				setZoomLevel( 1.0 ); // will be set later
    			}
    		}
    		else { // This shouldn't happen
    			System.out.println("Unrecognized Zoom Level, "+zlStr+", in pArea.");
    			setZoomLevel( 1.0 );
    		}
    	}
    }

    @Override
    public void dispose() {

// int editorInstanceNum = EditorManager.getNumOfEditorInstance(editorNum);
        
        // System.out.println("NCMapRenderableDisplay "+
        // this.getDisplayName()+" disposed  editor num " + editorNum
        // +" instance "+ editorInstanceNum);
        if (this.descriptor != null ) {// && editorInstanceNum <= 1) {
            descriptor.getResourceList().clear();
            this.descriptor.getResourceList().removePostAddListener(
                    this.listener);
            this.descriptor.getResourceList().removePostRemoveListener(
                    this.listener);
        }
    }

    // create a PredefinedArea using the current gridGeometry/center/zoom.
    // 
    public PredefinedArea getCurrentArea() {
        // 
    	PredefinedArea curArea = new PredefinedArea( AreaSource.DISPLAY_AREA, 
    			getPaneName(), getDescriptor().getGridGeometry(), getMapCenter(), 
    			Double.toString( getZoomLevel() ) );
    	
    	return curArea;
    }
    
    // @Override
//	public GeneralGridGeometry getGridGeometry() {
//		return getDescriptor().getGridGeometry();
    // }

    public double[] getMapCenter() {
        return this.mapCenter;
    }

    public double getZoomLevel() {
        return zoomLevel;
    }

    public void setPaneName( String p ) {
    	paneName = p;
    }

    public String getPaneName() {
//    	if( getContainer() != null ) {
//    		return ((NCMapEditor)getContainer()).getDisplayName();
//    	}
    	return (paneName == null ? getPaneId().toString() : paneName); // shouldn't be null
    }

    public PaneID getPaneId() {
    	if( paneId == null ) {
    		paneId = new PaneID();
    	}
    	return paneId;
    }

    public void setPaneId(PaneID pid) {
    	paneId = pid;
	}
    
    // TODO? if null then set to the descriptors gridGeom??
    public PredefinedArea getInitialArea() {
    	if( initialArea == null ) {
    		return getCurrentArea();
    	}
    	return initialArea;
    }

    public NCMapDescriptor getDescriptor() {
        if (super.getDescriptor() instanceof NCMapDescriptor) {
            return (NCMapDescriptor) super.getDescriptor();
        }
        return null;
    }

    @Override
    public void setExtent(IExtent pe) {
    	super.setExtent(pe);
    }

//    public void setAreaProviderName(String areaName) {
//        this.areaProviderName = areaName;
//        
//        areaProvider = null;        
//    }

    // create a new PredefinedArea based on the current values for the
    // renderable display's geometry, center and zoomlevel. Use the name of the
    // display as the area name. 
    //  Note that the current area may be different than areaProvider used to set the
    // initial area of the display.
    // 
//    public IGridGeometryProvider createArea() {
//    	PredefinedArea pArea = new PredefinedArea();
//    	
//    	// ??? Should we still call this the original area name if the user has panned/zoomed to change it?
//    	// 
//    	// TODO: get the original area and check if it has changed and if so add a "*" to the name.
//    	
//    	pArea.setAreaName( getPaneName() );// areaProviderName );
//    	
//    	pArea.setMapCenter( getMapCenter() );
//    	pArea.setZoomLevel( zoomLevel );
//    	pArea.setGridGeometry( getDescriptor().getGridGeometry() );
//    	pArea.setDescription( "From Display "+getPaneName() );
//    	
//    	return pArea;
//    }
    
    @Override
    protected void customizeResourceList(ResourceList resourceList) {
        // resourceList. // check if already in the list???
        resourceList.add(ResourcePair
                .constructSystemResourcePair(legendRscData));
        resourceList.add(ResourcePair
                .constructSystemResourcePair(selectedRscData));
    }

}
