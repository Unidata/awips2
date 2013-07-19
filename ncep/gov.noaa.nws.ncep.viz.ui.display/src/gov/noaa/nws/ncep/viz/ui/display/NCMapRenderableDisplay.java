package gov.noaa.nws.ncep.viz.ui.display;

import gov.noaa.nws.ncep.viz.common.area.AreaName.AreaSource;
import gov.noaa.nws.ncep.viz.common.area.IAreaProviderCapable;
import gov.noaa.nws.ncep.viz.common.area.IGridGeometryProvider;
import gov.noaa.nws.ncep.viz.common.area.NcAreaProviderMngr;
import gov.noaa.nws.ncep.viz.common.area.PredefinedArea;
import gov.noaa.nws.ncep.viz.common.area.PredefinedAreaFactory;
import gov.noaa.nws.ncep.viz.common.area.IGridGeometryProvider.ZoomLevelStrings;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsPaneManager;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsRenderableDisplay;
import gov.noaa.nws.ncep.viz.common.display.INcPaneID;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayName.NcPaneName;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.maps.display.MapRenderableDisplay;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceList.AddListener;
import com.raytheon.viz.ui.editor.AbstractEditor;

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
 *  05/19/2013   #862       ghull       add paneName, implement IAreaProviderCapable 
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
							AddListener, INatlCntrsRenderableDisplay, 
							IAreaProviderCapable, ISerializableObject {

    public static final GenericResourceData legendRscData = new GenericResourceData(
            NCLegendResource.class);

    public static final GenericResourceData selectedRscData = new GenericResourceData(
            NcSelectedPaneResource.class);

    // can't marshal out an interface so store as a string. This should only be accessed via S
//    @XmlElement
//    private String paneIdStr;

    // I wanted this to be an INcPaneID but jaxb can't handle interfaces. Since now and for
    // forseeable future there will only be NcPaneID, just call it an NcPaneID.
    @XmlElement
    private NcPaneID paneId;
    
//    private String paneName; // the rbd/displayName + the paneId if multipane
    
    // either the RBD or the Display's paneManager
    private INatlCntrsPaneManager paneContainer;
    
    // the initial area that the display is set to. This is used for the unzoom.
    // after the display is loaded the user may pan/zoom in which case the current
    // area(gridGeometry,zoom,mapcenter) will be different than the initial area.
    // TODO: can we change this to only save the AreaName (with the areaSource.) ?
    //
    @XmlElement
    private PredefinedArea initialArea;

    public NCMapRenderableDisplay() {
        super();
    }
    
    @Override
    public void setInitialArea( PredefinedArea area) {
    	
    	try {
			initialArea = PredefinedAreaFactory.clonePredefinedArea( area );
		} catch (VizException e1) {
			System.out.println("error cloning PredefinedArea:"+e1.getMessage() );
    		return;
    	}
    	
    	// if this area came from the current area of another display then
    	// change the source to be this display's initial area so that the 
		// 
    	if( initialArea.getSource() == AreaSource.DISPLAY_AREA ) {
    		
    		initialArea.setAreaSource( AreaSource.INITIAL_DISPLAY_AREA.toString() );
    		initialArea.setAreaName( getAreaName() );
    	}
    	
    	try {
			setPredefinedArea( initialArea );
			
			if( initialArea.getMapCenter() == null ) {
				initialArea.setMapCenter( getMapCenter() );
			}
		} 
    	catch (VizException e) {
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

    public double[] getMapCenter() {
        return this.mapCenter;
    }

    public double getZoomLevel() {
        return zoomLevel;
    }

    @Override
    public NcPaneName getPaneName() {
    	if( getPaneManager().getPaneLayout().getNumberOfPanes() == 1 ) {
        	return new NcPaneName( getPaneManager().getDisplayName() );    		
    	}
    	else {
    		return new NcPaneName( getPaneManager().getDisplayName(), getPaneId() );
    }
    }

    @Override
    public INcPaneID getPaneId() {
    	if( paneId == null ) {
    		paneId = new NcPaneID();
    	}
    	return paneId;
    }
    
    @Override
    public void setPaneId(INcPaneID pid) {
    	paneId = (NcPaneID) pid;
	}
    
	// TODO? if null then set to the default. This shouldn't happen except possibly 
    // in the case of out of date RBDs.
    @Override
    public PredefinedArea getInitialArea() {
    	if( initialArea == null ) {
			try {
				initialArea = 
					PredefinedAreaFactory.getDefaultPredefinedAreaForDisplayType( 
							NcDisplayType.NMAP_DISPLAY );
			} catch (VizException e) {
			}

    		//getCurrentArea();
//    		try {
//    			NcDisplayName dispName = dispPane.getPaneName().getDispName();				
//    			AreaSource as = ( dispName.getId() > 0 ? 
//    					AreaSource.DISPLAY_AREA : AreaSource.RBD );
    			// if this display is from an editor then the areaSource is  
//    			if( getPaneManager() instanceof NatlCntrsEditor ) {
//    				
//    				IGridGeometryProvider ia = 
//    					NcAreaProviderMngr.getSourceProviderFactory( AreaSource.DISPLAY_AREA ).
//    						createGeomProvider( getPaneName().toString() );
//    				// sanity check 
//    				if( ia instanceof PredefinedArea ) {
//    					initialArea = (PredefinedArea)ia;
//    				}
//    			}
//    			else { // if from an RBD, then the 
    				// create a PredefinedArea from the geo
//    				initialArea = NcDisplayAreaProviderFactory.
//    					createPredefinedAreaFromRenderableDisplay( this, AreaSource.DISPLAY_INITIAL_AREA );    				
//    			}
				
//			} catch (VizException e) {
//			}    			
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
        resourceList.addPostAddListener( this );
    }
    
    @Override
    public void notifyAdd( ResourcePair rp ) throws VizException {

    	// TODO : any checks on the type of resource here. 
        AbstractNcPaneManager pm = NcEditorUtil.getNcPaneManager( (AbstractEditor)container );        
        if( pm != null ) {
        	pm.setDisplayAvailable( false );
        }        
    }

	@Override
	public AreaSource getSourceProvider() {
		return AreaSource.DISPLAY_AREA;
	}

	@Override
	public String getAreaName() {
		return getPaneName().toString();
	}
 
	@Override
	public void setPaneManager(INatlCntrsPaneManager pm) {
		paneContainer = pm;
	}

	@Override 
	public void setContainer( IDisplayPaneContainer container ) {
		super.setContainer( container );
	
		if( container instanceof AbstractEditor ) {
			INatlCntrsPaneManager pm = NcEditorUtil.getNcPaneManager( (AbstractEditor)container );
			setPaneManager( pm );
		}
	}
	
	@Override
	public INatlCntrsPaneManager getPaneManager() {
		return paneContainer;
	}
}
