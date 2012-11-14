package gov.noaa.nws.ncep.viz.tools.predefinedArea;

import gov.noaa.nws.ncep.viz.rsc.satellite.rsc.AbstractSatelliteResource;
import gov.noaa.nws.ncep.viz.rsc.satellite.rsc.GiniSatResource;
import gov.noaa.nws.ncep.viz.rsc.satellite.rsc.McidasSatResource;
import gov.noaa.nws.ncep.viz.tools.panZoom.ZoomUtil;
import gov.noaa.nws.ncep.viz.ui.display.NCDisplayPane;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import java.util.Iterator;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.ui.PlatformUI;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.ResourceList;

/**
 * Searches the current Editor for an AbstractSatelliteResource, and changes the 
 * Coordinate Reference System in the mapDescriptor to the same CRS of the
 * satellite Image. This will mostly be used to display non-remapped satellite
 * images in their native projection.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Sep 09, 2010             gilbert       
 * Jan 18, 2011             ghull         Multi-Pane and geoSync
 *  
 * </pre>
 * 
 * @author sgilbert
 * @version 1
 */
public class SatelliteAreaAction extends AbstractHandler {

    public static final String DISPLAY_MODE = "displayMode";

    public static final String SIZE_OF_IMAGE = "sizeOfImage";

    public SatelliteAreaAction() {
    }

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
         
        String mode = event.getParameter(DISPLAY_MODE);

    	// Is current Editor a Map Editor?
        NCMapEditor editor = NmapUiUtils.getActiveNatlCntrsEditor();
    	if ( editor == null ) 
    		return null;

    	boolean satRscFound = false;
    	boolean geoSyncedPanes = editor.arePanesGeoSynced();

    	// get the panes to reproject.
        NCDisplayPane[] displayPanes = (NCDisplayPane[]) (geoSyncedPanes ? editor
                .getDisplayPanes() : editor.getSelectedPanes());

        AbstractSatelliteResource satResource = null;
        					
        // If the geoSync flag is set then we need to check if there is one or
        // more
        // sat resources in the any of the selected panes. 
    	// 
    	if( geoSyncedPanes ) {
    		//
    		String satAreaName=null;

    		for( IDisplayPane pane : editor.getSelectedPanes() ) {
        		
    			if( satResource == null ) {   
    				AbstractSatelliteResource tmpSatRsc = getSatResource( pane );
    				
                    // if a sat rsc is found, save the area name if we can get
                    // the geometry.
                    // It is possible for the satellite image to not have a
                    // geometry if the
                    // image is outside the current area or if there isn't a
                    // time matched image
                    if (tmpSatRsc != null
                            && tmpSatRsc.getImageGeometry() != null) {
                		
            			satResource = tmpSatRsc;
            			satAreaName = getSatAreaName( tmpSatRsc );
                    	satRscFound = true;                			
            		}
                } else {
    				// if there are multiple panes with a sat resources with
    				// different area names then prompt
    				// the user whether to use the first sat or not continue
    				if( getSatResource( pane ) != null ) {
    					
    					String otherSatAreaName = getSatAreaName( getSatResource( pane ) );
    					
    					if( !satAreaName.equals( otherSatAreaName ) ) {
                            MessageBox mb = new MessageBox(PlatformUI
                                    .getWorkbench().getActiveWorkbenchWindow()
                                    .getShell(), SWT.OK);
    			            mb.setText("Information");
                            mb.setMessage("The Geo-Sync Panes option is set for this RBD and there are\n"
                                    + "multiple selected panes with conflicting satellite areas.\n\n"
                                    + "Either deselect the Geo-Sync panes option or select just the\n"
                                    + "one pane with the satellite that you want to set the area to.\n");
    			            mb.open();
    			            return null;
    					}
    				}        			
        		}        		    			
            }
    		
    		// if no satellite found then can't do anything
    		if( !satRscFound ) {
                MessageBox mb = new MessageBox(PlatformUI.getWorkbench()
                		.getActiveWorkbenchWindow().getShell(), SWT.OK);
                mb.setText("Error");
                mb.setMessage("Unable to change the Display Area. \n"
                        + "No Satellite Resources are Loaded\n"
                        + "in the selected Panes.");
                mb.open();
                return null;
    		}
    		
    		// else we have the satellite resource to reproject to for all panes
    	}
    	
    	// loop thru each pane and set the area
    	//
        for( IDisplayPane pane : displayPanes ) {
        	
        	// if geoSync not set then look for a sat rsc for this pane
        	//
        	if( !geoSyncedPanes ) {
            	satResource = getSatResource( pane );        	
            	
                // if not geoSynced and no satellite then don't do anything for
                // this pane
            	if( satResource == null ) {
            		continue;
                } else if (satResource.getImageGeometry() == null) {
                    MessageBox mb = new MessageBox(PlatformUI.getWorkbench()
                    		.getActiveWorkbenchWindow().getShell(), SWT.OK);
                    mb.setText("Error");
                    mb.setMessage("Unable to get Satellite Image Geometry\n");
                    mb.open();
                    return null;
            	}
            	satRscFound = true;            	
        	}

        	/*
        	 * calculate info needed for new GridGeometry
        	 */
        	GeneralGridGeometry gridgeom = satResource.getImageGeometry();
        	int xdimArea = gridgeom.getGridRange().getSpan(0);
        	int ydimArea = gridgeom.getGridRange().getSpan(1);
            	
            /*
             * Update mapDescriptor with new satellite CRS
             */
            NCMapRenderableDisplay existingDisplay = (NCMapRenderableDisplay) pane
                    .getRenderableDisplay();
            NCMapDescriptor existingMD = (NCMapDescriptor) existingDisplay
                    .getDescriptor();

        	try {
                existingMD.setGridGeometry(new GridGeometry2D(
                        new GeneralGridEnvelope(new int[] { 0, 0 }, new int[] {
                                xdimArea, ydimArea }, false), gridgeom
                                .getEnvelope()));

                double[] center = existingMD.pixelToWorld(new double[] {
                        xdimArea / 2, ydimArea / 2, 0. });

                NCMapRenderableDisplay newDisplay = new NCMapRenderableDisplay(
                        existingMD);

                if (isSizeOfImageMode(mode)) {
                    newDisplay.setExtent(new PixelExtent(pane.getBounds()));
                    existingMD.setSuspendZoom(true);
                    ZoomUtil.suspendZoom(editor);
                } else {
        		newDisplay.setZoomLevel(1.0);
                    existingMD.setSuspendZoom(false);
                    ZoomUtil.allowZoom(editor);
                }

        		newDisplay.setMapCenter(center);
        		newDisplay.setPredefinedAreaName("Satellite");
        		pane.setRenderableDisplay(newDisplay);

        	} catch (VizException e) {
        		// TODO Auto-generated catch block
        		e.printStackTrace();
        	}

        	editor.refresh();        	
        }
        
    	/*
    	 * No satellite Resource in any of the panes?   show message
    	 */
    	if( !satRscFound ) {
            MessageBox mb = new MessageBox(PlatformUI.getWorkbench()
            		.getActiveWorkbenchWindow().getShell(), SWT.OK);
            mb.setText("Error");
            mb.setMessage("Unable to change the Display Area. \nNo Satellite Resources are Loaded.\n");
            mb.open();
            return null;
    	}
    	
        return null;
    }
    
	/*
	 * Look for a Satellite Resource in the list of resources for this pane
	 */	
    private AbstractSatelliteResource getSatResource( IDisplayPane pane  ) {
    	ResourceList rlist = pane.getDescriptor().getResourceList();
    	Iterator<ResourcePair> iter = rlist.iterator();
    	
    	// TODO : What to do if there is more than one sat resource?
    	while ( iter.hasNext() ) {
    		ResourcePair rp = iter.next();
    		if ( rp.getResource() instanceof AbstractSatelliteResource ) {
    			return (AbstractSatelliteResource)rp.getResource();    	
    		}
    	}
    	return null;
    }
    
    private String getSatAreaName( AbstractSatelliteResource satRsc ) {
		// McIdas and GINI store the area differently.
		if( satRsc instanceof McidasSatResource ) {
            RequestConstraint reqCnst = satRsc.getResourceData()
                    .getMetadataMap().get("areaName");
			 if( reqCnst != null ) {
				 return reqCnst.getConstraintValue();
			 }
        } else if (satRsc instanceof GiniSatResource) {
            RequestConstraint reqCnst = satRsc.getResourceData()
                    .getMetadataMap().get("sectorID");
			if( reqCnst != null ) {
				return reqCnst.getConstraintValue();
			}	
		}
		return "";
    }

    private boolean isSizeOfImageMode(String mode) {
        if (mode != null && mode.equalsIgnoreCase(SIZE_OF_IMAGE)) {
            return true;
        }
        return false;
    }
}