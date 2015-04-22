package gov.noaa.nws.ncep.viz.tools.panZoom;

import gov.noaa.nws.ncep.viz.common.display.INatlCntrsDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;

import java.io.IOException;

import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.viz.ui.UiPlugin;
import com.raytheon.viz.ui.input.InputAdapter;
import com.raytheon.viz.ui.input.preferences.MousePreferenceManager;

/**
 * 
 * PanHandler implements the pan capability's mouse interactions
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Mar 18, 2011             ghull        copied from PanHander and implement geosync
 *    Feb 11, 2013   #972      ghull        AbstractNcEditor instead of NCMapEditor
 *    
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class NcPanHandler extends InputAdapter {
    
    private static IUFStatusHandler statusHandler = UFStatus.getHandler(NcPanHandler.class);

    private static final String PAN_PERCENTAGE = "panningPercentage";

    private static final String PAN_PREF = "com.raytheon.viz.ui.input.pan";

    private static final String ZOOMIN_PREF = "com.raytheon.viz.ui.input.zoomin";

    private static final String ZOOMOUT_PREF = "com.raytheon.viz.ui.input.zoomout";

    private static final String CLEAR_PREF = "com.raytheon.viz.ui.clear";

    private MousePreferenceManager prefManager = MousePreferenceManager
            .getInstance();

    protected IDisplayPaneContainer container;

    protected float theLastMouseX = 0;

    protected float theLastMouseY = 0;

    protected int[] downPosition;

    protected Job job;

    protected int zoomDir = 0;

    protected Double panningPercentage = null;

    /**
     * Constructor
     * 
     * @param container
     *            the container associated with the tool
     */
    public NcPanHandler(IDisplayPaneContainer container) {
    	if( !( container instanceof AbstractNcEditor) ) {
    		System.out.println( "NcPanHandler sanity check : container is not an AbstractNcEditor");
    	}
    	
    	this.container = container;    	
    }

    /**
     * Retarget the pan handler to a specific container
     * 
     * @param container
     */
    public void setContainer(IDisplayPaneContainer container) {
        this.container = container;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int, int,
     * int)
     */
    @Override
    public boolean handleMouseDown(int x, int y, int button) {

    	// TODO : Should we use Raytheon's behaviour preferences for the mouse
    	
        // Dont do anything if it is right button or if it is another button and
        // we have no preference set for that button
//        if (prefManager.handleLongClick(ZOOMIN_PREF, button)
//                || prefManager.handleLongClick(ZOOMOUT_PREF, button)) {
//            theLastMouseX = x;
//            theLastMouseY = y;
//            if (prefManager.handleLongClick(ZOOMIN_PREF, button)) {
//                zoomDir = 1;
//            } else {
//                zoomDir = -1;
//            }
//            if (job == null) {
//                job = new Job("ZoomHandler") {
//
//                    @Override
//                    protected IStatus run(IProgressMonitor monitor) {
//                        if (zoomDir != 0) {
//                            for (IDisplayPane pane : container
//                                    .getDisplayPanes()) {
//
//                                pane.zoom(zoomDir, (int) theLastMouseX,
//                                        (int) theLastMouseY);
//
//                            }
//                            container.refresh();
//                            job.schedule(50);
//                        }
//                        return Status.OK_STATUS;
//                    }
//
//                };
//            }
//            if (job.getState() != Job.RUNNING) {
//                job.schedule(500);
//            }
//        }
    	if(!( container instanceof AbstractNcEditor) )
    		return false;
    	
        if (!prefManager.handleDrag(PAN_PREF, button)
                && !prefManager.handleClick(ZOOMIN_PREF, button)
                && !prefManager.handleClick(ZOOMOUT_PREF, button))
            return false;
        downPosition = new int[] { x, y };
        theLastMouseX = x;
        theLastMouseY = y;
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
     * int, int)
     */
    @Override
    public boolean handleMouseDownMove(int aX, int aY, int button) {
//        if (prefManager.handleLongClick(ZOOMIN_PREF, button)
//                || prefManager.handleLongClick(ZOOMOUT_PREF, button)) {
//            theLastMouseX = aX;
//            theLastMouseY = aY;
//        }
        if ((!prefManager.handleDrag(PAN_PREF, button)) || container == null || 
        	 !(container instanceof AbstractNcEditor) )
            return false;
        
        // NatlCntrs addition to implement the geoSync flag.
        
// add sanity check for all Editors that this handler is supposed to be active for.
        
        AbstractNcEditor ncEditor = (AbstractNcEditor)container;
        boolean geoSyncPanes = NcEditorUtil.arePanesGeoSynced(ncEditor);
        IDisplayPane[] panes = container.getDisplayPanes();
        
        for (IDisplayPane p : panes) {
        	
        	if( !(geoSyncPanes ||
        		 NcEditorUtil.isSelectedPane( ncEditor, p ) ) ) {
        		continue;
        	}
        	
            IView tmpView = (IView) p.getRenderableDisplay().getView().clone();
            tmpView.shiftExtent(new double[] { aX, aY }, new double[] {
                    theLastMouseX, theLastMouseY }, p.getTarget());
            IExtent tmpExtent = tmpView.getExtent();
            double percentage = getPanningPercentage();
            double xMinThreshold = tmpExtent.getMinX()
                    + (tmpExtent.getMaxX() - tmpExtent.getMinX()) * percentage;
            double xMaxThreshold = tmpExtent.getMinX()
                    + (tmpExtent.getMaxX() - tmpExtent.getMinX())
                    * (1.0 - percentage);
            double yMinThreshold = tmpExtent.getMinY()
                    + (tmpExtent.getMaxY() - tmpExtent.getMinY()) * percentage;
            double yMaxThreshold = tmpExtent.getMinY()
                    + (tmpExtent.getMaxY() - tmpExtent.getMinY())
                    * (1.0 - percentage);

            double height = p.getRenderableDisplay().getWorldHeight();
            double width = p.getRenderableDisplay().getWorldWidth();

            int aX2 = aX, aY2 = aY;

            if ((0 <= xMinThreshold && width >= xMaxThreshold) == false) {
                if (((width < xMaxThreshold && theLastMouseX < aX) || (0 > xMinThreshold && theLastMouseX > aX)) == false) {
                    aX2 = (int) theLastMouseX;
                }
            }

            if ((0 <= yMinThreshold && height >= yMaxThreshold) == false) {
                if (((height < yMaxThreshold && theLastMouseY < aY) || (0 > yMinThreshold && theLastMouseY > aY)) == false) {
                    aY2 = (int) theLastMouseY;
                }
            }

            if (aX2 != theLastMouseX || aY2 != theLastMouseY) {
                p.shiftExtent(new double[] { aX2, aY2 }, new double[] {
                        theLastMouseX, theLastMouseY });
            }
        }
        theLastMouseX = aX;
        theLastMouseY = aY;
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int, int)
     */
    public boolean handleMouseUp(int x, int y, int button) {
    	if(!( container instanceof AbstractNcEditor) )
    		return false;
        zoomDir = 0;

//        if (prefManager.handleClick(CLEAR_PREF, button)) {
//            for (IDisplayPane pane : container.getDisplayPanes()) {
//                pane.clear();
//            }
//            return true;
//        }

        if (this.downPosition == null || downPosition[0] != x
                || downPosition[1] != y)
            return false;

//        if (prefManager.handleClick(ZOOMIN_PREF, button)
//                || prefManager.handleClick(ZOOMOUT_PREF, button)) {
//            IDisplayPane[] panes = container.getDisplayPanes();
//
//            Coordinate world = container.translateClick(x, y);
//            if (world == null) {
//                return false;
//            }
//            for (IDisplayPane pane : panes) {
//
//                // Check to see if the click is in the legend area
//                // if it is, don't process any zoom action
//                ResourcePair rsc = UiUtil.getResourceAt(this.container, x, y,
//                        pane.getRenderableDisplay());
//                if (rsc != null)
//                    return false;
//
//                if (prefManager.handleClick(ZOOMIN_PREF, button)) {
//                    pane.zoom(15, x, y);
//                } else {
//                    pane.zoom(-15, x, y);
//                }
//            }
//            container.refresh();
//            return false;
//        }

        container.refresh();
        return false;
    }

    private double getPanningPercentage() {
        if (panningPercentage == null) {
            HierarchicalPreferenceStore store = UiPlugin.getDefault()
                    .getPreferenceStore();
            panningPercentage = UiPlugin.getDefault().getPreferenceStore()
                    .getDouble(PAN_PERCENTAGE);

            if (panningPercentage < 0.0 || panningPercentage > 1.0) {
                // bad value set, reset and store
                panningPercentage = panningPercentage < 0.0 ? 0.0 : 1.0;

                store.setValue(PAN_PERCENTAGE, panningPercentage.doubleValue());
                try {
                    store.save();
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM, "Error saving panning percentage preference", e);
                }
            }
        }
        return panningPercentage.doubleValue();
    }

    // Raytheon's PanHandler implements the wheel zoom here, so we will do the same and 
    // move it from the NCPerspective.
    //
    // TODO : Do we want to use all of their mouse preferences?
    //
    @Override
    public boolean handleMouseWheel(Event event, int x, int y) {
    	if(!( container instanceof AbstractNcEditor) )
    		return false;
    	
        com.raytheon.viz.ui.input.preferences.MouseEvent SCROLL_FORWARD = com.raytheon.viz.ui.input.preferences.MouseEvent.SCROLL_FORWARD;
        com.raytheon.viz.ui.input.preferences.MouseEvent SCROLL_BACK = com.raytheon.viz.ui.input.preferences.MouseEvent.SCROLL_BACK;
        
        // NatlCntrs addition to implement the geoSync flag.

        AbstractNcEditor ncEditor = (AbstractNcEditor)container;
        boolean geoSyncPanes = NcEditorUtil.arePanesGeoSynced(ncEditor);
        IDisplayPane[] panes = ( geoSyncPanes ? container.getDisplayPanes() :
        	                     NcEditorUtil.getSelectedPanes(ncEditor) );
                
        if ((event.stateMask & SWT.SHIFT) == 0
                && container.translateClick(x, y) != null) {

            if ((event.count < 0 && prefManager.handleEvent(ZOOMIN_PREF,
                    SCROLL_FORWARD))
                    || (event.count > 0 && prefManager.handleEvent(
                            ZOOMOUT_PREF, SCROLL_BACK))) {
            	
                for (IDisplayPane pane : panes) {
                	INatlCntrsDescriptor d = (INatlCntrsDescriptor)pane.getDescriptor();
                	if( !d.getSuspendZoom() ) {
                		pane.zoom(event.count, event.x, event.y);
                	}
                }
                return true;
            } else if ((event.count > 0 && prefManager.handleEvent(
                    ZOOMOUT_PREF, SCROLL_FORWARD))
                    || (event.count < 0 && prefManager.handleEvent(ZOOMIN_PREF,
                            SCROLL_BACK))) {
                for (IDisplayPane pane : panes) {
                    pane.zoom(-event.count, event.x, event.y);
                }
                return true;
            } else if ((event.count > 0 && prefManager.handleEvent(CLEAR_PREF,
                    SCROLL_FORWARD))
                    || (event.count < 0 && prefManager.handleEvent(CLEAR_PREF,
                            SCROLL_BACK))) {

                for (IDisplayPane pane : panes) {
                    pane.clear();
                }
                return true;
            }
        }
        return false;
    }
}
