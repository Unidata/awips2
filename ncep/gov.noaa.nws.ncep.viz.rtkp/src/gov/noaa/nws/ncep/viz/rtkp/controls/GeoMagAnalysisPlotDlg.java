/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.viz.rtkp.controls;

import gov.noaa.nws.ncep.viz.common.display.INatlCntrsDescriptor;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsRenderableDisplay;
import gov.noaa.nws.ncep.viz.resources.manager.AbstractRBD;
import gov.noaa.nws.ncep.viz.resources.time_match.NCTimeMatcher;
import gov.noaa.nws.ncep.viz.rsc.timeseries.rsc.GeoMagResourceData;
import gov.noaa.nws.ncep.viz.ui.display.NCPaneManager;

import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener.DisplayChangeType;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.ui.editor.ISelectedPanesChangedListener;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Display Magnetometer analysis plot (h, hqdc, d and dqdc) Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/14/2014              sgurung     Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class GeoMagAnalysisPlotDlg extends CaveSWTDialog implements
        IMultiPaneEditor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GeoMagAnalysisPlotDlg.class);

    private int height = 750;

    private int width = 850;

    private LoopProperties loopProps = new LoopProperties();

    private NCPaneManager paneManager = null;

    private AbstractRBD<?> rbdBndl = null;

    private String stationCode = null;

    private String sourceId = null;

    public NCPaneManager getPaneManager() {
        return paneManager;
    }

    public void setPaneManager(NCPaneManager paneManager) {
        this.paneManager = paneManager;
    }

    /**
     * Constructor.
     * 
     * @param parentShell
     *            The parent shell
     * @param height
     *            The height of the dialog
     * @param width
     *            The width of the dialog
     * @param rbdBndl
     *            The resource bundle file to load
     * @param stationCode
     *            The stationCode
     * @param height
     *            The sourceId
     */
    public GeoMagAnalysisPlotDlg(Shell parentShell, int height, int width,
            AbstractRBD<?> rbdBndl, String stationCode, String sourceId) {
        super(parentShell);
        setText("Magnetometer H & D Plot for " + stationCode);
        this.height = height;
        this.width = width;
        this.rbdBndl = rbdBndl;
        this.stationCode = stationCode;
        this.sourceId = sourceId;

        paneManager = new NCPaneManager(rbdBndl.getPaneLayout(),
                rbdBndl.getDisplayType());
        paneManager.setContextualMenusEnabled(false);
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        mainLayout.horizontalSpacing = 1;
        mainLayout.verticalSpacing = 1;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        // Initialize all of the controls and layout
        initializeComponents();
    }

    /**
     * Initialize the gui widgets
     */
    private void initializeComponents() {

        Composite comp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, true);
        comp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        gd.widthHint = width;
        gd.heightHint = height;
        comp.setLayoutData(gd);

        paneManager.initializeComponents(this, comp);

        AbstractRenderableDisplay[] bundleDisplays = rbdBndl.getDisplays();
        int numDisplays = bundleDisplays.length;

        for (int i = 0; i < numDisplays; ++i) {
            addPane(bundleDisplays[i]);
            ((INatlCntrsRenderableDisplay) bundleDisplays[i])
                    .setPaneManager(getPaneManager());
            loadResourceBundleDefn(getDisplayPanes()[i],
                    (INatlCntrsRenderableDisplay) bundleDisplays[i],
                    (NCTimeMatcher) bundleDisplays[i].getDescriptor()
                            .getTimeMatcher());
        }
    }

    public boolean loadResourceBundleDefn(IDisplayPane pane,
            INatlCntrsRenderableDisplay rendDisplay, NCTimeMatcher timeMatcher) {

        if (timeMatcher == null) {
            System.out.println("Error Loading  Timeline???");
            return false;
        }

        INatlCntrsDescriptor descr = (INatlCntrsDescriptor) rendDisplay
                .getDescriptor();

        ResourceList rscList = descr.getResourceList();

        for (int r = 0; r < rscList.size(); r++) {
            if (rscList.get(r).getResourceData() instanceof GeoMagResourceData) {
                GeoMagResourceData rscData = (GeoMagResourceData) rscList
                        .get(r).getResourceData();
                // ResourceName rscName = rscData.getResourceName();
                alterResource(rscData, stationCode, "stationCode");
                alterResource(rscData, sourceId, "sourceId");
            }
        }

        rbdBndl.getTimeMatcher().setLatestRefTime();
        rbdBndl.initTimeline();
        descr.setTimeMatcher(timeMatcher);
        descr.setNumberOfFrames(timeMatcher.getNumFrames());
        DataTime[] dataTimes = timeMatcher.getFrameTimes().toArray(
                new DataTime[0]);

        if (dataTimes == null || dataTimes.length == 0) {
            // descr.setDataTimes( null );
        } else {
            descr.setDataTimes(dataTimes);

            if (timeMatcher.isForecast()) {
                descr.setFrame(0);
            } else {
                descr.setFrame(dataTimes.length - 1);
            }
        }

        descr.setSuspendZoom(true);
        rscList.instantiateResources(descr, true);

        pane.setRenderableDisplay(rendDisplay);
        pane.zoom(15, 0, 0);
        pane.getTarget().setNeedsRefresh(true);
        pane.refresh();

        return true;
    }

    private void alterResource(AbstractRequestableResourceData data,
            String selectedString, String key) {
        Map<String, RequestConstraint> reqMap = data.getMetadataMap();
        RequestConstraint rc = reqMap.get(key);
        if (rc != null) {
            rc.setConstraintValue(selectedString);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#registerMouseHandler(com
     * .raytheon.uf.viz.core.rsc.IInputHandler)
     */
    @Override
    public void registerMouseHandler(IInputHandler handler) {
        paneManager.registerMouseHandler(handler);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#unregisterMouseHandler
     * (com.raytheon.uf.viz.core.rsc.IInputHandler)
     */
    @Override
    public void unregisterMouseHandler(IInputHandler handler) {
        paneManager.unregisterMouseHandler(handler);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#getLoopProperties()
     */
    @Override
    public LoopProperties getLoopProperties() {
        return loopProps;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#setLoopProperties(com.
     * raytheon.uf.viz.core.datastructure.LoopProperties)
     */
    @Override
    public void setLoopProperties(LoopProperties loopProperties) {
        loopProps = loopProperties;
    }

    @Override
    public IDisplayPane addPane(IRenderableDisplay renderableDisplay) {
        return paneManager.addPane(renderableDisplay);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#translateClick(double,
     * double)
     */
    // @Override
    public Coordinate translateClick2(double x, double y) {
        IDisplayPane pane = getActiveDisplayPane();
        // Convert the screen coordinates to grid space
        double[] world = pane.screenToGrid(x, y, 0);
        IExtent extent = pane.getRenderableDisplay().getExtent();
        // Verify grid space is within the extent, otherwiser return null
        if (world == null || extent.contains(world) == false) {
            return null;
        }
        // use descriptor to convert pixel world to CRS world space
        world = pane.getDescriptor().pixelToWorld(world);
        // Check for null
        if (world == null) {
            return null;
        }
        return new Coordinate(world[0], world[1], world[2]);
    }

    @Override
    public Coordinate translateClick(double x, double y) {
        return getPaneManager().translateClick(x, y);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#translateInverseClick(
     * com.vividsolutions.jts.geom.Coordinate)
     */
    @Override
    public double[] translateInverseClick(Coordinate c) {
        if (c == null) {
            return null;
        }
        IDisplayPane pane = getActiveDisplayPane();
        double[] grid = pane.getDescriptor().worldToPixel(
                new double[] { c.x, c.y, c.z });
        if (grid == null) {
            return null;
        }
        return pane.gridToScreen(grid);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#getDisplayPanes()
     */
    @Override
    public IDisplayPane[] getDisplayPanes() {
        return paneManager.getDisplayPanes();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#refresh()
     */
    @Override
    public void refresh() {
        paneManager.refresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#getActiveDisplayPane()
     */
    @Override
    public IDisplayPane getActiveDisplayPane() {
        return paneManager.getActiveDisplayPane();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.editor.IMultiPaneEditor#getNumberofPanes()
     */
    @Override
    public int getNumberofPanes() {
        return paneManager.getNumberofPanes();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#setSelectedPane(java.lang
     * .String, com.raytheon.uf.viz.core.IDisplayPane)
     */
    @Override
    public void setSelectedPane(String action, IDisplayPane pane) {
        paneManager.setSelectedPane(action, pane);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#getSelectedPane(java.lang
     * .String)
     */
    @Override
    public IDisplayPane getSelectedPane(String action) {
        return paneManager.getSelectedPane(action);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#getSelectedPanes(java.lang
     * .String)
     */
    @Override
    public IDisplayPane[] getSelectedPanes(String action) {
        return paneManager.getSelectedPanes(action);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#isSelectedPane(java.lang.
     * String, com.raytheon.uf.viz.core.IDisplayPane)
     */
    @Override
    public boolean isSelectedPane(String action, IDisplayPane pane) {
        return paneManager.isSelectedPane(action, pane);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#addSelectedPaneChangedListener
     * (com.raytheon.viz.ui.editor.ISelectedPanesChangedListener)
     */
    @Override
    public void addSelectedPaneChangedListener(
            ISelectedPanesChangedListener listener) {
        paneManager.addSelectedPaneChangedListener(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#removeSelectedPaneChangedListener
     * (com.raytheon.viz.ui.editor.ISelectedPanesChangedListener)
     */
    @Override
    public void removeSelectedPaneChangedListener(
            ISelectedPanesChangedListener listener) {
        paneManager.removeSelectedPaneChangedListener(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#removePane(com.raytheon.uf
     * .viz.core.IDisplayPane)
     */
    @Override
    public void removePane(IDisplayPane pane) {
        paneManager.removePane(pane);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#hidePane(com.raytheon.uf.
     * viz.core.IDisplayPane)
     */
    @Override
    public void hidePane(IDisplayPane pane) {
        paneManager.hidePane(pane);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#showPane(com.raytheon.uf.
     * viz.core.IDisplayPane)
     */
    @Override
    public void showPane(IDisplayPane pane) {
        paneManager.showPane(pane);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.editor.IMultiPaneEditor#displayedPaneCount()
     */
    @Override
    public int displayedPaneCount() {
        return paneManager.displayedPaneCount();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.editor.IMultiPaneEditor#clear()
     */
    @Override
    public void clear() {
        paneManager.clear();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#
     * addRenderableDisplayChangedListener
     * (com.raytheon.uf.viz.core.IRenderableDisplayChangedListener)
     */
    @Override
    public void addRenderableDisplayChangedListener(
            IRenderableDisplayChangedListener displayChangedListener) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#
     * removeRenderableDisplayChangedListener
     * (com.raytheon.uf.viz.core.IRenderableDisplayChangedListener)
     */
    @Override
    public void removeRenderableDisplayChangedListener(
            IRenderableDisplayChangedListener displayChangedListener) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#
     * notifyRenderableDisplayChangedListeners
     * (com.raytheon.uf.viz.core.IDisplayPane,
     * com.raytheon.uf.viz.core.drawables.IRenderableDisplay,
     * com.raytheon.uf.viz
     * .core.IRenderableDisplayChangedListener.DisplayChangeType)
     */
    @Override
    public void notifyRenderableDisplayChangedListeners(IDisplayPane pane,
            IRenderableDisplay display, DisplayChangeType type) {

    }

    public int getHeight() {
        return height;
    }

    public void setHeight(int height) {
        this.height = height;
    }

    public int getWidth() {
        return width;
    }

    public void setWidth(int width) {
        this.width = width;
    }

    @Override
    public void registerMouseHandler(IInputHandler handler,
            InputPriority priority) {
    }

}
