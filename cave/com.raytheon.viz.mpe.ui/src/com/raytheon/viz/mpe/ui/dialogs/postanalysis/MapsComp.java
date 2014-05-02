/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.mpe.ui.dialogs.postanalysis;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;


import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener.DisplayChangeType;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory;
import com.raytheon.viz.hydrocommon.util.MPEColors;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.GetColorValues;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.MPEPlotType;
import com.raytheon.viz.mpe.ui.rsc.MPEPlotGriddedResourceData;
import com.raytheon.viz.mpe.ui.rsc.PAAsciiXmrgResource;
import com.raytheon.viz.mpe.ui.rsc.PAAsciiXmrgResourceData;
import com.raytheon.viz.mpe.ui.rsc.PAXmrgResource;
import com.raytheon.viz.mpe.ui.rsc.PAXmrgResourceData;
import com.raytheon.viz.mpe.util.ReadQPFGrids;
import com.raytheon.viz.ui.actions.LoadSerializedXml;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.ui.editor.ISelectedPanesChangedListener;
import com.raytheon.viz.ui.input.PanHandler;
import com.raytheon.viz.ui.panes.VizDisplayPane;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Class containing the maps composite that contains the maps of the dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 14, 2011            lvenable     Initial creation
 * Dec 15, 2013            cgobs        Wrapup of functionality
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class MapsComp implements IMultiPaneEditor {
////////////////////////////////////
    /** The application name */
    private final String APPLICATION_NAME = "hmapmpe";
	//private final String APPLICATION_NAME = "PostAnalysis";
////////////////////////////////////    
    /**
     * Parent shell.
     */
    private Shell parentShell = null;

    /**
     * Post Analysis pane manager.
     */
    private PostAnalysisPaneManager paneManager = new PostAnalysisPaneManager();

    /**
     * Bundle file location
     */
    private static final String BUNDLE_LOC = "bundles/MPE/postAnalysisBundle.xml";

    /**
     * Status handler.
     */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MapsComp.class);

    /**
     * Loop properties.
     */
    private LoopProperties loopProps = new LoopProperties();

    private Rectangle rectBounds;

    private double[] centerArray = new double[3];
    
    private BasePostAnalysisDlg dialog = null;
    
    private static final List<NamedColorUseSet> pColorSetGroup = MPEColors.build_mpe_colors();
    
   // ColorLegendMgr colorLegendMgr = null;
    
//////////////////////////////////////////    
//    private List<NamedColorUseSet> colorSetGroups;
//////////////////////////////////////////    

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public MapsComp(Shell parentShell, BasePostAnalysisDlg baseDialog) {
        this.parentShell = parentShell;
        
        dialog = baseDialog;
        
        initializeMapsComp();
        
  //      setColorLegendMgr( baseDialog.getColorLegendMgr();
//////////////////////////////////////////    
  //      colorSetGroups = MPEDisplayManager.getCurrent().getColorSetGroup();
//////////////////////////////////////////    
    }

    /**
     * Initialize the map comp.
     */
    private void initializeMapsComp() {
        Composite comp = new Composite(parentShell, SWT.NONE);
        GridLayout gl = new GridLayout(1, true);
        gl.horizontalSpacing = 1;
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        comp.setLayout(gl);
        // GridData gd = new GridData(1100, SWT.DEFAULT);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 900;
        comp.setLayoutData(gd);

        
        //////////////////////////////////////////
        loadData();
        //////////////////////////////////////////
     
        createMaps(comp);

    }

    /**
     * Create the maps.
     * 
     * @param mainComp
     */
    private void createMaps(Composite mainComp) {
        Composite paneComp = new Composite(mainComp, SWT.NONE);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 900;
        gd.heightHint = 450;
        paneComp.setLayoutData(gd);

        paneManager.initializeComponents(this, paneComp);
        paneManager.registerMouseHandler(new PanHandler(this));
        initializeMaps();
    }

    /**
     * Initialize the maps.
     */
    private void initializeMaps() {
        File defaultBundle = PathManagerFactory.getPathManager().getStaticFile(
                BUNDLE_LOC);

        try {
            LoadSerializedXml.loadTo(this,
                    Bundle.unmarshalBundle(defaultBundle, getSubstitutions()));
            IDisplayPane[] panes = getDisplayPanes();

            IDisplayPane pane = panes[0];
       
            for (ResourcePair rp : pane.getDescriptor().getResourceList()) {
                if (rp.getProperties().isMapLayer()) {
                    rectBounds = rp.getResource().getDescriptor()
                            .getRenderableDisplay().getBounds();
                }
            }

            System.out.println("rectBounds = " + rectBounds);

        } catch (VizException e) {
            System.out.println("Initialize maps error....");
            statusHandler.handle(Priority.PROBLEM, "Error initializing panes",
                    e);
            parentShell.dispose();
        }
    }
    
//////////////////////////////////////////    
    /**
     * Load the data.  
     */
    private void loadData() {
        
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMddHH");
        dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
    //    String userId = System.getProperty("user.name");
        IDisplayPane[] panes = getDisplayPanes();

        try
        {
        	loadDataByPane(panes[0], dialog.getDataFileName1(), dialog.getDataArray1(), dialog.getExtent1(), 
        							 dialog.getNamedColorUseSet1(), dialog.getResourceType1());
        	
        	loadDataByPane(panes[1], dialog.getDataFileName2(), dialog.getDataArray2(), dialog.getExtent2(),
        							 dialog.getNamedColorUseSet2(), dialog.getResourceType2());
        }
        catch (Throwable t)
        {
        	System.out.println(t.getStackTrace());
        }
        
    }
    
    private void loadDataByPane(IDisplayPane pane, String filePath, float[] dataArray, 
    							java.awt.Rectangle extent,
    							NamedColorUseSet namedColorUseSet,
    							PAResourceType resourceType)
    {
    	
    	if (resourceType == PAResourceType.XMRG)
    	{
    	
    		loadDataByPaneXmrg(pane, filePath, dataArray, extent, namedColorUseSet);
    	}
    	else // (resourceType == PAResourceType.ASCII_XMRG)
    	{
    		loadDataByPaneAsciiXmrg(pane, filePath, dataArray, extent, namedColorUseSet);
    	}
    }
    
    private void loadDataByPaneXmrg(IDisplayPane pane, String filePath, float[] dataArray, java.awt.Rectangle extent, NamedColorUseSet namedColorUseSet)
    {
    	  String userId = System.getProperty("user.name");
    	
    	  IDescriptor descriptor = pane.getDescriptor();

          //DisplayFieldData dataType = DisplayFieldData.gageOnly;
         // String cv_use = dataType.getCv_use();
    
    	  String cv_use = namedColorUseSet.getColor_use_db_name();
    	  int cv_duration = namedColorUseSet.getDefault_duration();
    	  
          String fname = filePath;
          PAXmrgResourceData xmrgRscData = null;
          
          ResourcePair rp = new ResourcePair();
          
          if (filePath != null)
          {
        	  XmrgFile xmrg = new XmrgFile(fname);
              xmrgRscData = new PAXmrgResourceData(xmrg, cv_use);
          }
          else
          {
        	  xmrgRscData = new PAXmrgResourceData(dataArray, extent, cv_use);
          }
          
        
      
          List<NamedColorUseSet> namedColorUseSetList = pColorSetGroup;
           
          List<Colorvalue> colorSet = GetColorValues.get_colorvalues(userId,
        		  APPLICATION_NAME, cv_use,
        		  cv_duration, "E", namedColorUseSetList);



          xmrgRscData.setColorList(colorSet);
          rp.setResourceData(xmrgRscData);
          descriptor.getResourceList().add(rp);
          descriptor.getResourceList().instantiateResources(descriptor, true);
          PAXmrgResource resource = (PAXmrgResource) rp.getResource();
          
          resource.getProperties().setMapLayer(true);       
    }

    
    private void loadDataByPaneAsciiXmrg(IDisplayPane pane, String filePath,
    									 float[] dataArray, java.awt.Rectangle extent, 
    									 NamedColorUseSet namedColorUseSet)
    {
    	  String userId = System.getProperty("user.name");
    	
    	  IDescriptor descriptor = pane.getDescriptor();
  
    	  String cv_use = namedColorUseSet.getColor_use_db_name();
    	  int cv_duration = namedColorUseSet.getDefault_duration();
    	  
          PAAsciiXmrgResourceData gridResourceData = null;
          
          ResourcePair rp = new ResourcePair();

          if (filePath != null)
          {
        	  gridResourceData = new PAAsciiXmrgResourceData(filePath, cv_use);
          }
          else
          {
        	  gridResourceData = new PAAsciiXmrgResourceData(dataArray, extent, cv_use);
        	  //  gridResourceData.
          }
          
          //hook up to MPEColors
          List<NamedColorUseSet> namedColorUseSetList = pColorSetGroup;
                  
          List<Colorvalue> colorSet = GetColorValues.get_colorvalues(userId,
        		  APPLICATION_NAME, cv_use,
        		  cv_duration, "E", namedColorUseSetList);

          
          gridResourceData.setColorList(colorSet);

          //get the resource
          rp.setResourceData(gridResourceData);
          descriptor.getResourceList().add(rp);
          descriptor.getResourceList().instantiateResources(descriptor, true);
          PAAsciiXmrgResource resource = (PAAsciiXmrgResource) rp.getResource();


          //set resource properties    
          ResourceProperties gageResourceProperties = resource.getProperties();
          gageResourceProperties.setMapLayer(true);
  
    }
//////////////////////////////////////////    
    /**
     * Get substitutions for the map center.
     * 
     * @return Map of substitutions.
     */
    private Map<String, String> getSubstitutions() {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();

        Map<String, String> subs = new HashMap<String, String>();
        subs.put("mapCenter", appsDefaults.getToken("mpe_center_lon") + " "
                + appsDefaults.getToken("mpe_center_lat") + " 0.0");

        centerArray[0] = Double.parseDouble(appsDefaults
                .getToken("mpe_center_lon"));
        centerArray[1] = Double.parseDouble(appsDefaults
                .getToken("mpe_center_lat"));
        centerArray[2] = 0.0;

        return subs;
    }

    /**
     * Toggle the overlays.
     * 
     * @param visible
     *            Flag for the visibility.
     * @param menuText
     *            Menu text.
     */
    public void toggleOverlay(boolean visible, String menuText) {
        for (IDisplayPane pane : getDisplayPanes()) {
            for (ResourcePair rp : pane.getDescriptor().getResourceList()) {
                if (rp.getProperties().isMapLayer()
                        && rp.getResource().getName().equals(menuText)) {
                    rp.getProperties().setVisible(visible);
                    
                }
            }
        }
    }

    public void resetZoom() {
        for (IDisplayPane pane : getDisplayPanes()) {
            pane.setZoomLevel(1.0f);
            pane.scaleToClientArea();

            ((VizDisplayPane) pane).getRenderableDisplay()
                    .recenter(centerArray);
 
            ((VizDisplayPane) pane).zoom(.25);
        }
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
        this.loopProps = loopProperties;
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
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#refresh()
     */
    @Override
    public void refresh() {
        loadData();
    	paneManager.refresh();
        
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#translateClick(double,
     * double)
     */
    @Override
    public Coordinate translateClick(double x, double y) {
        IDisplayPane pane = getActiveDisplayPane();
        // Convert the screen coordinates to grid space
        double[] world = pane.screenToGrid(x, y, 0);
        IExtent extent = pane.getRenderableDisplay().getExtent();
        // Verify grid space is within the extent, otherwiser return null
        if ((world == null) || (extent.contains(world) == false)) {
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
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#
     * addRenderableDisplayChangedListener
     * (com.raytheon.uf.viz.core.IRenderableDisplayChangedListener)
     */
    @Override
    public void addRenderableDisplayChangedListener(
            IRenderableDisplayChangedListener displayChangedListener) {
        // Not used

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
        // Not used
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
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#registerMouseHandler(com
     * .raytheon.uf.viz.core.rsc.IInputHandler,
     * com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority)
     */
    @Override
    public void registerMouseHandler(IInputHandler handler,
            InputPriority priority) {
        // TODO Auto-generated method stub

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
        // TODO Auto-generated method stub

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
        // TODO Auto-generated method stub

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
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#addPane(com.raytheon.uf.viz
     * .core.drawables.IRenderableDisplay)
     */
    @Override
    public IDisplayPane addPane(IRenderableDisplay renderableDisplay) {
        return paneManager.addPane(renderableDisplay);
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

}
