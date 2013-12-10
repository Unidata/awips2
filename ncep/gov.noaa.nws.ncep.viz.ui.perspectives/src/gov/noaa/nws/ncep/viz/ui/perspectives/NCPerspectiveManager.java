package gov.noaa.nws.ncep.viz.ui.perspectives;

import gov.noaa.nws.ncep.staticdataprovider.StaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.controls.PgenFileNameDisplay;
import gov.noaa.nws.ncep.viz.common.area.AreaMenusMngr;
import gov.noaa.nws.ncep.viz.common.area.NcAreaProviderMngr;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsRenderableDisplay;
import gov.noaa.nws.ncep.viz.common.display.INcPaneID;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.gempak.grid.inv.NcGridInventory;
import gov.noaa.nws.ncep.viz.gempak.grid.mapper.GridMapper;
import gov.noaa.nws.ncep.viz.gempak.grid.units.GempakGridParmInfoLookup;
import gov.noaa.nws.ncep.viz.gempak.grid.units.GempakGridVcrdInfoLookup;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.resourceManager.ui.ResourceManagerDialog;
import gov.noaa.nws.ncep.viz.resources.manager.AbstractRBD;
import gov.noaa.nws.ncep.viz.resources.manager.NcMapRBD;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceBndlLoader;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.resources.manager.SpfsManager;
import gov.noaa.nws.ncep.viz.rsc.satellite.units.NcSatelliteUnits;
import gov.noaa.nws.ncep.viz.tools.frame.FrameDataDisplay;
import gov.noaa.nws.ncep.viz.tools.imageProperties.FadeDisplay;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.xml.bind.JAXBException;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.jface.action.ContributionItem;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.commands.ICommandService;

import com.raytheon.uf.common.dataplugin.satellite.units.SatelliteUnits;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.ProgramArguments;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IVizEditorChangedListener;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.input.InputAdapter;
import com.raytheon.viz.ui.perspectives.AbstractCAVEPerspectiveManager;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Manages the life cycle of the National Centers Perspectives
 * 
 * Installs a perspective watcher that handles the transitions in and out of the
 * National Centers perspectives.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 12/2008		22			M. Li		Created
 * 03/2009      75          B. Hebbard  Rename class and all references NMAP->NC
 * 08/05/09                 G. Hull     Load a default RBD
 * 09/27/09     #169        G. Hull     create an NCMapEditor and remove non NC editors
 * 11/05/09     183         Q. Zhou     Added Fading scale
 * 11/13/09     180         G. Hull     NmapCommon.NatlCntrsPerspectiveID
 * 02/20/10     226         G. Hull     Use RbdBundle
 * 03/16/10   238, 239      Archana     Added FrameDataDisplay to the status bar.
 * 05/23/10   dr11 migration G. Hull    manage Cave's TimeDisplay 
 * 05/26/10                 G. Hull     Call NcSatelliteUnits
 * 08/27/10     #303        G. Hull     Set the editor name based on the default RBD name 
 * 09/23/10     #307        G. Hull     Load spf from the command line.
 * 10/20/10     #307        G. Hull     NcAutoUpdater
 * 03/22/11   r1g2-9        G. Hull     extend AbstractCAVEPerspectiveManager
 * 06/07/11     #445        X. Guo      Data Manager Performance Improvements
 *                                      Initialize Data resources
 * 07/28/2011    450        G. Hull     NcPathManager
 * 10/25/2011   #467        G. Hull     close the ResourceManager on deactivate/close
 * 10/26/2011               X. Guo      Init ncgrib inventory
 * 11/22/2011   #514        G. Hull     add an IVizEditorChangedListener to update the GUI when the editor changes
 * 12/13/2011               J. Wu       Added PGEN file name display
 * 02/06/2011               S. Gurung   Commented out code in handleKeyUp and handleKeyDown methods (See NCLegendHandler) 
 * 02/15/2012   627        Archana      Updated the call to addRbd() to accept 
 *                                      a NCMapEditor object as one of the arguments
 *                                      Removed the call to setNcEditor() and updated initFromEditor()
 *                                      to take an editor as one of the arguments    
 * 04/16/2012	740			B. Yin		Start the static data service before opening map editor.  
 * 03/01/2012   #606        G. Hull     initialize NcInventory
 * 05/15/2012               X. Guo      initialize NcGridInventory
 * 05/17/2012               X. Guo      Changed "true" to "false" to initialize NcGridInventory
 * 06/01/2012   #815        G. Hull     Create DESK Level for Localization.
 * 06/13/2012   #817        G. Hull     for -spf arg, create one rbdLoader and call initTimeline on the rbds.
 * 12/12/2012   #630        G. Hull     rm check for suspendZoom in displayChangeLister. code moved to refreshGUIElements 
 * 01/20/2013   #972        G. Hull     set new NcContextActivator   
 * 03/15/2013   #972/#875   G. Hull     override openNewEditor() to be called after raytheon code change to NewAbstractEditor
 * 04/15/2013   #864        G. Hull     display warnings from RD loading.
 * 04/17/2013   #863        G. Hull     Initialize Predefined Areas
 * 11/14/2013   #2361       N. Jensen   Initialize SerializationUtil when activated
 * 
 * </pre>
 * 
 * @author
 * @version 1.0
 */

public class NCPerspectiveManager extends AbstractCAVEPerspectiveManager {
    /** The National Centers Perspective Class */
    // put this in common to avoid dependencies on this project
    public static final String NC_PERSPECTIVE = NmapCommon.NatlCntrsPerspectiveID;

    public static final String newDisplayCmd = "gov.noaa.nws.ncep.viz.ui.newMapEditor";

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NCPerspectiveManager.class);

    private IVizEditorChangedListener displayChangeListener = null;

    // Issue the newDisplay command the same as if called from the main menu
    // new Display
    @Override
    public AbstractEditor openNewEditor() {

        AbstractEditor curEd = NcDisplayMngr.getActiveNatlCntrsEditor();

        NcDisplayType dt = NcEditorUtil.getNcDisplayType(curEd);

        if (dt == NcDisplayType.NSHARP_DISPLAY) {
            // MessageDialog errDlg = new MessageDialog(
            // perspectiveWindow.getShell(), "Error", null,
            // "Can't create more than one NSharp Editor.",
            // MessageDialog.ERROR, new String[] { "OK" }, 0);
            // errDlg.open();
            return null;
        }

        ICommandService service = (ICommandService) curEd.getSite().getService(
                ICommandService.class);

        Command cmd = service.getCommand(newDisplayCmd);
        if (cmd == null) {
            System.out.println("Error getting cmd: " + newDisplayCmd);
            return null;
        }
        try {
            HashMap<String, Object> params = new HashMap<String, Object>();
            params.put("promptForName", "false");

            ExecutionEvent exec = new ExecutionEvent(cmd, params, null, null);
            Object obj = cmd.executeWithChecks(exec);

            if (obj != null && obj instanceof AbstractEditor) {

                return (AbstractEditor) obj;
            }

            System.out.println("sanity check: cmd, " + newDisplayCmd
                    + ", not returning an editor object");

        } catch (Exception e) {
            System.out.println("Error executing cmd: " + newDisplayCmd);
        }

        return null;
    }

    @Override
    protected void open() {
        contextActivator = new NcContextActivator(page);

        // force DESK level to be created.
        NcPathManager.getInstance();

        if (!NcGridInventory.getInstance().isInitialized()) {
            long t0 = System.currentTimeMillis();

            try {
                NcGridInventory.getInstance().initialize(5); // try 5 times
            } catch (final VizException e) {
                // NcGridInventory.getInstance().dumpNcGribInventory();
                MessageDialog errDlg = new MessageDialog(
                        perspectiveWindow.getShell(),
                        "Error",
                        null,
                        "Error initializing NcGridInventory\n"
                                + "Please click OK and wait while a new inventory is created",
                        MessageDialog.ERROR, new String[] { "OK" }, 0);
                errDlg.open();

                try {
                    NcGridInventory.getInstance().createInventory();
                } catch (VizException e1) {
                    errDlg = new MessageDialog(perspectiveWindow.getShell(),
                            "Error", null, "Error creating NcGridInventory\n",
                            MessageDialog.ERROR, new String[] { "OK" }, 0);
                    errDlg.open();
                }
            }

            GridMapper.GridMapperInit();
            GempakGridParmInfoLookup.getInstance();
            GempakGridVcrdInfoLookup.getInstance();
            long t1 = System.currentTimeMillis();
            System.out.println("NcGridInventory Init took: " + (t1 - t0));
        }

        displayChangeListener = new IVizEditorChangedListener() {
            @Override
            public void editorChanged(IDisplayPaneContainer container) {
                if (container == null)
                    return;
                else if (container instanceof AbstractNcEditor) {
                    // ((AbstractNcEditor) container).refreshGUIElements();
                    NcEditorUtil
                            .refreshGUIElements((AbstractNcEditor) container);
                } else {
                    // display a warning/error msg
                }
            }
        };

        // Add an observer to process the dataURI Notification msgs from edex.
        //
        ProductAlertObserver.addObserver(null, new NcAutoUpdater());

        // NatlCntrs uses a different equation to compute the Temperature values
        // from
        // a Satellite IR image so this will override the 'IRPixel' label used
        // by satellite
        // images and will create our Units and UnitConverter class to do the
        // conversion.
        //
        NcSatelliteUnits.register();

        // read in and validate all of the Predefined Area files.
        try {
            List<VizException> warnings = NcAreaProviderMngr.initialize();// .getWarnings();//PredefinedAreasMngr.readPredefinedAreas();

            if (warnings != null && !warnings.isEmpty()) {
                final StringBuffer msgBuf = new StringBuffer(
                        "The following Warnings occurs while loading the Predefined Areas::\n\n");
                int numWarns = 0;
                for (VizException vizex : warnings) {
                    msgBuf.append(" -- " + vizex.getMessage() + "\n");

                    if (++numWarns > 20) {
                        msgBuf.append(" .....and more....");
                        break;
                    }
                }

                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        MessageDialog warnDlg = new MessageDialog(
                                perspectiveWindow.getShell(), "Warning", null,
                                msgBuf.toString(), MessageDialog.WARNING,
                                new String[] { "OK" }, 0);
                        warnDlg.open();
                    }
                });

            }
        } catch (VizException el) {
            MessageDialog errDlg = new MessageDialog(
                    perspectiveWindow.getShell(), "Error", null,
                    "Error Reading in Predefined Areas:\n\n" + el.getMessage(),
                    MessageDialog.ERROR, new String[] { "OK" }, 0);
            errDlg.open();
        }

        AreaMenusMngr.getInstance();

        // Force the RBDs to read from localization to save time
        // bringing up the RBD manager
        //
        SpfsManager.getInstance();

        // Initialize the NcInventory. This cache is stored on the server side
        // and will only
        // need initialization for the first instance of cave.
        try {
            ResourceDefnsMngr.getInstance(); // force reading in of the resource
                                             // definitions

            if (!ResourceDefnsMngr.getInstance().getBadResourceDefnsErrors()
                    .isEmpty()) {

                final StringBuffer errBuf = new StringBuffer(
                        "There were errors creating the following Resource Defintions:\n\n");
                int numErrs = 0;
                for (VizException vizex : ResourceDefnsMngr.getInstance()
                        .getBadResourceDefnsErrors()) {
                    errBuf.append(" -- " + vizex.getMessage() + "\n");

                    if (++numErrs > 20) {
                        errBuf.append(" .....and more....");
                        break;
                    }
                }

                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        MessageDialog errDlg = new MessageDialog(
                                perspectiveWindow.getShell(), "Error", null,
                                errBuf.toString(), MessageDialog.ERROR,
                                new String[] { "OK" }, 0);
                        errDlg.open();
                    }
                });
            }

            if (!ResourceDefnsMngr.getInstance().getResourceDefnWarnings()
                    .isEmpty()) {

                final StringBuffer msgBuf = new StringBuffer(
                        "The following Warnings occurs while loading the Resource Definitions::\n\n");
                int numWarns = 0;
                for (VizException vizex : ResourceDefnsMngr.getInstance()
                        .getResourceDefnWarnings()) {
                    msgBuf.append(" -- " + vizex.getMessage() + "\n");

                    if (++numWarns > 20) {
                        msgBuf.append(" .....and more....");
                        break;
                    }
                }

                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        MessageDialog warnDlg = new MessageDialog(
                                perspectiveWindow.getShell(), "Warning", null,
                                msgBuf.toString(), MessageDialog.WARNING,
                                new String[] { "OK" }, 0);
                        warnDlg.open();
                    }
                });
            }

            // ResourceDefnsMngr.getInstance().createInventory();

        } catch (VizException el) {
            MessageDialog errDlg = new MessageDialog(
                    perspectiveWindow.getShell(), "Error", null,
                    "Error Initializing NcInventory:\n\n" + el.getMessage(),
                    MessageDialog.ERROR, new String[] { "OK" }, 0);
            errDlg.open();
        }

        // Load either the default RBD or RBDs in the command line spf
        //
        List<AbstractRBD<?>> rbdsToLoad = new ArrayList<AbstractRBD<?>>();

        String spfName = ProgramArguments.getInstance().getString("-spf");

        if (spfName != null && !spfName.isEmpty()) {
            String[] grpAndSpf = spfName.split(File.separator);

            // the name of the spf should include a group name
            // TODO : check that there is a group and if not use a default.
            if (grpAndSpf.length != 2) {
                System.out
                        .println("The -spf argument is specified without an spf group (ex spfGroupName/spfName.");
                // load the default rbd...
                MessageDialog errDlg = new MessageDialog(
                        perspectiveWindow.getShell(),
                        "Error",
                        null,
                        "The -spf arguement is missing an SPF group name.\nEx. \"SpfGroupName/SpfName\"",
                        MessageDialog.WARNING, new String[] { "OK" }, 0);
                errDlg.open();
            } else {

                try {
                    rbdsToLoad = SpfsManager.getInstance().getRbdsFromSpf(
                            grpAndSpf[0], grpAndSpf[1], true); // resolve Latest
                                                               // Cycle times
                } catch (VizException e) {
                    MessageDialog errDlg = new MessageDialog(
                            perspectiveWindow.getShell(), "Error", null,
                            "The -spf arguement, " + spfName
                                    + " doen't exist\n", MessageDialog.WARNING,
                            new String[] { "OK" }, 0);
                    errDlg.open();
                }
            }
        }

        if (rbdsToLoad.isEmpty()) {
            try {
                AbstractRBD<?> dfltRbd = NcMapRBD.getDefaultRBD();
                rbdsToLoad.add(dfltRbd);

            } catch (Exception ve) {
                System.out.println("Could not load rbd: " + ve.getMessage());
                ve.printStackTrace();
            }
        }

        // start data provider before creating ncmapeditor
        StaticDataProvider.start();

        ResourceBndlLoader rbdLoader = new ResourceBndlLoader("Loading SPF: ");

        // loop thru the rbds and load them into a new editor.
        for (final AbstractRBD<?> rbd : rbdsToLoad) {

            rbd.initTimeline();

            AbstractEditor editor;

            try {
                editor = NcDisplayMngr.createNatlCntrsEditor(
                        rbd.getDisplayType(), rbd.getRbdName(),
                        rbd.getPaneLayout());
                rbdLoader.addRBD(rbd, editor);

            } catch (final VizException e) {
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        MessageDialog errDlg = new MessageDialog(
                                perspectiveWindow.getShell(), "Error", null,
                                "Error Creating Eclipse Editor for RBD "
                                        + rbd.getRbdName() + "\n"
                                        + e.getMessage(), MessageDialog.ERROR,
                                new String[] { "OK" }, 0);
                        errDlg.open();
                    }
                });
            }
        }

        VizApp.runAsync(rbdLoader);
    }

    @Override
    public void activate() {
        super.activate();

        // initialize SerializationUtil's JAXBContext until deprecated
        // ISerializableObject usage is replaced
        try {
            SerializationUtil.getJaxbContext();
        } catch (JAXBException e) {
            statusHandler.handle(Priority.CRITICAL,
                    "An error occured initializing Serialization", e);
        }

        // add an EditorChangedListener
        VizWorkbenchManager.getInstance().addListener(displayChangeListener);

        // Experiment.
        // statusLine.setErrorMessage("Status Line ERROR MSG B");
        // statusLine.setMessage("Status Line MESSAGE B");

        // read in and validate all of the Predefined Area files.

        List<VizException> warnings = NcAreaProviderMngr.reinitialize();

        if (warnings != null && !warnings.isEmpty()) {
            final StringBuffer msgBuf = new StringBuffer(
                    "The following Warnings occurs while re-initializing the Predefined Areas::\n\n");
            int numWarns = 0;
            for (VizException vizex : warnings) {
                msgBuf.append(" -- " + vizex.getMessage() + "\n");

                if (++numWarns > 20) {
                    msgBuf.append(" .....and more....");
                    break;
                }
            }

            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    MessageDialog warnDlg = new MessageDialog(perspectiveWindow
                            .getShell(), "Warning", null, msgBuf.toString(),
                            MessageDialog.WARNING, new String[] { "OK" }, 0);
                    warnDlg.open();
                }
            });
        }

        // relayout the shell since we added widgets
        perspectiveWindow.getShell().layout(true, true);

        NcSatelliteUnits.register();
    }

    @Override
    public void deactivate() {
        super.deactivate();

        VizWorkbenchManager.getInstance().removeListener(displayChangeListener);

        SatelliteUnits.register();

        // would rather do this another way, preferably by having
        // ResourceManagerDialog extend CaveSWTDialog (do this later) or
        // by implementing a perspective closed listener (cyclical dependency
        // problem)
        ResourceManagerDialog.close();
    }

    @Override
    protected List<ContributionItem> getStatusLineItems() {
        List<ContributionItem> stsLineDisplays = new ArrayList<ContributionItem>();
        // in reverse order
        stsLineDisplays.add(new FadeDisplay());
        stsLineDisplays.add(PgenFileNameDisplay.getInstance());
        stsLineDisplays.add(FrameDataDisplay.createInstance());

        return stsLineDisplays;
    }

    @Override
    public void close() {
        super.close();

        VizWorkbenchManager.getInstance().removeListener(displayChangeListener);
        displayChangeListener = null;

        ResourceManagerDialog.close();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.AbstractVizPerspective#getPerspectiveInputHandlers
     * (com.raytheon.viz.ui.editor.AbstractEditor)
     */
    @Override
    public IInputHandler[] getPerspectiveInputHandlers(
            final AbstractEditor editor) {
        // currently only implementing handleMouseWheel which is now done below.
        IInputHandler[] superHandlers = super
                .getPerspectiveInputHandlers(editor);

        // If this is a GLMapEditor from D2D then just return the
        // abstractEditors handlers
        // (this won't last long since the perspective will remove/save off the
        // editors.

        if (!NcDisplayMngr.isNatlCntrsEditor(editor)) {
            // if (!(editor instanceof AbstractNcEditor)) {
            return superHandlers;
        }

        // No-Ops for doubleClick, keyUp/Down, mouseDown, mouseHover and mouseUp
        IInputHandler handler = new InputAdapter() {

            private boolean isShiftDown = false;

            @Override
            public boolean handleMouseDownMove(int x, int y, int mouseButton) {
                // Set mouse position
                Coordinate ll = editor.translateClick(x, y);
                gov.noaa.nws.ncep.viz.common.CoorBean.getInstance().setCoor(ll);
                return false;
            }

            @Override
            public boolean handleMouseMove(int x, int y) {
                // Set mouse position
                Coordinate ll = editor.translateClick(x, y);
                gov.noaa.nws.ncep.viz.common.CoorBean.getInstance().setCoor(ll);
                return false;
            }

            private void toggleVisibility(ResourcePair rp) {
                AbstractVizResource<?, ?> rsc = rp.getResource();
                if (rsc != null) {
                    rp.getProperties().setVisible(
                            !rp.getProperties().isVisible());
                }
            }

        };

        ArrayList<IInputHandler> handlers = new ArrayList<IInputHandler>();
        // handlers.addAll(Arrays.asList(superHandlers));
        handlers.add(handler);
        return handlers.toArray(new IInputHandler[handlers.size()]);
    }

    @Override
    public void addContextMenuItems(IMenuManager menuManager,
            IDisplayPaneContainer container, IDisplayPane pane) {
        if (!(container instanceof AbstractEditor)) {
            return;
        }

        // TODO : add menu actions to minimize/maximize the selected pane.
        //
        if (container instanceof AbstractNcEditor
                && pane.getRenderableDisplay() instanceof INatlCntrsRenderableDisplay) {

            int numPanes = NcEditorUtil
                    .getNumberOfPanes((AbstractEditor) container);

            if (numPanes > 1) {
                INcPaneID pid = ((INatlCntrsRenderableDisplay) pane
                        .getRenderableDisplay()).getPaneId();

                // menuManager.add( new xxx(pid) );
            }
            // options to delete this pane?? add a new pane.?
        }
    }
}
