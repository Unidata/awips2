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
package com.raytheon.uf.viz.radarapps.fsi;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Scanner;

import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataRetriever;
import com.raytheon.uf.common.dataplugin.radar.util.TerminalRadarUtils;
import com.raytheon.uf.common.dataquery.db.OrderField.ResultOrder;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.response.AbstractResponseMessage;
import com.raytheon.uf.common.message.response.ResponseMessageError;
import com.raytheon.uf.common.message.response.ResponseMessageGeneric;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.catalog.DbQuery;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.exception.VizServerSideException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.awipstools.common.StormTrackData;
import com.raytheon.viz.radar.rsc.image.RadarSRMResource.SRMSource;
import com.raytheon.viz.radar.ui.RadarDisplayControls;
import com.raytheon.viz.radar.ui.RadarDisplayManager;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.raytheon.viz.ui.input.EditableManager;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;

public class FSILauncherLayer extends
        AbstractVizResource<FSILauncherResourceData, MapDescriptor> implements
        IContextMenuContributor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FSILauncherLayer.class);

    private MouseHandler mouseHandler;

    private MenuManager quickMenuManager;

    private MenuManager fullMenuManager;

    private Shell shell;

    public FSILauncherLayer(FSILauncherResourceData fsiLauncherResourceData,
            LoadProperties loadProperties) {
        super(fsiLauncherResourceData, loadProperties);

        IMenuListener menuListener = new IMenuListener() {
            @Override
            public void menuAboutToShow(IMenuManager manager) {
                addLauncherActions(manager);
            }
        };

        shell = VizWorkbenchManager.getInstance().getCurrentWindow().getShell();

        quickMenuManager = new MenuManager("Start FSI");
        quickMenuManager.setRemoveAllWhenShown(true);
        quickMenuManager.addMenuListener(menuListener);
        quickMenuManager.createContextMenu(shell);

        fullMenuManager = new MenuManager("Start FSI");
        fullMenuManager.setRemoveAllWhenShown(true);
        fullMenuManager.addMenuListener(menuListener);

        getRadarList();
    }

    @Override
    public String getName() {
        return "FSI, when editable, right-click over storm";
    }

    @Override
    protected void disposeInternal() {
        getResourceContainer().unregisterMouseHandler(mouseHandler);
        quickMenuManager.dispose();
        fullMenuManager.dispose();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        mouseHandler = new MouseHandler();
        getResourceContainer().registerMouseHandler(mouseHandler);
        EditableManager.makeEditable(this, true);
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        // nothing
    }

    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        if (isEditable()) {
            menuManager.add(fullMenuManager);
        }
    }

    public boolean isEditable() {
        return getCapability(EditableCapability.class).isEditable();
    }

    private void addLauncherActions(IMenuManager manager) {
        if (manager == quickMenuManager) {
            Action a = new Action("Start FSI") {

            };
            a.setEnabled(false);
            manager.add(a);
            manager.add(new Separator());
        }
        for (String radarName : getRadarList()) {
            manager.add(new LaunchFSIAction(radarName));
        }
    }

    private String readScript(String name) {
        InputStream ins = FSILauncherLayer.class.getResourceAsStream(name);
        if (ins != null) {
            Scanner s = new Scanner(ins);
            StringBuilder sb = new StringBuilder();
            while (s.hasNextLine()) {
                sb.append(s.nextLine());
                sb.append('\n');
            }
            s.close();
            return sb.toString();
        }
        return null;
    }

    private List<String> getRadarList() {
        ArrayList<String> result = new ArrayList<String>();
        try {
            String script = readScript("getFsiRadars.py");

            if (script != null) {
                Message message = Connector.getInstance().connectMessage(
                        script, null, 60000);
                AbstractResponseMessage[] absresponses = message.getBody()
                        .getResponses();
                AbstractResponseMessage response = absresponses[0];

                if (response instanceof ResponseMessageError) {
                    ResponseMessageError rme = (ResponseMessageError) response;
                    VizServerSideException innerException = new VizServerSideException(
                            rme.toString());
                    throw new VizServerSideException(rme.getErrorMsg(),
                            innerException);
                } else {
                    try {
                        for (Object o : (ArrayList<?>) ((ResponseMessageGeneric) response)
                                .getContents())
                            result.add((String) o);
                    } catch (RuntimeException e) {
                        throw new VizException("Unexpected server response", e);
                    }
                }
            } else
                throw new VizException("Could not load getFsiRadars.py");
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not retrieve FSI radar list", e);
        }
        return result;
    }

    private Coordinate geoClickedPoint;

    private class MouseHandler extends InputAdapter {

        private static final int MOUSE_BUTTON_TO_USE = 3;

        private boolean clicked = false;

        @Override
        public boolean handleMouseDown(int x, int y, int mouseButton) {
            geoClickedPoint = getResourceContainer().translateClick(x, y);
            if (isEditable()) {
                // panelClickPoint = new Point(x, y);
                if (mouseButton == MOUSE_BUTTON_TO_USE)
                    clicked = true;
            }
            return false;
        }

        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            if (clicked && mouseButton == MOUSE_BUTTON_TO_USE) {
                clicked = false;

                /*
                 * Cannot relate x, y to anything on the screen, so get the
                 * current pointer location.
                 */
                Point cursorLoc = shell.getDisplay().getCursorLocation();

                Menu menu = quickMenuManager.getMenu();
                menu.setLocation(cursorLoc.x, cursorLoc.y);
                menu.setVisible(true);
            }
            return false;
        }
    }

    private static class FSIEnvironment {
        public String lbOutputDir;

        public String rssdHost;
    }

    @SuppressWarnings("unchecked")
    private HashMap<String, String> getHashMapFromResponse(
            AbstractResponseMessage response) {
        return (HashMap<String, String>) ((ResponseMessageGeneric) response)
                .getContents();
    }

    private FSIEnvironment getFSIEnvironment() {
        FSIEnvironment result = null;
        try {
            String script = readScript("getFsiEnviron.py");

            if (script != null) {
                Message message = Connector.getInstance().connectMessage(
                        script, null, 60000);
                AbstractResponseMessage[] absresponses = message.getBody()
                        .getResponses();
                AbstractResponseMessage response = absresponses[0];

                if (response instanceof ResponseMessageError) {
                    ResponseMessageError rme = (ResponseMessageError) response;
                    VizServerSideException innerException = new VizServerSideException(
                            rme.toString());
                    throw new VizServerSideException(rme.getErrorMsg(),
                            innerException);
                } else {
                    try {
                        HashMap<String, String> hm = getHashMapFromResponse(response);
                        result = new FSIEnvironment();
                        result.lbOutputDir = hm.get("lbOutputDir");
                        result.rssdHost = hm.get("rssdHost");
                    } catch (RuntimeException e) {
                        throw new VizException("Unexpected server response", e);
                    }
                }
            } else
                throw new VizException("Could not retrieve FSI environment");
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not retrieve FSI radar list", e);
        }
        return result;

    }

    public class LaunchFSIAction extends Action {

        private String radarName;

        private class StormVector {
            public boolean useSTI = true;

            public double speed = 25;

            public double direction = 240;
        }

        public LaunchFSIAction(String radarName) {
            super(radarName);
            this.radarName = radarName;
        }

        /**
         * 
         * @return storm vector {speed, direction} or null (use STI)
         */
        private StormVector getStormVector() {
            StormVector result = new StormVector();

            final String fallbackMessage = "  Using a default vector of 240 degrees, 25 kts.";
            String warning = null;

            RadarDisplayManager rdm = RadarDisplayManager.getInstance();
            RadarDisplayControls rdc = rdm.getCurrentSettings();
            SRMSource srmSource = rdc.getSrmSource();

            if (srmSource == SRMSource.STI) {
                result.useSTI = true;
            } else if (srmSource == SRMSource.CUSTOM) {
                result.useSTI = false;
                result.speed = rdc.getSrmSpeed();
                result.direction = rdc.getSrmDir();
            } else if (srmSource == SRMSource.WARNGEN) {
                result.useSTI = false;
                StormTrackData stormTrackData = ToolsDataManager.getInstance()
                        .getStormTrackData();
                if (stormTrackData != null) {
                    // stormTrackData.getMotionSpeed() is in kts which is what
                    // FSI expects
                    result.speed = stormTrackData.getMotionSpeed();
                    result.direction = stormTrackData.getMotionDirection();
                } else {
                    warning = "Storm vector not properly set.";
                }
            } else {
                warning = "Unknown storm vector type.";
            }

            if (warning != null) {
                // Maybe this should go to alert viz?
                MessageDialog dlg = new MessageDialog(shell,
                        "FSI Storm Vector", null, warning + fallbackMessage,
                        MessageDialog.WARNING, new String[] { "OK" }, 0);
                dlg.open();
            }

            return result;
        }

        private double getRadarLowestElevation(String radarName) {
            /*
             * In AWIPS 1, this comes from $FXA_DATA/radar/kxxx/radarElevs.txt
             * which is generated by RadarServer and/or RadarMsgHandler when
             * GSMs are received.
             * 
             * We just read the GSM. This is a waste of time for WSR-88Ds
             * because the result is always 0.5.
             */

            /*
             * Not necessarily a dedicated radar so use EDEX instead of
             * RadarServer.
             */
            DbQuery query = new DbQuery("radar");
            query.addConstraint("icao", radarName);
            query.addConstraint("productCode", 2);
            query.setOrderAscending(ResultOrder.DESC);
            query.addOrderBy("dataTime");
            query.setMaxResults(1);
            try {
                RadarRecord radarRecord = null;
                try {
                    List<Object[]> obs = query.performQuery();
                    if (obs != null && !obs.isEmpty() && obs.get(0).length > 0) {
                        radarRecord = (RadarRecord) obs.get(0)[0];
                    } else {
                        // default to 0.5 for non-terminal radars, test if
                        // terminal
                        double elevation = 0.5;
                        if (TerminalRadarUtils.isTerminalRadar(radarName)) {
                            elevation = TerminalRadarUtils
                                    .parseTerminalRadarFile().get(radarName)
                                    .get(0);
                        }
                        return elevation;
                    }
                } catch (RuntimeException e) {
                    throw new VizException("Unexpected response format", e);
                }
                radarRecord.setPluginName("radar"); // TODO: huh?
                File loc = HDF5Util.findHDF5Location(radarRecord);
                IDataStore dataStore = DataStoreFactory.getDataStore(loc);
                try {
                    RadarDataRetriever.populateRadarRecord(dataStore,
                            radarRecord);
                    // TODO: They're stored as doubles but still in tenths of a
                    // degree?
                    return radarRecord.getGsmMessage().getElevation()[0] / 10.0;
                } catch (Exception e) {
                    throw new VizException(
                            "Could not retrieve latest GSM for radar", e);
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not retrieve latest GSM for radar " + radarName,
                        e);
            }

            /*
             * TODO: Use ElevationInfo as a fallback? May not give the correct
             * result from TDWRs.
             */
            // TODO: What happens if the wrong number is returned?
            // 0.5 is always correct for WSR-88Ds
            return 0.5;
        }

        private String createControlMessage() {
            FSIEnvironment env = getFSIEnvironment();
            if (env == null)
                return null;

            // According to FSI_GUI, this must have the format ##.##
            String subTypeStr = String.format("%04.2f",
                    getRadarLowestElevation(radarName));

            String latStr = "0.0";
            String lonStr = "0.0";
            if (geoClickedPoint != null) {
                lonStr = Double.toString(geoClickedPoint.x);
                latStr = Double.toString(geoClickedPoint.y);
            }

            StormVector stormVector = getStormVector();

            String stiStr = stormVector.useSTI ? "STI" : "no STI";

            String msg = "<control>\n"
                    + "<command name='remove-all-sources' />\n"
                    + "<command name='set-source' target='fsi' >\n"
                    + "<string name='radar' value='"
                    + radarName
                    + "' />\n"
                    +
                    // Assumes env.lbOutputDir starts with a '/'
                    "<url name='source' value='orpg://"
                    + env.rssdHost
                    + ""
                    + env.lbOutputDir
                    + "/FSIradarLB_"
                    + radarName
                    + ".lb?protocol=xmllb' />\n"
                    + "<timeinterval name='history' value='120' units='minutes' />\n"
                    + "</command>\n"
                    + "<command name='show-product' target='fsi' >\n"
                    + "<string name='source' value='"
                    + radarName
                    + "'/>\n"
                    + "<string name='product' value='Reflectivity'/>\n"
                    + "<string name='subtype' value='"
                    + subTypeStr
                    + "'/>\n"
                    + "<string name='jump' value='latestLowestSubtype'/>\n"
                    + "</command>\n"
                    + "<command name='show-vslice' target='fsi' >\n"
                    + "<string name='source' value='"
                    + radarName
                    + "'/>\n"
                    + "<string name='product' value='Reflectivity'/>\n"
                    + "<angle name='latitude' value='"
                    + latStr
                    + "' units='degrees' />\n"
                    + "<angle name='longitude' value='"
                    + lonStr
                    + "' units='degrees' />\n"
                    + "<length name='altitude' value='2500' units='feet' />\n"
                    + "</command>\n"
                    + "<command name='set-storm-vector' target='fsi' >\n"
                    + "<string name='source' value='"
                    + radarName
                    + "'/>\n"
                    + "<string name='product' value='Reflectivity'/>\n"
                    + "<angle name='direction' value='"
                    + stormVector.direction
                    + "' units='degrees' />\n"
                    + "<speed name='speed' value='"
                    + stormVector.speed
                    + "' units='knots' />\n"
                    + "<string name='STIProductName' value='"
                    + stiStr
                    + "' />\n"
                    + "</command>\n"
                    + "<command name='fsi-select-product' target='fsi'>\n"
                    + "<string name='source' value='"
                    + radarName
                    + "'/>\n"
                    + "<string name='product' value='Reflectivity'/>\n"
                    + "</command>\n"
                    + "<command name='fsi-move-camera' target='fsi' >\n"
                    + "<angle name='latitude' value='"
                    + latStr
                    + "' units='degrees' />\n"
                    + "<angle name='longitude' value='"
                    + lonStr
                    + "' units='degrees' />\n"
                    + "<length name='altitude' value='50' units='meters' />\n"
                    + "<length name='cappi-altitude' value='2000' units='meters' />\n"
                    + "<length name='vslice-altitude' value='20000' units='meters' />\n"
                    + "<angle name='3d-panel-tilt' value='45' units='degrees' />\n"
                    + "<angle name='3d-panel-rotate' value='0' units='degrees' />\n"
                    + "</command>\n" + "</control>";

            return msg;
        }

        private static final String FSI_START_SCRIPT_NAME = "startWG.sh";

        @Override
        public void run() {
            String controlMessage = createControlMessage();
            if (controlMessage == null)
                return;

            File f = PathManagerFactory.getPathManager().getStaticFile(
                    "fsi" + File.separator + FSI_START_SCRIPT_NAME);

            if (f == null || !f.exists()) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not find the FSI start script.");
                return;
            }

            File controlLBFile = null;
            File logFile = null;

            IPath path = Activator.getDefault().getStateLocation();
            if (path != null) {
                File dir = path.toFile();
                if (dir != null) {
                    controlLBFile = new File(dir, "FSIcontrol.lb");
                    logFile = new File(dir, "FSI.log");
                }
            }
            // TODO: or temp file?
            if (controlLBFile == null) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not find location for FSI control file.");
                return;
            }

            String[] cmd = { f.getAbsolutePath(), "-l",
                    controlLBFile.getAbsolutePath(), "-m", controlMessage,
                    "-r", logFile.getAbsolutePath() };

            /*
             * The startWG.sh is not being installed with execute permissions. A
             * user may not have necessary permissions to change this so run the
             * script with the system shell.
             */
            if (!f.canExecute()) {
                String[] cmd2 = new String[cmd.length + 1];
                System.arraycopy(cmd, 0, cmd2, 1, cmd.length);
                cmd2[0] = "sh";
                cmd = cmd2;
            }

            final ProcessBuilder pb = new ProcessBuilder(cmd);
            pb.redirectErrorStream(true);
            // Gathers stderr and exit code and report. AWIPS 1 does *not*
            // do this.
            Thread t = new Thread(new Runnable() {
                @Override
                public void run() {
                    Process p = null;
                    try {
                        p = pb.start();
                        BufferedReader br = new BufferedReader(
                                new InputStreamReader(p.getInputStream()));
                        StringBuilder sb = new StringBuilder();
                        try {
                            final int MAX_LINES = 3;
                            int nLines = 0;
                            while (true) {
                                String s = br.readLine();
                                if (s == null)
                                    break;
                                sb.append(s);
                                sb.append('\n');
                                if (++nLines >= MAX_LINES)
                                    break;
                            }
                        } catch (IOException e) {
                            e.printStackTrace(System.err);
                        }
                        try {
                            p.getInputStream().close();
                        } catch (IOException e) {
                            // nothing
                        }
                        try {
                            int result = p.waitFor();
                            if (result != 0) {
                                // TODO: report error. Does this need an
                                // async exec?
                                // Is this thread-safe?
                                statusHandler
                                        .handle(Priority.PROBLEM,
                                                "FSI failed to start: "
														+ sb.toString());
                                return;
                            }
                        } catch (InterruptedException e) {
                            // nothing
                        }
                    } catch (IOException e) {
                        statusHandler.handle(
                                Priority.PROBLEM,
                                "Could not run FSI start script: "
                                        + e.getMessage(), e);
                    } finally {
                        if (p != null) {
                            p.destroy();
                        }
                    }
                }
            });
            t.setDaemon(true);
            t.start();
        }

    }
}
