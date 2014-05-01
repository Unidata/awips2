package com.raytheon.viz.awipstools.ui.action;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * This class performs all the filesystem and server input/output for the laps
 * tools. This implementation was ported as directly as possible from awips1,
 * and is not necessarily a good design for multiprocess and multisystem
 * communication.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class LapsToolsIO {

    private static final String SERVER_STATUS_CMD_FRMT = "%s uname -n";

    private static final String TIMESTAMP_CMD_FRMT = "%s %s/etc/fxalogdir.pl";

    private static final String SURFACE_LOG_CMD_FRMT = "%s %s/sfc.pl -l%s -rlaps_sfc";

    private static final String OTHER_LOG_CMD_FRMT = "%s %s/%s %s";

    private static final String CHANGE_CENTER_CMD_FMT = "%s/etc/change-center.pl %s";

    private static final String WINDOW_DOMAIN_CMD_FMT = "%s/etc/window_domain_rt.pl -w laps -i %s -s %s -t %s -c";

    private static final String RELOCALIZE_CMD_FMT = "%s/etc/wfo-relocalization.pl";

    private static final String FINALIZE_CMD_FMT = "/awips/fxa/bin/fxaAnnounce '%s' SYSTEM SIGNIFICANT; rm %s";

    private static String serverName;

    private static File lapsRoot;

    private static File scriptDir;

    private static String execCmd;

    private static File nest7grid;

    private static File nest7grid_template;

    private static File nest7grid_orig;

    private static File fxaData;

    private static List<String> dataChoices;

    private static List<String> dataScripts;

    private static File cornerPointFile;

    private static File lockFile;

    static {
        // TODO all this configuration should be customizable by the user.
        serverName = "px1f";
        if (System.getenv("FXA_DATA") == null) {
            fxaData = new File("/awips/fxa");
        } else {
            fxaData = new File(System.getenv("FXA_DATA"));
        }
        if (System.getenv("LAPSROOT") == null) {
            lapsRoot = new File(fxaData + "/laps");
        } else {
            lapsRoot = new File(System.getenv("LAPSROOT"));
        }
        scriptDir = new File(lapsRoot + "/etc");
        execCmd = "ssh -q " + serverName;
        dataChoices = Arrays.asList("Surface", "Wind", "Humidity", "Clouds",
                "Temperature");
        dataScripts = Arrays.asList("sfc.pl", "wind3d.pl", "hum3d.pl",
                "cloud.pl", "temp.pl");
        nest7grid = new File(fxaData + "/laps/static/nest7grid.parms");
        nest7grid_orig = new File(fxaData + "/laps_data/static/nest7grid.parms");
        nest7grid_template = new File(fxaData + "/laps_domain/nest7grid.parms");
        cornerPointFile = new File(fxaData + "/laps/static/corners.dat");
        lockFile = new File(fxaData + "/laps_domain/lock_laps_ui");
    }

    public static Collection<String> getDataChoices() {
        return dataChoices;
    }

    public static void lockLaps() throws VizException {
        if (lockFile.exists()) {
            throw new VizException(
                    "Sorry, but LAPS tool is currently processing another request and will not be available until the current process has completed.");
        }
        // For some reason awips1 doesn't actually lock it until you localize
    }

    public static void unlockLaps() {
        if (lockFile.exists()) {
            lockFile.delete();
        }
    }

    public static String getFailover() throws IOException,
            InterruptedException, VizException {
        Process p = null;
        try {
            p = Runtime.getRuntime().exec(
                    String.format(SERVER_STATUS_CMD_FRMT, execCmd));
            int status = p.waitFor();
            String result = readOutput(p);

            if (status == 0) {
                // Success getting to machine
                String actual_name = result.split("-")[0];
                if (!serverName.replaceAll("f$", "").equals(actual_name)) {
                    // Failover state
                    return String
                            .format("Note: %s is running in a failover state. LAPS log files may not yet be available.",
                                    serverName);
                }
            } else {
                throw new VizException(
                        String.format(
                                "Sorry, error getting to %s system. The LAPS Tools GUI is not available.",
                                serverName));
            }
        } finally {
            if (p != null) {
                p.destroy();
            }
        }
        return null;
    }

    public static String getLogs(String type) throws IOException,
            InterruptedException {
        Process tsProcess = null;
        Process logProcess = null;
        try {
            tsProcess = Runtime.getRuntime().exec(
                    String.format(TIMESTAMP_CMD_FRMT, execCmd, lapsRoot));
            tsProcess.waitFor();
            String timeStamp = readOutput(tsProcess);
            String cmd = null;
            if (type.equals("Surface")) {
                cmd = String.format(SURFACE_LOG_CMD_FRMT, execCmd, scriptDir,
                        timeStamp);
            } else {
                cmd = String.format(OTHER_LOG_CMD_FRMT, execCmd, scriptDir,
                        dataScripts.get(dataChoices.indexOf(type)), timeStamp);
            }
            logProcess = Runtime.getRuntime().exec(cmd);
            logProcess.waitFor();
            String result = readOutput(logProcess);
            return result;
        } finally {
            if (tsProcess != null) {
                tsProcess.destroy();
            }
            if (logProcess != null) {
                logProcess.destroy();
            }
        }
    }

    public static LapsToolsData loadData() throws IOException, VizException,
            SpatialException {
        LapsToolsData data = new LapsToolsData();
        LapsToolsIO.readParmsFile(data);
        LapsToolsIO.readCornerPointsFile(data);
        LapsToolsIO.readCountyWarningArea(data);
        return data;
    }

    public static String getLocalizationQuestion() {
        String date = new SimpleDateFormat("HH:mm").format(SimulatedTime
                .getSystemTime().getTime());
        return String
                .format("Are you sure you want to localize? \n- The procedure takes five minutes. Consider that it is %s and LAPS runs at 20 after the hour. \n\n- Localization runs remotely on %s. The LAPS Tools will be unavailable until the relocalization is complete, which will be noted in the AlertViz window.",
                        date, serverName);
    }

    public static void localize(LapsToolsData data) throws IOException,
            InterruptedException {
        if (!lockFile.getParentFile().exists()) {
            lockFile.getParentFile().mkdir();
        }
        if (!lockFile.exists()) {
            lockFile.createNewFile();
        }
        if (!nest7grid_template.getParentFile().exists()) {
            nest7grid_template.getParentFile().mkdir();
        }
        Writer writer = new FileWriter(nest7grid_template);
        writer.write(" &lapsparms_nl\n");
        writer.write(String.format(" GRID_CEN_LAT_CMN=%s,\n",
                data.getGridCenter().y));
        writer.write(String.format(" GRID_CEN_LON_CMN=%s,\n",
                data.getGridCenter().x));
        writer.write(String.format(" NX_L_CMN=%s,\n", data.getNx()));
        writer.write(String.format(" NY_L_CMN=%s,\n", data.getNy()));
        writer.write(String.format(" NK_LAPS=%s,\n", data.getNz()));
        writer.write("/\n");
        writer.close();
        String centerPoints = data.getGridCenter().y + " "
                + data.getGridCenter().x;

        String dir = lapsRoot.getAbsolutePath();
        String templateDir = nest7grid_template.getParent();
        String msg = "LAPS localization is finished.";
        // execute the command in a shell to achieve proper IO redirection to
        // laps_ui.log
        Process p = null;
        try {
            p = Runtime.getRuntime().exec("/bin/sh");
            writer = new OutputStreamWriter(p.getOutputStream());
            writer.write(execCmd);
            writer.write(" \"");
            writer.write(String
                    .format(CHANGE_CENTER_CMD_FMT, dir, centerPoints));
            writer.write("; ");
            writer.write(String.format(WINDOW_DOMAIN_CMD_FMT, dir, dir, dir,
                    templateDir));
            writer.write("; ");
            writer.write(String.format(RELOCALIZE_CMD_FMT, dir));
            writer.write("; ");
            writer.write(String.format(FINALIZE_CMD_FMT, msg, lockFile));
            writer.write("\"");
            writer.write(" >& /tmp/laps_ui.log &");
            writer.write("\n");
            writer.close();

            // Don't wait for the process to finish.
            final Process process = p;

            Thread t = new Thread(new Runnable() {

                @Override
                public void run() {
                    try {
                        process.waitFor();
                    } catch (InterruptedException e) {
                        // Ignore
                    } finally {
                        process.destroy();
                    }
                }
            });
            // Let the Thread do the destroy when process is finished.
            p = null;
            t.start();
        } finally {
            if (p != null) {
                p.destroy();
            }
        }
    }

    public static void readParmsFile(LapsToolsData data) throws IOException,
            VizException {
        loadFileParms(nest7grid, data);
    }

    public static void readDefaultParmsFile(LapsToolsData data)
            throws IOException, VizException {
        if (!nest7grid_orig.isFile()) {
            loadFileParms(nest7grid_orig, data);
        }
        loadFileParms(nest7grid_orig, data);
    }

    private static void loadFileParms(File file, LapsToolsData data)
            throws VizException, IOException {
        BufferedReader reader;
        try {
            reader = new BufferedReader(new FileReader(file));
        } catch (FileNotFoundException e) {
            throw new VizException(String.format(
                    "LAPS Tools GUI cannot run. %s does not exist.",
                    file.getAbsolutePath()), e);
        }
        int index = 0;
        for (String line = reader.readLine(); line != null; line = reader
                .readLine()) {
            index += 1;
            if (index == 1 || line.length() < 3 || line.startsWith("c")
                    || line.startsWith(" &")) {
                // Ignore comments, short lines and the first line, because
                // Awips1 does
            } else {
                line = line.trim();
                line = line.replaceAll(",$", "");
                String[] halves = line.split("=");
                String var = halves[0].toLowerCase().trim();
                if (var.equals("grid_cen_lon_cmn")) {
                    data.setGridCenterLon(Double.valueOf(halves[1].trim()));
                } else if (var.equals("grid_cen_lat_cmn")) {
                    data.setGridCenterLat(Double.valueOf(halves[1].trim()));
                } else if (var.equals("grid_spacing_m_cmn")) {
                    data.setGridSpacing(Double.valueOf(halves[1].trim()));
                } else if (var.equals("nx_l_cmn")) {
                    data.setNx(Integer.valueOf(halves[1].trim()));
                } else if (var.equals("ny_l_cmn")) {
                    data.setNy(Integer.valueOf(halves[1].trim()));
                } else if (var.equals("standard_latitude")) {
                    data.setLat(Double.valueOf(halves[1].trim()));
                } else if (var.equals("standard_latitude2")) {
                    data.setLat2(Double.valueOf(halves[1].trim()));
                } else if (var.equals("standard_longitude")) {
                    data.setLon(Double.valueOf(halves[1].trim()));
                } else if (var.equals("nk_laps")) {
                    data.setNz(Integer.valueOf(halves[1].trim()));
                } else if (var.equals("pressure_interval_l")) {
                    data.setDp(Double.valueOf(halves[1].trim()));
                } else if (var.equals("pressure_bottom_l")) {
                    data.setLowp(Double.valueOf(halves[1].trim()));
                }
            }
        }
    }

    private static void readCornerPointsFile(LapsToolsData data)
            throws VizException, NumberFormatException, IOException {
        BufferedReader reader;
        try {
            reader = new BufferedReader(new FileReader(cornerPointFile));
        } catch (FileNotFoundException e) {
            throw new VizException(String.format(
                    "LAPS Tools GUI cannot run. %s does not exist.",
                    cornerPointFile.getAbsolutePath()), e);
        }

        Coordinate[] corners = new Coordinate[4];
        int count = 0;
        for (String line = reader.readLine(); line != null; line = reader
                .readLine()) {
            corners[count] = new Coordinate(Double.valueOf(line.trim().split(
                    " ", 2)[0].trim()), Double.valueOf(line.trim()
                    .split(" ", 2)[1].trim()));
            count += 1;
        }
        data.setCorners(corners);
        reader.close();
    }

    private static void readCountyWarningArea(LapsToolsData data)
            throws SpatialException {
        Map<String, RequestConstraint> map = new HashMap<String, RequestConstraint>();
        map.put("cwa", new RequestConstraint(LocalizationManager.getInstance()
                .getCurrentSite()));
        SpatialQueryResult[] result = SpatialQueryFactory.create().query("cwa",
                null, null, map, null);
        if (result == null || result.length == 0) {
            return;
        }
        data.setCwaArea(result[0].geometry.getEnvelopeInternal());
    }

    private static String readOutput(Process p) throws IOException,
            InterruptedException {
        StringBuilder output = new StringBuilder();
        char[] buf = new char[10];
        Reader reader = new InputStreamReader(p.getInputStream());
        for (int count = 0; count != -1; count = reader.read(buf)) {
            output.append(buf, 0, count);
        }
        reader.close();
        return output.toString();
    }

}
