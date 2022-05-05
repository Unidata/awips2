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
package com.raytheon.uf.viz.monitor.scan;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Provides access to the radar elevations lists data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 22, 2016 5841       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 */
public class ElevationLists implements ILocalizationPathObserver {

    public static final int OTR_VCP = -1;

    public static final int RMR_VCP = -2;

    public static final int OTR_TDWR_VCP = -3;

    public static final int RMR_TDWR_VCP = -4;

    private static final int ANGLE_START_IDX = 3;

    private static final int VCP_IDX = 0;

    public static Pattern VCP_PATTERN = Pattern.compile("VCP(\\d+)");

    public static String ELEVATIONS_PATH = "radar" + IPathManager.SEPARATOR
            + "elevationLists.txt";

    private static Map<Integer, Double[]> elevationAngles = null;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ElevationLists.class);

    private static final ElevationLists LISTENER = new ElevationLists();

    private ElevationLists() {
        super();
    }

    /**
     * Get the elevation angles for the vcp.
     *
     * @return The elevation angles for the vcp, or null if there are no known
     *         elevation angles for the given vcp.
     */
    public static Double[] getElevationsAngles(int vcp) {
        if (elevationAngles == null) {
            loadElevationData();
            PathManagerFactory.getPathManager()
                    .addLocalizationPathObserver(ELEVATIONS_PATH, LISTENER);
        }
        return elevationAngles.get(vcp);
    }

    /**
     * Load the elevation data.
     */
    private static void loadElevationData() {
        Map<Integer, Set<Double>> angles = readElevationData();

        Map<Integer, Double[]> newElevAngles = new HashMap<>();
        for (Entry<Integer, Set<Double>> entry : angles.entrySet()) {
            if (!entry.getValue().isEmpty()) {
                newElevAngles.put(entry.getKey(),
                        entry.getValue().toArray(new Double[0]));
            }
        }
        if (!newElevAngles.isEmpty() || elevationAngles == null) {
            elevationAngles = newElevAngles;
        }
    }

    /**
     * Read elevation data from the localization file.
     */
    private static Map<Integer, Set<Double>> readElevationData() {
        Map<Integer, Set<Double>> angles = new HashMap<>();
        ILocalizationFile lf = PathManagerFactory.getPathManager()
                .getStaticLocalizationFile(LocalizationType.COMMON_STATIC,
                        ELEVATIONS_PATH);
        if (lf == null) {
            return angles;
        }

        try (BufferedReader reader = new BufferedReader(
                new InputStreamReader(lf.openInputStream()))) {

            String line;
            while ((line = reader.readLine()) != null) {
                if (line.startsWith("\t") || (line = line.trim()).isEmpty()
                        || line.startsWith("#") || line.startsWith("/")) {
                    continue;
                }

                String[] parts = line.split("\\s+");
                if (parts.length <= ANGLE_START_IDX) {
                    statusHandler.warn(
                            "Unable to parse the elevation list from " + line);
                    continue;
                }

                String vcpToken = parts[VCP_IDX];
                int vcp;
                Matcher m = VCP_PATTERN.matcher(vcpToken);
                if (m.matches()) {
                    vcp = Integer.parseInt(m.group(1));
                } else if (vcpToken.equals("OTR")) {
                    vcp = OTR_VCP;
                } else if (vcpToken.equals("RMR")) {
                    vcp = RMR_VCP;
                } else if (vcpToken.equals("OTR-TDWR")) {
                    vcp = OTR_TDWR_VCP;
                } else if (vcpToken.equals("RMR-TDWR")) {
                    vcp = RMR_TDWR_VCP;
                } else {
                    statusHandler.warn("Unable to parse the VCP " + vcpToken);
                    continue;
                }

                Set<Double> angleSet = angles.get(vcp);
                if (angleSet == null) {
                    angleSet = new TreeSet<>();
                    angles.put(vcp, angleSet);
                }

                for (int i = ANGLE_START_IDX; i < parts.length; i++) {
                    try {
                        angleSet.add(Double.parseDouble(parts[i]));
                    } catch (NumberFormatException e) {
                        statusHandler
                                .warn("Unable to parse the angle " + parts[i]);
                        break;
                    }
                }
            }
        } catch (IOException | LocalizationException e) {
            statusHandler.handle(Priority.WARN,
                    "Error while reading " + ELEVATIONS_PATH, e);
        }
        return angles;
    }

    @Override
    public void fileChanged(ILocalizationFile file) {
        loadElevationData();
    }
}
