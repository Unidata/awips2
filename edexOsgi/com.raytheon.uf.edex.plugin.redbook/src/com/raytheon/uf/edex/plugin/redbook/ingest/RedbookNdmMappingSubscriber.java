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
package com.raytheon.uf.edex.plugin.redbook.ingest;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.redbook.RedbookWMOMap;
import com.raytheon.uf.common.dataplugin.redbook.RedbookWMOMap.Info;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.plugin.redbook.decoder.RedbookFcstMap;
import com.raytheon.uf.edex.plugin.redbook.decoder.RedbookFcstMap.MapFcstHr;

/**
 * This class listens for Redbook AWIPS1 NDM files (redbookDataKeys.txt,
 * redbookDepictKeys.txt, redbookProductButtons.txt) to be dropped into EDEX
 * ingest, updates the appropriate mapping file(s) (redbookMapping.xml and/or
 * redbookFcstMap.xml) and tells the Redbook menus that they should be recreated
 * using the new files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 25, 2015 4512       mapeters    Initial creation.
 * 
 * </pre>
 * 
 * @author mapeters
 * @version 1.0
 */
public class RedbookNdmMappingSubscriber extends AbstractRedbookNdmSubscriber {

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RedbookNdmMappingSubscriber.class);

    private static final String FCST_MAP_FILE_NAME = "redbookFcstMap.xml";

    private static final String MAPPING_FILE_NAME = "redbookMapping.xml";

    /*
     * The following map projection values were determined from an A1
     * makeDataSups.csh file. For example, the line
     * "$maksuparg 9 0 -80 0 b04.sup l -20.0 -160.0 50.0 0.0" in the file was
     * used to map "b04" to b04_proj below, where the first number indicates the
     * projection type (9 = mercator), and the fourth number is ignored.
     */
    private static final String b04_proj = "mercator 0 -80 -20.0 -160.0 50.0 0.0";

    private static final String b05_proj = "mercator 0 -80 -3.4581 -121.0959 45.0 -38.9041";

    private static final String b06_proj = "mercator 0 -115 -30.0 -160.0 59.0 0.0";

    private static final String b07_proj = "mercator 0 -120 -4.3808 -160.0 45.5722 -75.0";

    private static final String b08_proj = "mercator 0 -75 -30.0 -160.0 59.0 0.0";

    private static final String B05_proj = "mercator 0 -90 -48.6 170.7 69.4 15.4";

    private static final String B07_proj = "mercator 0 180 -48.6 80.7 69.4 -76.4";

    private static final String b_TSA_proj = "mercator 0 -80 -21.7022 -155.0 51.1567 -30.0";

    private static enum ProjectionKey {
        b04, b05, b06, b07, b08, B05, B07, b_TSA;
    }

    private static Map<String, String> projectionMap;

    private static String getProjection(String key) {
        if (projectionMap == null) {
            projectionMap = new HashMap<String, String>(
                    ProjectionKey.values().length);
            projectionMap.put(ProjectionKey.b04.name(), b04_proj);
            projectionMap.put(ProjectionKey.b05.name(), b05_proj);
            projectionMap.put(ProjectionKey.b06.name(), b06_proj);
            projectionMap.put(ProjectionKey.b07.name(), b07_proj);
            projectionMap.put(ProjectionKey.b08.name(), b08_proj);
            projectionMap.put(ProjectionKey.B05.name(), B05_proj);
            projectionMap.put(ProjectionKey.B07.name(), B07_proj);
            projectionMap.put(ProjectionKey.b_TSA.name(), b_TSA_proj);
        }
        return projectionMap.get(key);
    }

    /**
     * Build and marshal a {@link RedbookFcstMap} object from the given
     * redbookDataKeys.txt contents.
     * 
     * Logic for parsing NDM files comes from the AWIPS1 documentation in the
     * Redbook section of www.nws.noaa.gov/ndm/.
     * 
     * @param dataKeysLines
     */
    private static void buildRedbookFcstMapXml(List<String> dataKeysLines) {
        RedbookFcstMap fcstMap = new RedbookFcstMap();

        for (String line : dataKeysLines) {
            line = line.trim();
            // Skip comment/empty lines
            if (line.startsWith("#") || line.length() == 0) {
                continue;
            }

            String[] parts = line.split("\\|");

            int depictKeyValue = Integer.valueOf(parts[0].trim());
            if (depictKeyValue <= 5000 || depictKeyValue >= 6000) {
                /*
                 * AWIPS1 documentation indicates that depict key values range
                 * from 5000 to 5999, so values outside this are ignored. First
                 * line in redbookDataKeys.txt starts with 5000 and has no WMO
                 * ID, so 5000 is also ignored.
                 */
                continue;
            }

            MapFcstHr value = new MapFcstHr();
            String key = parts[10].trim().substring(0, 6);

            String prdAndOfs = parts[6].trim();
            if (!prdAndOfs.isEmpty()) {
                String[] prdAndOfsArray = prdAndOfs.split(",");
                value.binPeriod = Integer.valueOf(prdAndOfsArray[0]);
                if (prdAndOfsArray.length > 1) {
                    value.binOffset = Integer.valueOf(prdAndOfsArray[1]);
                }
            }
            String fcstHr = parts[3].trim();
            if (!fcstHr.isEmpty()) {
                int fcstHrInt = Integer.valueOf(fcstHr);
                if (fcstHrInt < 0) {
                    // Negative sign indicates value is in days, not
                    // hours
                    int days = fcstHrInt * -1;
                    fcstHr = Integer.toString(days * TimeUtil.HOURS_PER_DAY);
                } else if (fcstHrInt > 240) {
                    // > 240 indicates value is in seconds, not hours
                    int seconds = fcstHrInt;
                    fcstHr = Integer.toString(seconds
                            / TimeUtil.SECONDS_PER_HOUR);
                }
                value.fcstHR = fcstHr;
            }
            fcstMap.addEntry(key, value);
        }

        marshalToXml(fcstMap, FCST_MAP_FILE_NAME);
    }

    /**
     * Build and marshal a {@link RedbookWMOMap} object from the given
     * redbookDataKeys.txt and redbookDepictKeys.txt contents.
     * 
     * @param dataKeysLines
     * @param depictKeysLines
     */
    private static void buildRedbookMappingXml(List<String> dataKeysLines,
            List<String> depictKeysLines) {
        Map<String, String> dataMap = getSubstitutionMap(dataKeysLines);
        Map<String, String> projectionMap = getProjectionMap(dataKeysLines);

        RedbookWMOMap wmoMap = new RedbookWMOMap();
        for (String line : depictKeysLines) {
            line = line.trim();
            // Skip comment/empty lines
            if (line.startsWith("#") || line.length() == 0) {
                continue;
            }

            Info value = new Info();

            String[] depictKeysParts = line.split("\\|");
            String[] dataMapKeys = depictKeysParts[2].trim().split(",");

            StringBuilder key = new StringBuilder();
            String projectionKey = "";
            boolean first = true;
            for (String dataMapKey : dataMapKeys) {
                String dataMapValue = dataMap.get(dataMapKey);
                if (dataMapValue != null) {
                    if (!first) {
                        key.append(",");
                    } else {
                        first = false;
                    }
                    key.append(dataMapValue);
                    /*
                     * Use any key that maps to a value to determine the
                     * projection
                     */
                    projectionKey = dataMapKey;
                }
            }

            String projection = projectionMap.get(projectionKey);
            if (projection != null) {
                value.projection = projection;
            }

            value.name = depictKeysParts[6].trim();

            wmoMap.addEntry(key.toString(), value);
        }

        marshalToXml(wmoMap, MAPPING_FILE_NAME);
    }

    /**
     * Marshal the given Redbook map object to the given file path in
     * localization.
     * 
     * @param redbookMap
     *            the Redbook map object (RedbookWMOMap or RedbookFcstMap)
     * @param locFilePath
     *            the localization file path
     */
    private static void marshalToXml(Object redbookMap, String locFilePath) {
        lock.lock();
        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationFile destinationLocFile = pm.getLocalizationFile(
                    LOC_CONTEXT, "redbook" + IPathManager.SEPARATOR
                            + locFilePath);
            if (!destinationLocFile.getFile().exists()) {
                destinationLocFile.getFile().getParentFile().mkdirs();
            }
            try {
                destinationLocFile.jaxbMarshal(redbookMap,
                        new SingleTypeJAXBManager<>(redbookMap.getClass()));
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, "Failed to marshal "
                        + redbookMap.toString() + " to "
                        + destinationLocFile.getFile().getAbsolutePath(), e);
            }
        } finally {
            lock.unlock();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void notify(final String fileName, final File file) {
        storeFile(fileName, file);
        if (!fileName.equals(PRODUCT_BUTTONS_FILE_NAME)) {
            List<String> dataKeysLines = getNdmFileLines(DATA_KEYS_FILE_NAME);
            List<String> depictKeysLines = getNdmFileLines(DEPICT_KEYS_FILE_NAME);

            if (fileName.equals(DATA_KEYS_FILE_NAME)) {
                buildRedbookFcstMapXml(dataKeysLines);
            }
            buildRedbookMappingXml(dataKeysLines, depictKeysLines);
        }
        RedbookNdmMenuSubscriber.notifyAllMenus();
    }

    /**
     * Get a map of key to projection values (e.g. 5312 --> "mercator 0 -80
     * -21.7022 -155.0 51.1567 -30.0").
     * 
     * @param dataKeys
     *            List of strings from the redbookDataKeys.txt file
     * @return Map for key -> projection string
     */
    private static Map<String, String> getProjectionMap(List<String> dataKeys) {
        Map<String, String> projectionMap = new HashMap<>();
        for (String line : dataKeys) {
            line = line.trim();
            // Skip comment/empty lines
            if (line.startsWith("#") || line.length() == 0) {
                continue;
            }
            String[] parts = line.split("\\|");
            String projectionKey = parts[1].trim();
            if (!projectionKey.isEmpty()) {
                String projection = getProjection(projectionKey);
                if (projection != null)
                    projectionMap.put(parts[0].trim(), projection);
            }
        }

        return projectionMap;
    }
}
