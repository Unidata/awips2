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
package com.raytheon.uf.common.dataplugin.gfe.textproduct;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.file.Paths;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.lang3.concurrent.BasicThreadFactory;

import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.python.PyCacheUtil;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;

import jep.JepConfig;

/**
 * Creates a combinations file used to keep track of the areas in text formatter
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 25, 2008           mnash     Initial creation
 * Aug 07, 2013  1561     njensen   Use pm.listFiles() instead of
 *                                  pm.listStaticFiles()
 * Sep 05, 2013  2329     randerso  Moved genereateAutoCombinationsFile here
 *                                  Cleaned up error handling
 * Sep 30, 2013  2361     njensen   Use JAXBManager for XML
 * Feb 05, 2014  2591     randerso  Forced retrieval of combinations file
 *                                  Implemented retry on error
 * Aug 27, 2014  3561     randerso  Yet another attempt to fix combinations file
 *                                  updating
 * Sep 08, 2014  3592     randerso  Changed to use only list site level files as
 *                                  all combo files are saved to the site level
 * Oct 07, 2015  4695     dgilling  Code cleanup to remove compile warnings.
 * Nov 12, 2015  4834     njensen   Changed LocalizationOpFailedException to
 *                                  LocalizationException
 * Nov 18, 2015  5129     dgilling  Support new IFPClient.
 * Feb 05, 2016  5242     dgilling  Remove calls to deprecated Localization
 *                                  APIs.
 * Apr 25, 2016  5605     randerso  Switched back to writing combinations file
 *                                  using Localization
 * Aug 10, 2016  5828     randerso  Fix file dead lock when loading updated
 *                                  combinations file
 * Oct 03, 2016  19293    randerso  Moved CombinationsFileUtil to common
 * Nov 11, 2016  19293    randerso  Added methods to allow siteId to be
 *                                  specified for use in EDEX
 * Feb 16, 2018  7122     randerso  Changed to use String.join(). Code Cleanup
 * Sep 12, 2019  7917     tgurney   Update handling of pyc files for Python 3
 * Jan 28, 2021  8336     randerso  Move loading of combinations file onto a
 *                                  separate thread so it can be called from a
 *                                  procedure.
 *
 * </pre>
 *
 * @author mnash
 */

public class CombinationsFileUtil {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(CombinationsFileUtil.class);

    private static final int MAX_TRIES = 2;

    /**
     * Directory for actual combinations files used by formatters
     */
    public static final String COMBINATIONS_DIR_PATH = LocalizationUtil
            .join("gfe", "combinations");

    /**
     * Directory for saved combo files. These are used to save combinations for
     * later use. The latest combinations used by formatters are stored in
     * COMBINATIONS_DIR_PATH.
     */
    public static final String SAVED_COMBO_DIR = LocalizationUtil.join("gfe",
            "comboData");

    private static final SingleTypeJAXBManager<ComboData> jaxb = SingleTypeJAXBManager
            .createWithoutException(ComboData.class);

    private static final BasicThreadFactory factory = new BasicThreadFactory.Builder()
            .namingPattern("loadCombinations-%d").build();

    private static final ExecutorService executor = Executors
            .newCachedThreadPool(factory);

    /**
     * Zone Combination Data
     *
     * Stores a list of entries for each zone and it's associated zone group
     */
    @XmlRootElement
    @XmlAccessorType(XmlAccessType.NONE)
    public static class ComboData {

        /**
         * ComboData Entry
         *
         * Represents the association of a zone with a zone group
         */
        @XmlRootElement
        @XmlAccessorType(XmlAccessType.NONE)
        public static class Entry {
            /**
             * The group number associated with this zone
             */
            @XmlAttribute
            public int group;

            /**
             * The zone name/id (e.g. NEZ123)
             */
            @XmlAttribute
            public String zone;

            /**
             * Default constructor
             */
            public Entry() {
            }

            /**
             * Constructor
             *
             * @param zone
             * @param group
             */
            public Entry(String zone, int group) {
                this.zone = zone;
                this.group = group;
            }
        }

        @XmlElement(name = "entry")
        private List<Entry> combos;

        /**
         * Default constructor
         */
        public ComboData() {
        }

        /**
         * Constructor
         *
         * @param comboDict
         */
        public ComboData(Map<String, Integer> comboDict) {
            this.combos = new ArrayList<>(comboDict.size());
            for (java.util.Map.Entry<String, Integer> entry : comboDict
                    .entrySet()) {
                this.combos.add(new Entry(entry.getKey(), entry.getValue()));
            }
        }
    }

    /**
     * Gets the saved combo files for the menus in text formatter
     *
     * @return the saved combo files
     */

    public static LocalizationFile[] getSavedCombos() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile[] combos = pm.listFiles(
                pm.getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.SITE),
                SAVED_COMBO_DIR, new String[] { ".xml" }, false, true);

        return combos;
    }

    /**
     * @param file
     * @return the ID for the specified LocalizationFile
     */
    public static String fileToId(ILocalizationFile file) {
        String id = LocalizationUtil.extractName(file.getPath()).replace(".xml",
                "");
        id = FileUtil.unmangle(id);

        return id;
    }

    /**
     * @param id
     * @return the LocalizationFile for the specified ID
     */
    public static ILocalizationFile idToFile(String id) {
        String s = FileUtil.mangle(id) + ".xml";
        IPathManager pm = PathManagerFactory.getPathManager();
        ILocalizationFile lf = pm.getLocalizationFile(
                pm.getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.SITE),
                LocalizationUtil.join(SAVED_COMBO_DIR, s));
        return lf;
    }

    /**
     * Save the combo data using the specified ID
     *
     * @param id
     * @param combos
     *            the combol data
     * @throws LocalizationException
     * @throws SerializationException
     * @throws IOException
     */
    public static void saveComboData(String id, Map<String, Integer> combos)
            throws LocalizationException, SerializationException, IOException {
        ILocalizationFile lf = idToFile(id);
        try (SaveableOutputStream out = lf.openOutputStream()) {
            ComboData comboData = new ComboData(combos);
            jaxb.marshalToStream(comboData, out);
            out.save();
        }
    }

    /**
     * Delete the combo file for the specified ID
     *
     * @param id
     * @throws LocalizationException
     */
    public static void deleteComboData(String id) throws LocalizationException {
        ILocalizationFile lf = idToFile(id);
        lf.delete();
    }

    /**
     * Converts named combination to real name in server.
     *
     * @param mapNames
     * @param name
     * @return the combo file name
     */
    public static String nameToFN(List<String> mapNames, String name) {
        StringBuilder s = new StringBuilder();
        for (String mapName : mapNames) {
            s.append(mapName);
        }
        s.append(':').append(name);

        return s.toString();
    }

    /**
     * Converts filename in server to named combination. May return empty
     * string, indicating invalid name. Note that names are filtered by mapname.
     *
     * @param mapNames
     * @param fn
     * @return the combo name
     */
    public static String fnToName(List<String> mapNames, String fn) {
        StringBuilder s = new StringBuilder();
        for (String mapName : mapNames) {
            s.append(mapName);
        }
        s.append(':');

        if (fn.length() > s.length() && fn.indexOf(s.toString()) == 0) {
            return fn.substring(s.length());
        } else {
            return "";
        }
    }

    /**
     * Loads a saved combo file and returns the combo data
     *
     * @param id
     *            ID of combo data file to load
     * @return the combo data
     * @throws SerializationException
     * @throws IOException
     * @throws LocalizationException
     */
    public static Map<String, Integer> loadComboData(String id)
            throws SerializationException, IOException, LocalizationException {
        ILocalizationFile lf = idToFile(id);
        try (InputStream in = lf.openInputStream()) {
            ComboData comboData = jaxb.unmarshalFromInputStream(in);

            Map<String, Integer> comboDict = new HashMap<>(
                    comboData.combos.size());
            for (ComboData.Entry entry : comboData.combos) {
                comboDict.put(entry.zone, entry.group);
            }

            return comboDict;
        }
    }

    /**
     * Load a combinations file for the default site
     *
     * (normally used in CAVE)
     *
     * @param comboName
     *            combinations name
     * @return the combinations
     * @throws GfeException
     */
    public static List<List<String>> init(String comboName)
            throws GfeException {

        IPathManager pm = PathManagerFactory.getPathManager();

        // retrieve combinations file if it's changed
        LocalizationFile lf = pm.getStaticLocalizationFile(
                LocalizationType.CAVE_STATIC, LocalizationUtil
                        .join(COMBINATIONS_DIR_PATH, comboName + ".py"));

        return loadCombinationsFile(pm, lf, comboName);
    }

    /**
     * Load a combinations file for a specific site
     *
     * (normally used in EDEX)
     *
     * @param siteId
     *            the desired site ID
     * @param comboName
     *            combinations name
     * @return the combinations
     * @throws GfeException
     */
    public static List<List<String>> loadCombinationsFile(String siteId,
            String comboName) throws GfeException {

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext siteContext = pm
                .getContextForSite(LocalizationType.CAVE_STATIC, siteId);
        LocalizationContext configContext = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.CONFIGURED);
        configContext.setContextName(siteId);

        LocalizationFile lf = pm.getStaticLocalizationFile(
                new LocalizationContext[] { siteContext, configContext },
                LocalizationUtil.join(COMBINATIONS_DIR_PATH,
                        comboName + ".py"));

        return loadCombinationsFile(pm, lf, comboName);
    }

    @SuppressWarnings("unchecked")
    private static List<List<String>> loadCombinationsFile(IPathManager pm,
            LocalizationFile lf, String comboName) throws GfeException {
        File file = null;
        if (lf != null) {
            try {
                // retrieve the .py file
                file = lf.getFile(true);
            } catch (LocalizationException e) {
                throw new GfeException(
                        "Error retrieving combinations file: " + comboName, e);
            }
        }

        if (file == null || !file.exists()) {
            return Collections.emptyList();
        }

        final File pyFile = file;

        LocalizationContext context = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        String scriptPath = pm
                .getLocalizationFile(context,
                        LocalizationUtil.join(GfePyIncludeUtil.COMMON_GFE,
                                "CombinationsInterface.py"))
                .getFile().getAbsolutePath();

        Callable<List<List<String>>> readJob = new Callable<List<List<String>>>() {
            @Override
            public List<List<String>> call() throws Exception {
                try (PythonScript python = new PythonScript(
                        new JepConfig()
                                .addIncludePaths(PyUtil.buildJepIncludePath(
                                        lf.getFile().getParent(),
                                        GfePyIncludeUtil
                                                .getCommonPythonIncludePath()))
                                .setClassLoader(CombinationsFileUtil.class
                                        .getClassLoader()),
                        scriptPath)) {
                    Object response = python.execute("getCombinations",
                            Map.of("comboName", comboName));
                    List<List<String>> combos = (List<List<String>>) response;
                    return combos;
                }
            }
        };

        Exception lastException = null;
        for (int retryCount = 0; retryCount < MAX_TRIES; retryCount++) {
            try {
                Future<List<List<String>>> future = executor.submit(readJob);
                List<List<String>> result = future.get();
                return result;
            } catch (Exception e) {
                // if not last try, log and try again
                if (retryCount < MAX_TRIES - 1) {
                    // log but don't pop up
                    statusHandler.handle(Priority.EVENTB,
                            "Error loading combinations file: " + comboName
                                    + ", retrying ",
                            e);
                    lastException = e;

                    try {
                        PyCacheUtil.clean(Paths.get(pyFile.getAbsolutePath()));
                    } catch (IOException e1) {
                        statusHandler.debug(e1.getLocalizedMessage(), e1);
                    }
                }
            }
        }

        // we only get if here if we have exhausted all retries
        throw new GfeException("Error loading combinations file: " + comboName,
                lastException);
    }

    /**
     * Generates combinations files based on just running the formatter
     *
     * Save combinations to a file for the default site
     *
     * (normally used in CAVE)
     *
     * @param zoneGroupList
     *            the zone combinations
     * @param comboName
     *            combinations name
     * @throws LocalizationException
     * @throws Exception
     */
    public static void generateAutoCombinationsFile(
            List<List<String>> zoneGroupList, String comboName)
            throws LocalizationException {

        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        saveCombinationsFile(pm, context, zoneGroupList, comboName);
    }

    /**
     * Save combinations to a file for a specific site
     *
     * (normally used in EDEX)
     *
     * @param siteId
     *            the desired site ID
     * @param zoneGroupList
     *            the zone combinations
     * @param comboName
     *            combinations name
     * @throws LocalizationException
     */
    public static void saveCombinationsFile(String siteId,
            List<List<String>> zoneGroupList, String comboName)
            throws LocalizationException {
        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext context = pm
                .getContextForSite(LocalizationType.CAVE_STATIC, siteId);

        saveCombinationsFile(pm, context, zoneGroupList, comboName);
    }

    private static void saveCombinationsFile(IPathManager pm,
            LocalizationContext context, List<List<String>> zoneGroupList,
            String comboName) throws LocalizationException {

        String fileName = LocalizationUtil.join(COMBINATIONS_DIR_PATH,
                comboName) + ".py";
        LocalizationFile lf = pm.getLocalizationFile(context, fileName);

        // delete the local pycache files to force regeneration
        String pyFilePath = lf.getFile(false).getPath();
        try {
            PyCacheUtil.clean(Paths.get(pyFilePath));
        } catch (IOException e) {
            statusHandler.debug(e.getLocalizedMessage(), e);
        }

        try (SaveableOutputStream stream = lf.openOutputStream();
                Writer outWriter = new OutputStreamWriter(stream)) {

            String zoneComments = "\n# Automatically generated combinations file\n# "
                    + comboName + "\n\nCombinations = [\n";
            outWriter.write(zoneComments);

            NumberFormat df = new DecimalFormat("00");
            for (int i = 0; i < zoneGroupList.size(); i++) {
                StringBuilder nextLineToWrite = new StringBuilder();
                List<String> modZGL = new ArrayList<>(
                        zoneGroupList.get(i).size());
                for (String zone : zoneGroupList.get(i)) {
                    modZGL.add("'" + zone + "'");
                }
                nextLineToWrite.append("\t([");
                nextLineToWrite.append(String.join(",", modZGL));
                nextLineToWrite.append("], ");
                nextLineToWrite.append("'Region");
                nextLineToWrite.append(df.format(i + 1));
                nextLineToWrite.append("' ),\n");
                outWriter.write(nextLineToWrite.toString());
            }
            outWriter.write("]");
            outWriter.close();
            stream.save();

        } catch (Exception e) {
            statusHandler.error(
                    "Error saving combinations file: " + lf.getPath(), e);
        }

    }
}
