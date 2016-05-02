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
package com.raytheon.viz.gfe.textformatter;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.viz.gfe.textformatter.CombinationsFileUtil.ComboData.Entry;

/**
 * Creates a combinations file used to keep track of the areas in text formatter
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 25, 2008            mnash       Initial creation
 * Aug 07, 2013       1561 njensen     Use pm.listFiles() instead of pm.listStaticFiles()
 * Sep 05, 2013     #2329  randerso    Moved genereateAutoCombinationsFile here
 *                                     Cleaned up error handling
 * Sep 30, 2013      2361  njensen     Use JAXBManager for XML
 * Feb 05, 2014     #2591  randerso    Forced retrieval of combinations file
 *                                     Implemented retry on error
 * Aug 27, 2014     #3561  randerso    Yet another attempt to fix combinations file updating
 * Sep 08, 2014     #3592  randerso    Changed to use only list site level files as all 
 *                                     combo files are saved to the site level
 * Oct 07, 2015     #4695  dgilling    Code cleanup to remove compile warnings.
 * Apr 25, 2016  #5605     randerso    Switched back to writing combinations file using Localization
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class CombinationsFileUtil {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CombinationsFileUtil.class);

    private static final int MAX_TRIES = 2;

    public static String COMBINATIONS_DIR_PATH = FileUtil.join("gfe",
            "combinations");

    public static String SAVED_COMBO_DIR = FileUtil.join("gfe", "comboData");

    private static final SingleTypeJAXBManager<ComboData> jaxb = SingleTypeJAXBManager
            .createWithoutException(ComboData.class);

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.NONE)
    public static class ComboData {

        @XmlRootElement
        @XmlAccessorType(XmlAccessType.NONE)
        public static class Entry {
            @XmlAttribute
            public int group;

            @XmlAttribute
            public String zone;

            public Entry() {
            }

            public Entry(String zone, int group) {
                this.zone = zone;
                this.group = group;
            }
        }

        @XmlElement(name = "entry")
        private List<Entry> combos;

        public ComboData() {
        }

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
        LocalizationFile[] combos = pm.listFiles(pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE),
                SAVED_COMBO_DIR, new String[] { ".xml" }, false, true);

        return combos;
    }

    public static String fileToId(LocalizationFile file) {
        File f = new File(file.getName());
        String id = f.getName().replace(".xml", "");
        id = FileUtil.unmangle(id);

        return id;
    }

    public static LocalizationFile idToFile(String id) {
        String s = FileUtil.mangle(id) + ".xml";
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile lf = pm.getLocalizationFile(pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE), FileUtil
                .join(SAVED_COMBO_DIR, s));
        return lf;
    }

    public static void saveComboData(String id, Map<String, Integer> combos)
            throws LocalizationException, SerializationException, IOException {
        LocalizationFile lf = idToFile(id);
        try (SaveableOutputStream out = lf.openOutputStream()) {
            ComboData comboData = new ComboData(combos);
            jaxb.marshalToStream(comboData, out);
            out.save();
        }
    }

    public static void deleteComboData(String id)
            throws LocalizationOpFailedException {
        LocalizationFile lf = idToFile(id);
        lf.delete();
    }

    public static String nameToFN(List<String> mapNames, String name) {
        StringBuilder s = new StringBuilder();
        for (String mapName : mapNames) {
            s.append(mapName);
        }
        s.append(':').append(name);

        return s.toString();
    }

    public static String fnToName(List<String> mapNames, String fn) {
        StringBuilder s = new StringBuilder();
        for (String mapName : mapNames) {
            s.append(mapName);
        }
        s.append(':');

        if ((fn.length() > s.length()) && (fn.indexOf(s.toString()) == 0)) {
            return fn.substring(s.length());
        } else {
            return "";
        }
    }

    public static Map<String, Integer> loadComboData(String id)
            throws SerializationException, IOException, LocalizationException {
        LocalizationFile lf = idToFile(id);
        try (InputStream in = lf.openInputStream()) {
            ComboData comboData = (ComboData) jaxb.unmarshalFromInputStream(in);

            Map<String, Integer> comboDict = new HashMap<>(
                    comboData.combos.size());
            for (Entry entry : comboData.combos) {
                comboDict.put(entry.zone, entry.group);
            }

            return comboDict;
        }
    }

    @SuppressWarnings("unchecked")
    public static List<List<String>> init(String comboName) throws GfeException {

        IPathManager pm = PathManagerFactory.getPathManager();

        // retrieve combinations file if it's changed
        LocalizationFile lf = pm.getStaticLocalizationFile(
                LocalizationType.CAVE_STATIC,
                FileUtil.join(COMBINATIONS_DIR_PATH, comboName + ".py"));
        File pyFile = null;
        if (lf != null) {
            try {
                // get the local .py file
                pyFile = lf.getFile(false);

                // delete both the local .py and .pyc files to force retrieval
                // and regeneration
                pyFile.delete();
                File pycFile = new File(pyFile.getPath() + "c");
                pycFile.delete();

                // retrieve the .py file
                pyFile = lf.getFile(true);
            } catch (LocalizationException e) {
                throw new GfeException("Error retrieving combinations file: "
                        + comboName, e);
            }
        }

        if ((pyFile == null) || !pyFile.exists()) {
            return Collections.emptyList();
        }

        LocalizationContext baseContext = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.BASE);

        String scriptPath = FileUtil.join(
                GfePyIncludeUtil.getUtilitiesLF(baseContext).getFile()
                        .getPath(), "CombinationsInterface.py");

        List<List<String>> combos = null;
        HashMap<String, Object> map = new HashMap<>();
        map.put("comboName", comboName);
        for (int retryCount = 0; retryCount < MAX_TRIES; retryCount++) {
            try (PythonScript python = new PythonScript(scriptPath,
                    PyUtil.buildJepIncludePath(
                            GfePyIncludeUtil.getCombinationsIncludePath(),
                            GfePyIncludeUtil.getCommonPythonIncludePath()),
                    CombinationsFileUtil.class.getClassLoader())) {
                Object com = python.execute("getCombinations", map);
                combos = (List<List<String>>) com;

                // if successfully retrieved break out of the loop
                break;
            } catch (JepException e) {
                // remove the .pyc file
                new File(pyFile.getAbsolutePath() + "c").delete();

                // if not last try, log and try again
                if (retryCount < (MAX_TRIES - 1)) {
                    // log but don't pop up
                    statusHandler.handle(Priority.EVENTB,
                            "Error loading combinations file: " + comboName
                                    + ", retrying ", e);
                }
                // else throw exception
                else {
                    throw new GfeException("Error loading combinations file: "
                            + comboName, e);
                }
            }
        }
        return combos;
    }

    /**
     * Generates combinations files based on just running the formatter
     * 
     * @param zoneGroupList
     * @param comboName
     * @throws Exception
     * @throws IOException
     */
    public static void generateAutoCombinationsFile(
            List<List<String>> zoneGroupList, String comboName)
            throws Exception {

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext localization = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);

        String fileName = FileUtil.join(COMBINATIONS_DIR_PATH, comboName)
                + ".py";
        LocalizationFile lf = pm.getLocalizationFile(localization, fileName);

        try (SaveableOutputStream stream = lf.openOutputStream();
                Writer outWriter = new OutputStreamWriter(stream)) {

            String zoneComments = "\n# Automatically generated combinations file\n# "
                    + comboName + "\n\nCombinations = [\n";
            outWriter.write(zoneComments);

            NumberFormat df = new DecimalFormat("00");
            for (int i = 0; i < zoneGroupList.size(); i++) {
                StringBuilder nextLineToWrite = new StringBuilder();
                List<String> modZGL = new ArrayList<>(zoneGroupList.get(i)
                        .size());
                for (String zone : zoneGroupList.get(i)) {
                    modZGL.add("'" + zone + "'");
                }
                nextLineToWrite.append("\t([");
                nextLineToWrite.append(StringUtil.join(modZGL, ','));
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
            statusHandler.error("Error saving combinations file: " + fileName,
                    e);
        }

    }
}
