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
package com.raytheon.uf.edex.core.modes;

import java.io.File;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.regex.Pattern;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.common.util.PropertiesUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.props.PropertiesFactory;

/**
 * EDEX utility class for accessing mode and mode configuration files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 5, 2013  2566       bgonzale     Initial creation.  Refactored from Executor.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class EDEXModesUtil {

    public static final String XML = ".xml";

    public static final Pattern XML_PATTERN = Pattern.compile("\\" + XML);

    public static final String RES_SPRING = "res/spring/";

    public static final Pattern RES_SPRING_PATTERN = Pattern
            .compile("res/spring/");

    private static final String MODES_FILE = "modes.xml";

    public static final String CONF_DIR = EDEXUtil.EDEX_HOME + File.separator
            + "conf";

    /**
     * Populates files with a list of files that match in the specified
     * directory
     * 
     * Returns a list of plugins, etc
     * 
     * @param jarDir
     * @param files
     * @return
     * @throws IOException
     * @throws JAXBException
     */
    public static List<String> extractSpringXmlFiles(List<String> files,
            String modeName) throws IOException, JAXBException {
        FilenameFilter filter = getModeFilter(modeName);
        String pluginDirStr = PropertiesFactory.getInstance()
                .getEnvProperties().getEnvValue("PLUGINDIR");

        List<String> retVal = new ArrayList<String>();
        File jarDirFile = new File(pluginDirStr);
        File[] jars = jarDirFile.listFiles();

        List<JarFile> jarList = new ArrayList<JarFile>(jars.length);
        for (File p : jars) {
            if (p.getName().endsWith(".jar")) {
                JarFile jar = new JarFile(p);
                jarList.add(jar);
            }
        }

        for (JarFile jar : jarList) {
            Enumeration<JarEntry> entries = jar.entries();
            while (entries.hasMoreElements()) {
                JarEntry e = entries.nextElement();
                String name = e.getName();
                if (filter.accept(null, name)) {
                    files.add(name);
                    retVal.add(RES_SPRING_PATTERN.matcher(
                            XML_PATTERN.matcher(name).replaceAll(""))
                            .replaceAll(""));
                }
            }

        }

        return retVal;
    }

    private static FilenameFilter getModeFilter(String modeName) throws IOException,
            JAXBException {
        File confDir = new File(CONF_DIR);
        EdexModesContainer emc = getModesContainer(confDir);
        EdexMode edexMode = emc.getMode(modeName);

        if (edexMode != null && edexMode.isTemplate()) {
            throw new UnsupportedOperationException(modeName
                    + " is a template mode, and is not bootable.");
        }

        FilenameFilter mode = edexMode;

        if (mode == null) {
            if (modeName == null || modeName.length() == 0) {
                mode = new DefaultEdexMode();
            } else {
                throw new UnsupportedOperationException(
                        "No EDEX run configuration specified in modes.xml for "
                                + modeName);
            }
        }
        return mode;
    }

    private static EdexModesContainer getModesContainer(File confDir)
            throws IOException, JAXBException {
        File file = new File(confDir.getPath(), MODES_FILE);

        FileReader reader = null;
        Unmarshaller msh = null;
        try {
            JAXBContext jaxbContext = JAXBContext
                    .newInstance(EdexModesContainer.class);
            msh = jaxbContext.createUnmarshaller();
            reader = new FileReader(file);
            EdexModesContainer emc = (EdexModesContainer) msh.unmarshal(reader);
            return emc;
        } finally {
            if (reader != null) {
                PropertiesUtil.close(reader);
            }
        }
    }

}
