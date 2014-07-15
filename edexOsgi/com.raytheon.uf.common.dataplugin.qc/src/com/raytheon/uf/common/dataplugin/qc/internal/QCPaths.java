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

package com.raytheon.uf.common.dataplugin.qc.internal;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLDecoder;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * This class should only be used by the QC plug-ins.
 */
public class QCPaths {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(QCPaths.class);

    public static final String PYTHON_INCLUDE_PATH;

    /** The directory containing the raw QC mesonet data */
    private static final String QC_RAW_DIR;

    static {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        PYTHON_INCLUDE_PATH = PyUtil.buildJepIncludePath(pathMgr.getFile(
                pathMgr.getContext(LocalizationType.COMMON_STATIC,
                        LocalizationLevel.BASE), "python").getAbsolutePath());
        QC_RAW_DIR = EDEXUtil.getEdexData() + File.separator
                + System.getProperty("HDF5_PATH");

    }

    public static String getPythonScriptPath(String scriptFileName) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        return pathMgr.getFile(
                pathMgr.getContext(LocalizationType.EDEX_STATIC,
                        LocalizationLevel.BASE), "/python/" + scriptFileName)
                .getPath();
    }

    public static Map<String, File> getPaths()
            throws UnsupportedEncodingException, IOException {
        Map<String, File> paths = new HashMap<String, File>();
        File baseDir = new File(QC_RAW_DIR);
        if (baseDir.exists()) {
            File[] files = baseDir.listFiles();
            if (files == null) {
                throw new FileNotFoundException(
                        "Unable to read files in directory: " + baseDir);
            }
            for (File f : files) {
                paths.put(f.getName(), f);
            }
        } else {
            throw new FileNotFoundException("Unable to open directory: "
                    + baseDir);
        }
        return paths;
    }

    public static Map<String, PointDataDescription> getPointDataDescriptions() {
        final String pddPath = "res/pointdata/pdd/";
        URL url = QCPaths.class.getResource("/" + pddPath);
        int colonIndex = url.getPath().indexOf(":") + 1;
        int exclamIndex = url.getPath().indexOf("!");
        String jarPath = url.getPath().substring(colonIndex, exclamIndex);
        Map<String, PointDataDescription> pdds = new HashMap<String, PointDataDescription>();
        File baseDir = new File(QC_RAW_DIR);

        try {
            JarFile jar = new JarFile(URLDecoder.decode(jarPath, "UTF-8"));

            for (Enumeration<JarEntry> entries = jar.entries(); entries
                    .hasMoreElements();) {
                JarEntry entry = entries.nextElement();

                if (entry.getName().startsWith(pddPath)) {
                    String pddFileName = entry.getName();
                    int lastSeparatorIndex = pddFileName
                            .lastIndexOf(File.separator) + 1;
                    int extensionIndex = pddFileName.lastIndexOf(".");

                    if (extensionIndex > 0) {
                        String key = pddFileName.substring(lastSeparatorIndex,
                                extensionIndex);
                        InputStream is = QCPaths.class.getResourceAsStream("/"
                                + pddFileName);
                        PointDataDescription pdd = PointDataDescription
                                .fromStream(is);
                        pdds.put(key, pdd);

                        // check if the corresponding NetCDF storage directory
                        // exists
                        File qcTypeStorageDir = new File(baseDir, key);
                        if (!qcTypeStorageDir.exists()) {
                            qcTypeStorageDir.mkdirs();
                            statusHandler.handle(Priority.INFO,
                                    "Created NetCDF data directory "
                                            + qcTypeStorageDir);
                        }
                    }
                }
            }

            if (jar != null) {
                jar.close();
            }

            return pdds;
        } catch (SerializationException e) {
            throw new RuntimeException(
                    "Failed to initialize QcNetCDF PointDataDescriptions.  QC Python netCDF calls will Fail!");
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(
                    "Failed to initialize QcNetCDF PointDataDescriptions.  QC Python netCDF calls will Fail!");
        } catch (IOException e) {
            throw new RuntimeException(
                    "Failed to initialize QcNetCDF PointDataDescriptions.  QC Python netCDF calls will Fail!");
        }

    }

    public static String getBaseDir() {
        return QC_RAW_DIR;
    }

}
