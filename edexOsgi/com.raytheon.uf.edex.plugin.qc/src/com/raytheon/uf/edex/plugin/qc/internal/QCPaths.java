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

package com.raytheon.uf.edex.plugin.qc.internal;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * This class should only be used by the QC plug-ins.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * ???           ???      ???       Initial creation
 * Aug 15, 2014  3530     bclement  moved from common to edex
 * Jul 15, 2016  5744     mapeters  Use common_static instead of edex_static
 *                                  in getPythonScriptPath()
 * Jan 05, 2018  6861     njensen   Removed python support
 *                                  Simplified getPointDataDescriptions()
 * 
 * </pre>
 * 
 */
public class QCPaths {

    /** The directory containing the raw QC mesonet data */
    private static final String QC_RAW_DIR;

    static {
        QC_RAW_DIR = EDEXUtil.getEdexData() + File.separator
                + System.getProperty("HDF5_PATH");
    }

    public static Map<String, File> getPaths() throws FileNotFoundException {
        Map<String, File> paths = new HashMap<>();
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
            throw new FileNotFoundException(
                    "Unable to open directory: " + baseDir);
        }
        return paths;
    }

    public static Map<String, PointDataDescription> getPointDataDescriptions() {
        Map<String, PointDataDescription> pdds = new HashMap<>();
        String mesonetPath = "/res/pointdata/pdd/ldadmesonet.xml";
        String msasPath = "/res/pointdata/pdd/msas.xml";

        try (InputStream is = QCPaths.class.getResourceAsStream(mesonetPath)) {
            PointDataDescription desc;
            try {
                desc = PointDataDescription.fromStream(is);
                pdds.put("ldadmesonet", desc);
            } catch (SerializationException e) {
                throw new RuntimeException(
                        "Error reading pointdata description from "
                                + mesonetPath,
                        e);
            }
        } catch (IOException e) {
            // ignore
        }

        try (InputStream is = QCPaths.class.getResourceAsStream(msasPath)) {
            PointDataDescription desc;
            try {
                desc = PointDataDescription.fromStream(is);
                pdds.put("msas", desc);
            } catch (SerializationException e) {
                throw new RuntimeException(
                        "Error reading pointdata description from " + msasPath,
                        e);
            }
        } catch (IOException e) {
            // ignore
        }

        return pdds;
    }

    public static String getBaseDir() {
        return QC_RAW_DIR;
    }

}
