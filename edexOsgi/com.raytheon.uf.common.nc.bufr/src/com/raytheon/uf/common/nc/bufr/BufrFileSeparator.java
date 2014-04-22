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
package com.raytheon.uf.common.nc.bufr;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import ucar.nc2.iosp.bufr.writer.BufrSplitter;
import ucar.nc2.iosp.bufr.writer.BufrSplitter.Options;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Utility to split mixed-type BUFR files into separate messages. Creates a new
 * BUFR file on the file system for each separate message.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 1, 2014  2905      bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class BufrFileSeparator {

    private static final IUFStatusHandler log = UFStatus
            .getHandler(BufrFileSeparator.class);

    public static final File DEFAULT_TMP_DIR;

    static {
        final String edexHomeProp = "edex.home";
        String baseDir = System.getProperty(edexHomeProp);
        if (baseDir == null || baseDir.trim().isEmpty()) {
            log.warn("Property '" + edexHomeProp
                    + "' not set, defaulting to system tmp directory");
            DEFAULT_TMP_DIR = new File(System.getProperty("java.io.tmpdir"));
        } else {
            DEFAULT_TMP_DIR = new File(baseDir + File.separator + "data",
                    "processing");
        }
    }

    private static final FilenameFilter BUFR_FILTER = new FilenameFilter() {
        @Override
        public boolean accept(File dir, String name) {
            return name.endsWith(".bufr");
        }
    };

    /**
     * Splits the mixed BUFR file into homogeneous BUFR files that are written
     * to the file system.
     * 
     * @param mixedBufrFile
     * @return list of absolute paths to new BUFR files
     * @throws IOException
     */
    public static List<String> separate(File mixedBufrFile) throws IOException {
        final String inputFile = mixedBufrFile.getAbsolutePath();
        final File outputDir = getOutputDir(mixedBufrFile);
        if (outputDir.exists()) {
            log.warn("BUFR splitter output directory already exists, is file "
                    + mixedBufrFile + " being processed twice?");
        }
        Options options = new Options() {
            @Override
            public String getFileSpec() {
                return inputFile;
            }

            @Override
            public String getDirOut() {
                return outputDir.getAbsolutePath();
            }
        };

        BufrSplitter splitter = new BufrSplitter(options);
        splitter.execute();

        File[] files = outputDir.listFiles(BUFR_FILTER);
        List<String> rval = new ArrayList<String>(files.length);
        for (File f : files) {
            rval.add(f.getAbsolutePath());
        }
        return rval;
    }

    /**
     * Create a temporary output directory based on the input file name
     * 
     * @param inputName
     * @return
     */
    private static File getOutputDir(final File inputFile) {
        String absPath = inputFile.getAbsolutePath();
        /*
         * include absolute path hash to account for files with the same name in
         * different directories (ie hourly subdirectories)
         */
        String hash = Integer.toHexString(absPath.hashCode());
        String name = inputFile.getName() + "-" + hash + "-split";
        File rval = new File(DEFAULT_TMP_DIR, name);
        return rval;
    }

    /**
     * Clean up split files and temp directory
     * 
     * @param mixedBufrFile
     */
    public static void clean(File mixedBufrFile) {
        File outputDir = getOutputDir(mixedBufrFile);
        if (!outputDir.exists()) {
            log.debug("Split output directory removed before clean");
            return;
        }
        for (File f : outputDir.listFiles(BUFR_FILTER)) {
            if (!f.delete() && f.exists()) {
                log.error("Unable to clean up temporary BUFR file: " + f);
            }
        }
        if (!outputDir.delete() && outputDir.exists()) {
            log.error("Unable to clean up temporary BUFR directory: "
                    + outputDir);
        }
    }
}
