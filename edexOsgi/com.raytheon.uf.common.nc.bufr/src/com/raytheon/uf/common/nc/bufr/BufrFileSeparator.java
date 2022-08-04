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
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Formatter;
import java.util.List;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import ucar.nc2.iosp.bufr.writer.BufrSplitter2;

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
 * Apr 01, 2014  2905      bclement     Initial creation
 * Sep 11, 2017  6406      bsteffen     Upgrade ucar
 * Mar 06, 2020  21781     rmanga       Remove "-split" directories 
 * Jan 07, 2020  71884     tjensen      Call BufrSplitter2.exit when done separating
 *
 * </pre>
 *
 * @author bclement
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

    private static final FilenameFilter CAT_FILTER = new FilenameFilter() {
        @Override
        public boolean accept(File dir, String name) {
            return name.startsWith("Category-");
        }
    };

    private static final FilenameFilter BUFR_FILTER = new FilenameFilter() {
        @Override
        public boolean accept(File dir, String name) {
            return name.endsWith(".bufr");
        }
    };
    
    private static final FilenameFilter SPLIT_FILTER = new FilenameFilter() {
        @Override
        public boolean accept(File dir, String name) {
            return name.contains("-split");
        }
    };
    
    /** Recursively delete empty directories. 
     *  Will exit if any files are inside.
     *  
     * @param A directory, of File type object, to be removed.
     * @return Whether or not the directory was recursively removed.
     * */
    private static boolean rmDir (File dir) {
        File[] contents = dir.listFiles();
        // Verifying if directory is actually empty
        if (contents.length > 0 || contents != null) {
            // Attempt to remove empty sub-directories:
            for ( File entity : contents ) {
                if (entity.isDirectory()) {
                    rmDir(entity);  // recursive call
                } else {
                log.error("Could not remove ["+
                dir.getAbsolutePath()+
                "] -- Files found inside diretory");
                return false;
                }
            }
        }
        return dir.delete();
    }

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
        Formatter out = new Formatter();
        BufrSplitter2 splitter = new BufrSplitter2(outputDir.getAbsolutePath(),
                out);
        splitter.execute(inputFile);
        File[] catDirs = outputDir.listFiles(CAT_FILTER);
        List<String> rval = new ArrayList<>();
        for (File catDir : catDirs) {
            File[] files = catDir.listFiles(BUFR_FILTER);
            for (File f : files) {
                rval.add(f.getAbsolutePath());
            }
        }

        // Make sure to call BufrSplitter2.exit() to close writers
        splitter.exit();

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

        for (File catDir : outputDir.listFiles(CAT_FILTER)) {
            for (File f : catDir.listFiles(BUFR_FILTER)) {
                if (!f.delete() && f.exists()) {
                    if (!rmDir(f) && f.exists()) {
                        log.error("Unable to clean up temporary BUFR file: " + f);
                    }
                }
            }
            try {
                Files.delete(catDir.toPath());
            } catch (IOException e) {
                log.error(
                        "Unable to clean up temporary BUFR Category directory: "
                                + catDir,
                        e);
            }
        }
        try {
            Files.delete(outputDir.toPath());
        } catch (IOException e) {
            log.error("Unable to clean up temporary BUFR Category directory: "
                    + outputDir, e);
        }
    }
}
