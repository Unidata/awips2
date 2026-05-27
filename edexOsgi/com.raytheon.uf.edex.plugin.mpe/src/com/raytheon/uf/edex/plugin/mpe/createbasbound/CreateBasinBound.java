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
package com.raytheon.uf.edex.plugin.mpe.createbasbound;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.mpe.constants.FilePermissionConstants;
import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.uf.common.mpe.util.AppsDefaultsPathException;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.ohd.AppsDefaultsDirKeys;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Create Basin Bound
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 31, 2016 5856       skorolev    Initial creation
 * Aug 07, 2017 6334       bkowal      Directories are now created with 770 permissions and files 660.
 *
 * </pre>
 *
 * @author skorolev
 */
public class CreateBasinBound {
    private final static Logger logger = LoggerFactory
            .getLogger(CreateBasinBound.class);

    private AppsDefaults appsDefaults = AppsDefaults.getInstance();

    private static CreateBasinBound instance;

    private Path hostPath;

    private static final String ASCIISTR = "ascii";

    private static final String ASCII_FOLDER = IPathManager.SEPARATOR + ASCIISTR
            + IPathManager.SEPARATOR;

    private static final String BIN_FOLDER = IPathManager.SEPARATOR + "binary"
            + IPathManager.SEPARATOR;

    private static final String ASCII_MAP_ORIG = ASCII_FOLDER
            + "map_basin.orig";

    private static final String ASCII_MAP_BAS = ASCII_FOLDER + "map_basin.dat";

    private static final String BIN_MAP_BAS = BIN_FOLDER + "map_basin.bin";

    private static final String IFP_RFC = "ifp_rfc";

    /**
     * Constructor
     * 
     * @throws EdexException
     */
    public CreateBasinBound() {
        try {
            Path geoPath = AppsDefaultsConversionWrapper
                    .getPathForToken(AppsDefaultsDirKeys.GEO_DATA);
            StringBuffer sb = new StringBuffer(geoPath.toString());
            sb.append(File.separatorChar).append(appsDefaults.getToken(IFP_RFC))
                    .append(File.separatorChar);
            setHostPath(Paths.get(sb.toString()));
        } catch (AppsDefaultsPathException e) {
            logger.error("Error getting host path.", e);
        }
    }

    public static CreateBasinBound getInstance() {
        if (instance == null) {
            instance = new CreateBasinBound();
        }
        return instance;
    }

    /**
     * Default: Creates binary file from ascii file
     * 
     * @param datFileName
     * @param binFileName
     * @throws AppsDefaultsPathException
     * @throws IOException
     * @throws EdexException
     */
    public void createFiles(String datFileName, String binFileName)
            throws AppsDefaultsPathException, IOException, EdexException {
        // get host path
        if (getInstance().hostPath != null) {

            String asciifName = buildFilename(ASCII_FOLDER + datFileName);
            String binfName = buildFilename(BIN_FOLDER + binFileName);
            File binDir = new File(buildFilename(BIN_FOLDER));
            if (!binDir.exists()) {
                com.raytheon.uf.common.util.file.Files.createDirectories(
                        binDir.toPath(),
                        FilePermissionConstants.POSIX_DIRECTORY_ATTRIBUTES);
            }
            CreateBasBoundBin.createBinFile(asciifName, binfName);
        } else {
            throw new EdexException("Host path is Null.");
        }
    }

    /**
     * Takes the output from an NWSRFS run (@DUMP PUNCH BASIN) processes one
     * basin at a time and writes information to an ascii file. Later call to
     * function create_bas_bound_bin reads the ascii file and creates the binary
     * file in a standard binary format for RFC applications.
     * 
     * @param datFileName
     * @throws AppsDefaultsPathException
     * @throws IOException
     * @throws EdexException
     */
    public void createFiles(String datFileName)
            throws AppsDefaultsPathException, IOException, EdexException {

        // get host path
        if (getInstance().hostPath != null) {

            String asciifName = buildFilename(ASCII_MAP_BAS);
            String binfName = buildFilename(BIN_MAP_BAS);

            if (ASCIISTR.equals(datFileName)) {
                // datFileName=="ascii".
                asciifName = buildFilename(ASCII_MAP_BAS);
                logger.info("ascii file name={}", asciifName);
                binfName = buildFilename(BIN_MAP_BAS);
                logger.info("bin file name={}", binfName);
            } else {
                String nwsrfsBasinFileName = "";
                // name of datFileName is not an "ascii"
                if (datFileName.charAt(0) == File.pathSeparatorChar) {
                    /*
                     * datFileName is a filename for the raw punch from NWSRFS
                     * (can be full path name)
                     */
                    nwsrfsBasinFileName = datFileName;
                    logger.info(" 1  nwsrfs_basin_fname = {}",
                            nwsrfsBasinFileName);
                } else {
                    nwsrfsBasinFileName = getHostPath().toString()
                            + ASCII_FOLDER + datFileName;
                    logger.info(" 2  nwsrfs_basin_fname = {}",
                            nwsrfsBasinFileName);
                }
                logger.info("ascii file name = {}", asciifName);
                logger.info("bin file name = {}", binfName);
                try {
                    CreateBasBoundAscii.createAsciiFile(nwsrfsBasinFileName,
                            asciifName);
                } catch (IOException e) {
                    throw new EdexException("Error creating bin file from "
                            + nwsrfsBasinFileName, e);
                }
            }
            CreateBasBoundBin.createBinFile(asciifName, binfName);
        } else {
            throw new EdexException("Host path is Null.");
        }
    }

    /**
     * Creates map_basin binary file.
     * 
     * @throws EdexException
     */
    public void createFiles() throws EdexException {
        // get host path
        if (getInstance().hostPath != null) {

            String nwsrfsBasinFileName = buildFilename(ASCII_MAP_ORIG);
            String asciifName = buildFilename(ASCII_MAP_BAS);
            String binfName = buildFilename(BIN_MAP_BAS);
            /*
             * That assumes existance of ascii original file (in proper format)
             * then only calls routine to create binary file from the ascii one.
             */
            try {
                CreateBasBoundAscii.createAsciiFile(nwsrfsBasinFileName,
                        asciifName);
                CreateBasBoundBin.createBinFile(asciifName, binfName);
            } catch (IOException e) {
                throw new EdexException(
                        "Error creating bin file from " + nwsrfsBasinFileName,
                        e);
            }
        } else {
            throw new EdexException("Host path is Null.");
        }
    }

    /**
     * Combines host path and a file together, and returns a full path string as
     * a result.
     * 
     * @param file
     * @return full file path
     */
    private String buildFilename(String file) {
        return this.hostPath.toString() + file;
    }

    public Path getHostPath() {
        return this.hostPath;
    }

    public void setHostPath(Path hostPath) {
        this.hostPath = hostPath;
    }
}
