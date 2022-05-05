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

package com.raytheon.uf.edex.ohd.pproc;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Date;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.mpe.constants.FilePermissionConstants;
import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.file.IOPermissionsHelper;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.ohd.MainMethod;
import com.raytheon.uf.edex.plugin.mpe.dpa.DecodeDPA;
import com.raytheon.uf.edex.plugin.mpe.dpa.DecodeDPAException;

/**
 * Service implementation for decoding DPA radar files.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 14, 2008            bphillip    Initial creation
 * Mar 20, 2013 1804       bsteffen    Switch all radar decompressing to be in
 *                                     memory.
 * Mar 28, 2014   2952     mpduff      Changed to use UFStatus for logging.
 * Apr 10, 2014   2675     mpduff      Removed call to GAFF.
 * Nov 05, 2015  15045     snaples     Removed file name and setup to use script to 
 *                                     run decoder on all radar products in gather directory.
 * Jul 20, 2016   4622     jschmid     Added instance of DecodeDPA to also decode DPA from Java.
 * Nov 03, 2016   4622     skorolev    Added info for parallel execution.
 * Aug 07, 2017   6334     bkowal      Directories are now created with 770 permissions and files 660.
 * 
 * </pre>
 * 
 * @author bphillip
 */
public class DecodeDpaSrv {

    /** WMO header regex */
    private static final Pattern wmoPat = Pattern
            .compile("(\\p{Alpha}{4}\\d{2}) (\\p{Alnum}{4}) (\\d{6})");

    private static final Pattern dpaPat = Pattern.compile("DPA...");

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(DecodeDpaSrv.class);

    private final AppsDefaults appsDefaults = AppsDefaults.getInstance();

    private DecodeDPA dpaDecoder;

    private File outFile;

    private CoreDao dao;

    public Object process(byte[] message, Headers headers)
            throws EdexException {

        boolean proc = false;
        File ingestFile = new File(headers.get("ingestfilename").toString());
        proc = checkFile(message, ingestFile.getName());
        if (proc == false) {
            return null;
        }
        try {
            dpaDecoder = new DecodeDPA();
        } catch (DecodeDPAException e) {
            logger.error("Could not create DecodeDPA-type decoder instance.",
                    e);
            dpaDecoder = null;
        }

        if ((null != dpaDecoder)
                && AppsDefaultsConversionWrapper.parallelExecEnabled()) {
            logger.info("DecodeDPA is running in parallel mode.");
            try {
                dpaDecoder.decodeRecord(ingestFile.getName());
            } catch (DecodeDPAException e) {
                logger.error("DecodeDPA, error decoding file: "
                        + ingestFile.getName() + ". ", e);
            }
        }

        String path = appsDefaults.getToken("pproc_bin");
        int exitValue = MainMethod.runProgram("ksh", path + "/Run_DecodeDPA");

        // Output result
        if (exitValue == 0) {
            logger.info("Dpa gather was run successfully.");
        } else {
            logger.error(
                    "DPA gather failed and the process terminated with exit code: "
                            + exitValue);
        }

        return null;
    }

    /**
     * DPA radar files have been known to contain extra bytes at the beginning
     * of the file. These bytes will cause the DPA decoder to fail.
     * <p>
     * This method removes the leading bytes to ensure proper decoding of DPA
     * files
     * 
     * @param fileName
     *            The name of the DPA radar file
     * @throws EdexException
     *             If IOExceptions occur
     */
    private boolean checkFile(byte[] fileContents, String fileName)
            throws EdexException {

        /*
         * Copy off the first few bytes to see if leading bytes are present
         * before the WMO header
         */
        if (fileContents.length < 80) {
            return false;
        }
        String fileStartStr = new String(fileContents, 0, 80);

        /*
         * Find the WMO header
         */
        Matcher wmomat = wmoPat.matcher(fileStartStr);
        Matcher dpamat = dpaPat.matcher(fileStartStr);
        String outPath = appsDefaults.getToken("dpa_gather");
        File oP = new File(outPath);
        if (!oP.exists()) {
            try {
                com.raytheon.uf.common.util.file.Files.createDirectories(
                        oP.toPath(),
                        FilePermissionConstants.POSIX_DIRECTORY_ATTRIBUTES);
            } catch (IOException e) {
                throw new EdexException(
                        "Failed to create DPA Gather directory: " + outPath
                                + ".",
                        e);
            }
        }

        if (wmomat.find()) {
            if (dpamat.find()) {
                String radarid = dpamat.group(0).substring(3).toUpperCase();
                if (radarid == null) {
                    return false;
                }
                String query = String.format(
                        "select * from radarloc where radid='%s' and use_radar='T' ",
                        radarid);
                dao = new CoreDao(DaoConfig.forDatabase("ihfs"));
                Object[] rs = dao.executeSQLQuery(query);
                if (rs.length > 0) {
                    /*
                     * If leading bytes exist, rewrite the file without them.
                     */
                    int offset = fileStartStr.indexOf(wmomat.group());
                    if (offset != 0) {
                        outFile = new File(FileUtil.join(outPath, fileName));
                        try (OutputStream os = IOPermissionsHelper
                                .getOutputStream(outFile.toPath(),
                                        FilePermissionConstants.POSIX_FILE_SET);
                                BufferedOutputStream outStream = new BufferedOutputStream(
                                        os)) {
                            logger.info("Re-writing contents of file: "
                                    + fileName + " to " + outFile);
                            outStream.write(fileContents, offset,
                                    fileContents.length - offset);
                        } catch (IOException e) {
                            throw new EdexException(
                                    "Error writing updated contents of DPA file: "
                                            + outFile,
                                    e);
                        }
                    }
                    return true;
                } else {
                    return false;
                }
            } else {
                return false;
            }
        } else {
            String radarid = fileName.substring(1, 4).toUpperCase();
            String query = String.format(
                    "select * from radarloc where radid='%s' and use_radar='T' ",
                    radarid);
            dao = new CoreDao(DaoConfig.forDatabase("ihfs"));
            Object[] rs = dao.executeSQLQuery(query);
            if (rs.length > 0) {
                outFile = new File(FileUtil.join(outPath, fileName));
                logger.info("No header found for file: " + outFile
                        + " decoding with filename.");
                try {
                    FileUtil.bytes2File(fileContents, outFile);
                    IOPermissionsHelper.applyFilePermissions(outFile.toPath(),
                            FilePermissionConstants.POSIX_FILE_SET);
                } catch (IOException e) {
                    throw new EdexException(
                            "Error copying file to destination directory: "
                                    + outFile,
                            e);
                }
                return true;
            } else {
                return false;
            }
        }
    }

    public static final void main(String[] args) {

        String input = "SDUS53 KABR 061713 CCA\r\r\nDPAABR\r\r\nSTUFF MORE STUFF";
        String ibuf = "ALNP1JDN  XXX  -1  12";
        String dhrin = "DHRABR";
        // String ibuf = "105 105 -1 25 40.10000 98.14803";
        Scanner s = new Scanner(ibuf);
        String regx = "^([0-9A-Z]{3,})\\s*(.*?)\\s+(\\d{1,})";
        String dhrpat = "(\\p{Alpha}{3})(\\p{Alpha}{3})";
        Pattern pp = Pattern.compile(regx);
        Matcher mt = pp.matcher(ibuf);
        Pattern ph = Pattern.compile(dhrpat);
        Matcher mh = ph.matcher(dhrin);

        if (mh.find()) {

            System.out.println("Found " + mh.group(1).trim()
                    + " data for radar: " + mh.group(2).trim());
            System.out.println("Timestamp:" + new Date().getTime());
        }

        if (mt.find()) {
            System.out.println("Basin ID is: " + mt.group(1));
            System.out.println("Group 2 contains: " + mt.group(2));
            System.out.println("Number of points in basin: " + mt.group(3));
        }
        Matcher m = wmoPat.matcher(input);
        Matcher p = dpaPat.matcher(input);

        if (m.find()) {
            System.out.println(m.group(0));
            if (p.find()) {
                System.out.println(p.group(0));
            }
        }

    }
}
