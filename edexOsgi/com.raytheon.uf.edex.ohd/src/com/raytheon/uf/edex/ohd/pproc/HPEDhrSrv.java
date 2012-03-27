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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.ohd.MainMethod;

/**
 * Service implementation for decoding DHR radar files.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 20, 2010  4200       snaples    Initial creation
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */
public class HPEDhrSrv {

    private Log logger = LogFactory.getLog(getClass());

    private AppsDefaults appsDefaults = AppsDefaults.getInstance();

    private CoreDao dao;

    private File outFile;

    private static final Pattern WMO_PATTERN = Pattern
            .compile("([A-Z]{4}[0-9]{2} [A-Z]{4} [0-9]{6})\\x0D\\x0D\\x0A(\\w{6})\\x0D\\x0D\\x0A");

    /** WMO header regex */
    // private static final Pattern wmoPat = Pattern
    // .compile("(\\p{Alpha}{4}\\d{2}) (\\p{Alnum}{4}) (\\d{6})");

    private static final Pattern ldmPat = Pattern
            .compile("(\\p{Alpha}{3})(\\p{Alpha}{3})");

    private static final Pattern radPat = Pattern
            .compile("(\\p{Alpha}{4}).(\\d{2,3}).(\\d{8}_\\d{4})");

    private final String DHR = "DHR";

    private final String dhrtype = "32";

    private final String dsptype1 = "138";

    private final String dsptype2 = "80";

    private final String DSP = "DSP";

    private final String STP = "STP";

    public Object process(File hpeFile) throws EdexException {
        // logger.info("Starting HPE Check message.");
        checkMessage(hpeFile);

        return null;
    }

    /**
     * 
     * @param dhrFile
     *            The radar server message
     * @throws EdexException
     *             If IOExceptions occur
     */
    private void checkMessage(File hpeFile) throws EdexException {

        boolean proc = false;
        // logger.info("Starting HPE CheckFile. ");
        proc = checkFile(hpeFile);
        if (proc == false) {
            return;
        }
        // logger.info("Finished HPE CheckFile. ");
        // logger.info("Starting HPE DSP gather.");
        int exitValue = MainMethod.runProgram("ksh",
                appsDefaults.getToken("pproc_bin") + "/DSPgather");
        // Output result
        if (exitValue == 0) {
            logger.info("HPE DSP gather was run ");
        } else {
            logger.error("HPE DSP gather did not run successfully and terminated with exit code: "
                    + exitValue);
        }
        // logger.info("Starting HPE DHR gather script. ");
        exitValue = MainMethod.runProgram("ksh",
                appsDefaults.getToken("pproc_bin") + "/DHRgather");

        // Output result
        if (exitValue == 0) {
            logger.info("HPE DHR gather was run ");
        } else {
            logger.error("HPE DHR gather did not run successfully and terminated with exit code: "
                    + exitValue);
        }
    }

    /**
     * DPA radar files have been known to contain extra bytes at the beginning
     * of the file. These bytes will cause the DHR decoder to fail.
     * <p>
     * This method removes the leading bytes to ensure proper decoding of DHR
     * files
     * 
     * @param fileName
     *            The name of the DHR radar file
     * @throws EdexException
     *             If IOExceptions occur
     */
    private boolean checkFile(File hpeFile) throws EdexException {
        /*
         * Read the contents of the file into memory.
         */
        BufferedInputStream inStream = null;
        try {
            inStream = new BufferedInputStream(new FileInputStream(hpeFile));
        } catch (FileNotFoundException e) {
            throw new EdexException("HPE Cannot find file: " + hpeFile, e);
        }
        byte[] fileContents = new byte[(int) hpeFile.length()];
        try {
            inStream.read(fileContents);
        } catch (IOException e) {
            throw new EdexException("HPE Error reading file: " + hpeFile, e);
        } finally {
            try {
                inStream.close();
            } catch (IOException e1) {
                throw new EdexException("HPE Error closing stream to file: "
                        + hpeFile, e1);
            }
        }
        String outPath = "";
        String fname = hpeFile.getName();
        String radid = "";
        String dtype = "";
        String dt = "";
        String outname = "";

        Matcher r = radPat.matcher(fname);
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
        Matcher wmomat = WMO_PATTERN.matcher(fileStartStr);
        Matcher dhrmat = ldmPat.matcher(fileStartStr);

        if (r.find()) {
            // logger.info("HPE DHRSrv found Radar file.");
            radid = r.group(1).substring(1).toUpperCase().trim();
            String ftype = r.group(2);
            dt = r.group(3);
            if (ftype.equals(dhrtype)) {
                dtype = DHR;
                // logger.info("HPE DHRSrv found Radar file type DHR.");
            } else if (ftype.equals(dsptype1) || ftype.equals(dsptype2)) {
                dtype = DSP;
                // logger.info("HPE DHRSrv found Radar file type DSP.");
            } else {
                // logger.info("HPE DHRSrv found Radar type unknown.");
                return false;
            }
        } else if (wmomat.find()) {
            if (dhrmat.find()) {
                // logger.info("HPE DHRSrv found LDM file.");
                dt = wmomat.group(1).substring(wmomat.group(1).length() - 6);
                radid = dhrmat.group(2).toUpperCase().trim();
                dtype = dhrmat.group(1).toUpperCase().trim();
                if (!dtype.equals(DSP) || !dtype.equals(STP)
                        || !dtype.equals(DHR)) {
                    return false;
                }
            } else {
                return false;
            }
        } else {
            return false;
        }
        // logger.info("HPE DHRSrv querying db for radid=" + radid
        // + " and use_radar=T.");
        String query = String.format(
                "select * from radarloc where radid='%s' and use_radar='T' ",
                radid);
        dao = new CoreDao(DaoConfig.forDatabase("ihfs"));
        Object[] rs = dao.executeSQLQuery(query);

        // logger.info("HPE DHRSrv querying db done.");
        if (rs.length > 0) {

            outname = dtype + radid + "." + dt;
            if (dtype.equals(DHR)) {
                outPath = appsDefaults.getToken("dhr_prod_dir");
            } else {
                outPath = appsDefaults.getToken("dsp_prod_dir");
            }
            File oP = new File(outPath);
            if (!oP.exists()) {
                oP.mkdir();
            }

            int offset = 0;
            offset = findStartRadarData(fileStartStr);
            outFile = new File(FileUtil.join(outPath, outname));
            BufferedOutputStream outStream = null;

            try {
                outStream = new BufferedOutputStream(new FileOutputStream(
                        outFile));
            } catch (FileNotFoundException e) {
                throw new EdexException("HPE Cannot find file: " + outFile, e);
            }

            try {
                outStream.write(fileContents, offset, fileContents.length
                        - offset);
                // logger.info("HPE Re-writing contents of file: " + hpeFile
                // + " to " + outFile);
            } catch (IOException e) {
                throw new EdexException(
                        "HPE Error writing updated contents of HPE file: "
                                + outFile, e);
            } finally {
                try {
                    outStream.close();
                } catch (IOException e1) {
                    throw new EdexException("HPE Unable to close file: "
                            + outFile, e1);
                }
            }
            return true;
        } else {
            return false;
        }
    }

    private int findStartRadarData(String headerInfo) {
        int startOfRadarData = 0;
        Matcher matcher = WMO_PATTERN.matcher(headerInfo);
        boolean foundHeader = matcher.find();
        if (foundHeader) {
            startOfRadarData = matcher.end();
        }

        return startOfRadarData;
    }

    public static final void main(String[] args) {

        String input = "SDUS53_ABR_DSP_201538_185076111.rad";
        String input2 = "koax.32.20111020_1553";
        Matcher m = ldmPat.matcher(input);
        Matcher p = radPat.matcher(input2);

        if (m.find()) {
            System.out.println(m.group(0));
            System.out.println(m.group(2));
            System.out.println(m.group(3));
            System.out.println(m.group(4) + "_" + m.group(5));
            System.out.println(m.group(3) + m.group(2) + "." + m.group(4) + "_"
                    + m.group(5));
        }
        if (p.find()) {

            System.out.println(p.group(0));
            System.out.println(p.group(1).substring(1).toUpperCase());
            System.out.println(p.group(2));
            if (p.group(2).equals("32")) {
                System.out.println("DHR");
                System.out.println("DHR"
                        + p.group(1).substring(1).toUpperCase() + "."
                        + p.group(3));
            } else {
                System.out.println("DSP");
                System.out.println("DSP"
                        + p.group(1).substring(1).toUpperCase() + "."
                        + p.group(3));
            }
        }

    }

}
