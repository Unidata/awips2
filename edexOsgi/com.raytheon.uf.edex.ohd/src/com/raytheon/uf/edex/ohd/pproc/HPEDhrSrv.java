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
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
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
 * Mar 09, 2012  417        dgilling   Refactor to use two-stage queue
 *                                     process.
 * Mar 20, 2013 1804       bsteffen    Switch all radar decompressing to be in
 *                                     memory.
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */
public class HPEDhrSrv {

    private static final transient IUFStatusHandler logger = UFStatus
            .getHandler(HPEDhrSrv.class);

    private static final Pattern WMO_PATTERN = Pattern
            .compile("([A-Z]{4}[0-9]{2} [A-Z]{4} [0-9]{6})\\x0D\\x0D\\x0A(\\w{6})\\x0D\\x0D\\x0A");

    private static final Pattern LDM_PATTERN = Pattern
            .compile("(\\p{Alpha}{3})(\\p{Alpha}{3})");

    private static final Pattern RAD_PATTERN = Pattern
            .compile("(\\p{Alpha}{4}).(\\d{2,3}).(\\d{8}_\\d{4})");

    private static final String DHRTYPE = "32";

    private static final String DSPTYPE1 = "138";

    private static final String DSPTYPE2 = "80";

    private static final String DHR = "DHR";

    private static final String DSP = "DSP";

    private static final String STP = "STP";

    private static final int RADID_IDX = 0;

    private static final int DTYPE_IDX = 1;

    private static final int DT_IDX = 2;

    private static final String JMS_QUEUE_URI = "jms-generic:queue:dhrProcess";

    private AppsDefaults appsDefaults = AppsDefaults.getInstance();

    /**
     * Route endpoint for "dhrIngestRoute". Takes a message, writes the file to
     * disk, and then runs the DHR data processing scripts so the file is
     * ingested.
     * 
     * @param message
     *            A <code>HPEDhrMessage</code> describing the DHR radar file to
     *            be ingested.
     */
    public void process(HPEDhrMessage message) {
        // logger.info("Starting HPE process message.");
        try {
            writeFile(message);
        } catch (FileNotFoundException e) {
            logger.handle(Priority.PROBLEM, "HPE cannot create output file.", e);
            return;
        } catch (IOException e) {
            logger.handle(Priority.PROBLEM,
                    "HPE Error writing updated contents of HPE file", e);
            return;
        }
        runGatherScripts();
    }

    /**
     * DPA radar files have been known to contain extra bytes at the beginning
     * of the file. These bytes will cause the DHR decoder to fail.
     * <p>
     * This method removes the leading bytes to ensure proper decoding of DHR
     * files.
     * 
     * @param message
     *            A <code>HPEDhrMessage</code> describing the DHR radar file to
     *            be ingested.
     * @throws FileNotFoundException
     *             If the output file cannot be created or opened for any
     *             reason.
     * @throws IOException
     *             If an I/O error occurs while writing the file.
     */
    private void writeFile(HPEDhrMessage message) throws FileNotFoundException,
            IOException {
        String dtype = message.getDtype();
        String outname = dtype + message.getRadarId() + "." + message.getDt();
        String outPath;
        if (dtype.equals(DHR)) {
            outPath = appsDefaults.getToken("dhr_prod_dir");
        } else {
            outPath = appsDefaults.getToken("dsp_prod_dir");
        }
        File oP = new File(outPath);
        if (!oP.exists()) {
            oP.mkdirs();
        }

        byte[] fileContents = message.getData();
        int offset = findStartRadarData(new String(fileContents, 0, 80));
        String outFile = FileUtil.join(outPath, outname);
        BufferedOutputStream outStream = null;

        try {
            outStream = new BufferedOutputStream(new FileOutputStream(outFile));
            // logger.info("HPE Writing contents of file to " +
            // outFile);
            outStream.write(fileContents, offset, fileContents.length - offset);
        } finally {
            if (outStream != null) {
                outStream.close();
            }
        }
    }

    /**
     * Route endpoint for "dhrIngestFilter". Reads the given file to memory and
     * determines whether or not this file is a DHR radar file. If it is, a
     * message is sent to a JMS queue so it is later ingested.
     * 
     * @param hpeFile
     *            The radar file to check.
     */
    public void filter(byte[] fileContents, Headers headers) {
        // logger.info("Starting HPE Check message.");

        if (fileContents.length < 80) {
            return;
        }

        File hpeFile = new File(headers.get("ingestfilename").toString());

        // check header
        String fileStartStr = new String(fileContents, 0, 80);
        // array will hold radar id, dtype, and dt information. using array so
        // its contents can be modified by the checkFile() method.
        String[] fileMetadata = new String[3];
        boolean validFile = checkFile(hpeFile.getName(), fileStartStr,
                fileMetadata);

        // write file to queue
        if (validFile) {
            try {
                sendFileToQueue(fileContents, fileMetadata[RADID_IDX],
                        fileMetadata[DTYPE_IDX], fileMetadata[DT_IDX]);
            } catch (SerializationException e) {
                logger.handle(Priority.PROBLEM,
                        "HPE can't serialize HPEDhrMessage for file: "
                                + hpeFile.toString(), e);
            } catch (EdexException e) {
                logger.handle(Priority.PROBLEM,
                        "HPE can't send message to QPID queue for file: "
                                + hpeFile.toString(), e);
            }
        }

        // logger.info("Finished HPE CheckFile. ");
    }

    /**
     * Takes the given parameters and constructs a <code>HPEDhrMessage</code> to
     * be placed onto the queue used by <code>HPEDhrSrv</code> for actual data
     * processing.
     * 
     * @param data
     *            The contents of the radar file.
     * @param radid
     *            The radar id for the given file.
     * @param dtype
     *            The file's dtype.
     * @param dt
     *            The file's dt.
     * @throws SerializationException
     *             If the constructed <code>HPEDhrMessage</code> cannot be
     *             serialized by Thrift.
     * @throws EdexException
     *             If the message cannot be placed onto the QPID queue for DHR
     *             data processing.
     */
    private void sendFileToQueue(byte[] data, String radid, String dtype,
            String dt) throws SerializationException, EdexException {
        HPEDhrMessage message = new HPEDhrMessage(data, radid, dtype, dt);
        byte[] messageData = SerializationUtil.transformToThrift(message);
        EDEXUtil.getMessageProducer().sendAsyncUri(JMS_QUEUE_URI, messageData);
    }

    /**
     * Runs the DSPgather and DHRgather data processing scripts.
     */
    private void runGatherScripts() {
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
     * Given a radar file name and file header, this function determines whether
     * or not this file is a DHR radar file.
     * 
     * @param fileName
     *            The name of the radar file
     * @param fileHeader
     *            The first 80 bytes from the radar file.
     * @param metadata
     *            A length 3 <code>String</code> array that will be used to pass
     *            the radar id, dtype, and dt back to the caller.
     * @return If the specified file is a DHR radar file.
     */
    private boolean checkFile(String fileName, String fileHeader,
            String[] metadata) {
        /*
         * Find the WMO header
         */
        Matcher r = RAD_PATTERN.matcher(fileName);
        Matcher wmomat = WMO_PATTERN.matcher(fileHeader);

        if (r.find()) {
            // logger.info("HPE DHRSrv found Radar file.");
            // getting the radid
            metadata[RADID_IDX] = r.group(1).substring(1).toUpperCase().trim();
            String ftype = r.group(2);
            // getting the dt
            metadata[DT_IDX] = r.group(3);
            // getting the dtype
            if (ftype.equals(DHRTYPE)) {
                metadata[DTYPE_IDX] = DHR;
                // logger.info("HPE DHRSrv found Radar file type DHR.");
            } else if (ftype.equals(DSPTYPE1) || ftype.equals(DSPTYPE2)) {
                metadata[DTYPE_IDX] = DSP;
                // logger.info("HPE DHRSrv found Radar file type DSP.");
            } else {
                // logger.info("HPE DHRSrv found Radar type unknown.");
                return false;
            }
        } else if (wmomat.find()) {
            Matcher dhrmat = LDM_PATTERN.matcher(fileHeader);
            if (dhrmat.find()) {
                // logger.info("HPEDHRSrv found LDM file.");
                metadata[DT_IDX] = wmomat.group(1).substring(
                        wmomat.group(1).length() - 6);
                metadata[RADID_IDX] = dhrmat.group(2).toUpperCase().trim();
                metadata[DTYPE_IDX] = dhrmat.group(1).toUpperCase().trim();
                if ((!metadata[DTYPE_IDX].equals(DSP))
                        && (!metadata[DTYPE_IDX].equals(STP))
                        && (!metadata[DTYPE_IDX].equals(DHR))) {
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
                metadata[RADID_IDX]);
        CoreDao dao = new CoreDao(DaoConfig.forDatabase("ihfs"));
        Object[] rs = dao.executeSQLQuery(query);
        // logger.info("HPE DHRSrv querying db done.");

        return (rs.length > 0);
    }

    /**
     * Given the header data from a radar file, this function determines the
     * ending offset of the WMO header.
     * 
     * @param headerInfo
     *            The header data from a radar file.
     * @return Returns the offset after the last character in the WMO header.
     */
    private int findStartRadarData(String headerInfo) {
        Matcher matcher = WMO_PATTERN.matcher(headerInfo);
        if (matcher.find()) {
            return matcher.end();
        }

        return 0;
    }

    public static final void main(String[] args) {

        String input = "SDUS53_ABR_DSP_201538_185076111.rad";
        String input2 = "koax.32.20111020_1553";
        Matcher m = LDM_PATTERN.matcher(input);
        Matcher p = RAD_PATTERN.matcher(input2);

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
