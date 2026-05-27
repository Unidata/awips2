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
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Path;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.mpe.constants.FilePermissionConstants;
import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.file.IOPermissionsHelper;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.ohd.MainMethod;
import com.raytheon.uf.edex.plugin.mpe.gather.dhr.DHRGather;

/**
 * Service implementation for decoding DHR radar files.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 20, 2010 4200       snaples     Initial creation
 * Mar 09, 2012 417        dgilling    Refactor to use two-stage queue
 *                                     process.
 * Mar 20, 2013 1804       bsteffen    Switch all radar decompressing to be in
 *                                     memory.
 * Sep 13, 2013 2368       rjpeter     Updated to use durable jms settings.
 * Jun 08, 2016 4624       skorolev    Added parallelExecEnabled key.
 * Jun 24, 2016 4625       bkowal      Fix so that the DHRgather script is ran instead
 *                                     of running DSPgather twice.
 * Jun 30, 2016 4625       bkowal      Execute the new {@link DHRGather} when parallel
 *                                     execution mode is enabled.
 * Jul 12, 2016 4619       bkowal      Moved {@link AppsDefaultsConversionWrapper} to common.
 * Jan 10, 2016 6058       bkowal      Removed direct usage of the Java-based DHRGather.
 * Aug 02, 2017 6334       bkowal      Write DHR/DSP gather files with the correct permissions.
 * Aug 07, 2017 6334       bkowal      Directories are now created with 770 permissions and files 660.
 * Jul 19, 2018 5588       mapeters    More fully separate legacy/java decode paths (e.g. use separate
 *                                     gather dirs), use separate JMS uris for DHR/DSP in java path
 *
 *
 * </pre>
 *
 * @author snaples
 */
public class HPEDhrSrv {

    private static final transient IUFStatusHandler logger = UFStatus
            .getHandler(HPEDhrSrv.class);

    private static final Pattern WMO_PATTERN = Pattern.compile(
            "([A-Z]{4}[0-9]{2} [A-Z]{4} [0-9]{6})\\x0D\\x0D\\x0A(\\w{6})\\x0D\\x0D\\x0A");

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

    private static final String DHR_JMS_QUEUE_URI = "jms-durable:queue:dhrProcess";

    private static final String DSP_JMS_QUEUE_URI = "jms-durable:queue:dspProcess";

    private static final String LEGACY_DHR_DSP_JMS_QUEUE_URI = "jms-durable:queue:legacyDhrDspProcess";

    private final AppsDefaults appsDefaults = AppsDefaults.getInstance();

    public static final String DHR_PROD_DIR = "dhr_prod_dir";

    public static final String DSP_PROD_DIR = "dsp_prod_dir";

    public static final String PPROC_BIN = "pproc_bin";

    public static final String SQL = "select * from radarloc where radid='%s' and use_radar='T' ";

    public static final String DB = "ihfs";

    public static final String INGEST_FILENAME = "ingestfilename";

    public static final String DSP_GATHER = "DSPgather";

    public static final String DHR_GATHER = "DHRgather";

    public static final String KSH = "ksh";

    /**
     * Route endpoint for "legacyDhrDspIngestRoute". Takes a message, writes the
     * file to disk, and then runs the DHR data processing scripts so the file
     * is ingested. This is everything necessary for legacy processing of the
     * given message.
     *
     * @param message
     *            A <code>HPEDhrMessage</code> describing the DHR radar file to
     *            be ingested.
     */
    public void legacyProcess(HPEDhrMessage message) {
        if (writeFile(message, true)) {
            runLegacyGatherScripts();
        }
    }

    /**
     * Route endpoint for "dhrIngestRoute" and "dspIngestRoute". Takes a message
     * and writes the file to disk so that it can be used by DHR/DSP Gather.
     *
     * @param message
     *            A <code>HPEDhrMessage</code> describing the DHR radar file to
     *            be ingested.
     * @return true if the file was successfully written, false otherwise
     */
    public boolean prepareForGather(HPEDhrMessage message) {
        return writeFile(message, false);
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
     * @param legacy
     *            whether to write the file to the legacy product dir or the
     *            Java-based product dir
     * @return true if the file was successfully written, false otherwise
     */
    private boolean writeFile(HPEDhrMessage message, boolean legacy) {
        String dtype = message.getDtype();
        String outname = dtype + message.getRadarId() + "." + message.getDt();

        String dirToken = DHR.equals(dtype) ? DHR_PROD_DIR : DSP_PROD_DIR;
        String outPath;
        if (legacy) {
            outPath = appsDefaults.getToken(dirToken);
        } else {
            Path path = AppsDefaultsConversionWrapper
                    .getPathForTokenWithoutCreating(dirToken);
            outPath = path.toAbsolutePath().toString();
        }

        try {
            File oP = new File(outPath);
            if (!oP.exists()) {
                com.raytheon.uf.common.util.file.Files.createDirectories(
                        oP.toPath(),
                        FilePermissionConstants.POSIX_DIRECTORY_ATTRIBUTES);
            }

            byte[] fileContents = message.getData();
            int offset = findStartRadarData(new String(fileContents, 0, 80));
            String outFile = FileUtil.join(outPath, outname);
            try (OutputStream os = IOPermissionsHelper.getOutputStream(
                    new File(outFile).toPath(),
                    FilePermissionConstants.POSIX_FILE_SET);
                    BufferedOutputStream bos = new BufferedOutputStream(os)) {
                bos.write(fileContents, offset, fileContents.length - offset);
            }
            return true;
        } catch (FileNotFoundException e) {
            logger.warn("HPE cannot create output file.", e);
        } catch (IOException e) {
            logger.warn("HPE Error writing updated contents of HPE file", e);
        }
        return false;
    }

    /**
     * Route endpoint for "dhrDspIngestFilter". Reads the given file to memory
     * and determines whether or not this file is a DHR/DSP radar file. If it
     * is, a message is sent to a JMS queue so it is later ingested.
     *
     * @param fileContents
     *            The radar file content
     * @param headers
     */
    public void filter(byte[] fileContents, Headers headers) {
        if (fileContents.length < 80) {
            return;
        }

        File hpeFile = new File(headers.get(INGEST_FILENAME).toString());

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
                                + hpeFile.toString(),
                        e);
            } catch (EdexException e) {
                logger.handle(Priority.PROBLEM,
                        "HPE can't send message to QPID queue for file: "
                                + hpeFile.toString(),
                        e);
            }
        }
    }

    /**
     * Takes the given parameters and constructs a <code>HPEDhrMessage</code> to
     * be placed onto the appropriate queues used by <code>HPEDhrSrv</code> for
     * actual data processing. If parallel exec is enabled, the file will be
     * placed both on the legacy DHR/DSP queue, and the product-type-specific
     * queue for Java processing. Otherwise, it will just be placed on the
     * legacy queue.
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
        EDEXUtil.getMessageProducer().sendAsyncUri(LEGACY_DHR_DSP_JMS_QUEUE_URI,
                messageData);
        if (AppsDefaultsConversionWrapper.parallelExecEnabled()) {
            String jmsQueueUri = DHR.equals(dtype) ? DHR_JMS_QUEUE_URI
                    : DSP_JMS_QUEUE_URI;
            EDEXUtil.getMessageProducer().sendAsyncUri(jmsQueueUri,
                    messageData);
        }
    }

    /**
     * Runs the DSPgather and DHRgather data processing scripts.
     */
    private void runLegacyGatherScripts() {
        int exitValue = MainMethod.runProgram(KSH,
                appsDefaults.getToken(PPROC_BIN) + File.separatorChar
                        + DSP_GATHER);
        // Output result
        if (exitValue == 0) {
            logger.info("HPE DSP gather was run ");
        } else {
            logger.error(
                    "HPE DSP gather did not run successfully and terminated with exit code: "
                            + exitValue);
        }

        exitValue = MainMethod.runProgram(KSH, appsDefaults.getToken(PPROC_BIN)
                + File.separatorChar + DHR_GATHER);
        // Output result
        if (exitValue == 0) {
            logger.info("HPE DHR gather was run ");
        } else {
            logger.error(
                    "HPE DHR gather did not run successfully and terminated with exit code: "
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
            // getting the radid
            metadata[RADID_IDX] = r.group(1).substring(1).toUpperCase().trim();
            String ftype = r.group(2);
            // getting the dt
            metadata[DT_IDX] = r.group(3);
            // getting the dtype
            if (ftype.equals(DHRTYPE)) {
                metadata[DTYPE_IDX] = DHR;
            } else if (ftype.equals(DSPTYPE1) || ftype.equals(DSPTYPE2)) {
                metadata[DTYPE_IDX] = DSP;
            } else {
                return false;
            }
        } else if (wmomat.find()) {
            Matcher dhrmat = LDM_PATTERN.matcher(fileHeader);
            if (dhrmat.find()) {
                metadata[DT_IDX] = wmomat.group(1)
                        .substring(wmomat.group(1).length() - 6);
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
        String query = String.format(SQL, metadata[RADID_IDX]);
        CoreDao dao = new CoreDao(DaoConfig.forDatabase(DB));
        Object[] rs = dao.executeSQLQuery(query);

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
}
