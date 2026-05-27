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
package com.raytheon.uf.edex.dissemination;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.SeekableByteChannel;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;

import com.raytheon.messaging.mhs.MhsMessage;
import com.raytheon.messaging.mhs.MhsMessagePriority;
import com.raytheon.messaging.mhs.MhsSubmitException;
import com.raytheon.uf.common.dissemination.OUPDisseminatorObserver;
import com.raytheon.uf.common.dissemination.OUPResponse;
import com.raytheon.uf.common.dissemination.OfficialUserProduct;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.plugin.manualIngest.MessageGenerator;

/**
 * Java implementation of the Python script handleOUP.py. This code is
 * responsible for disseminating issued text products as well as submitting them
 * for local ingest and archiving.
 *
 * This is a straight port of handleOUP.py with minimal deviation from the
 * behavior of the Python code, even in cases where the Python code seems weird
 * and bad. Deviations deemed nontrivial are noted specifically in the comments.
 * If you see something that looks dumb, it's most likely that way because
 * handleOUP.py did it that way as of April 2025.
 *
 * Any functional changes to this Java code must also be made to handleOUP.py,
 * until the latter has been completely decommissioned. This is so that
 * handleOUP.py can be used as a fallback if necessary.
 *
 * Be EXCEEDINGLY CAUTIOUS about changing this code, because it is
 * safety-critical. Problems in this code can cause delays or failures in
 * getting severe weather information out to the public.
 *
 * <br>
 * <br>
 *
 * Information from handleOUP.py:
 *
 * Derived from port of handleOUP.pl, but diverged to support single path of
 * dissemination. Assigns a priority to the product, and attempts to send it to
 * the message handling system if it's not in the include lists.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 7, 2025  2038247    tgurney     Initial creation
 * Apr 21, 2025 2038247    tgurney     Add disseminator observers
 *                                     (to support radar)
 *
 * </pre>
 *
 * @author tgurney
 */

public class OUPDisseminator {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(OUPDisseminator.class);

    private static final String DISSEMINATION_DIR = "dissemination/";

    private static final String INGEST_ROUTE = "handleoupFilePush";

    private final Map<String, String> ACTION_CODES = new HashMap<>();

    private final String dataDir;

    private final String OUT_DIR;

    private final String SITE_ID;

    /*
     * NOTE: handleOUP.py also had INGEST_DIR and several DB_* constants which
     * were apparently unused.
     */

    private final IPathManager pathMgr;

    /* File permissions for output files */
    private static final FileAttribute<?> o666 = PosixFilePermissions
            .asFileAttribute(Collections
                    .unmodifiableSet(EnumSet.of(PosixFilePermission.OWNER_READ,
                            PosixFilePermission.OWNER_WRITE,
                            PosixFilePermission.GROUP_READ,
                            PosixFilePermission.GROUP_WRITE,
                            PosixFilePermission.OTHERS_READ,
                            PosixFilePermission.OTHERS_WRITE)));

    private final Set<OUPDisseminatorObserver> observers;

    public OUPDisseminator() throws IOException {
        this(Set.of());
    }

    /**
     * Constructor
     *
     * @param observers
     *            Set of observers which will be notified of each OUP before it
     *            is sent to WAN. Empty set is permitted.
     * @throws IOException
     */
    public OUPDisseminator(Set<OUPDisseminatorObserver> observers)
            throws IOException {
        this.observers = observers;
        pathMgr = PathManagerFactory.getPathManager();
        String path = pathMgr
                .getStaticFile(DISSEMINATION_DIR + "rcv_action2codes.txt")
                .getPath();
        try (BufferedReader reader = new BufferedReader(new FileReader(path))) {
            String line;
            while ((line = reader.readLine()) != null) {
                String[] codeSplit = line.split("\\s+");
                ACTION_CODES.put(codeSplit[0], codeSplit[1]);
            }
        }
        dataDir = EDEXUtil.getEdexData();
        OUT_DIR = dataDir + "outgoing";
        if (!Files.isDirectory(Paths.get(OUT_DIR))) {
            Files.createDirectory(Paths.get(OUT_DIR));
        }
        SITE_ID = EDEXUtil.getEdexSite();
    }

    /*
     * NOTE: The Python version of this method had default arguments ackMgr=null
     * and test=false. We assume that no one will be calling this Java method
     * from existing Python code, so we don't have to provide additional method
     * signatures to allow omitting those arguments.
     */
    /**
     * Locally store and ingest the product, and also distribute it on the WAN
     * and/or NWWS.
     *
     * @throws IOException
     *             if a problem occurs with reading or writing to any file
     * @throws MhsSubmitException
     *             if a problem occurs with sending an MHS message
     *
     */
    public void process(OfficialUserProduct oup, String afosID,
            OUPResponse resp, OUPAckManager ackMgr, boolean test)
            throws IOException, MhsSubmitException {

        statusHandler.info("handleOUP java received " + oup.getFilename());
        String wmoTypeString = "";
        String userDateTimeStamp = "";
        String msg = "";

        /* WMO message type, aka bbb */
        if (!StringUtils.isEmpty(oup.getWmoType())) {
            wmoTypeString = oup.getWmoType().toUpperCase();
        }

        /* address */
        String address = oup.getAddress();
        if ("DEF".equals(address) || "ALL".equals(address)) {
            address = "DEFAULTNCF,NWWSUP";
        } else if (address == null) {
            address = "DEFAULTNCF,NWWSUP";
        }

        /* source, possibly None */
        String source = oup.getSource();

        /* time stamp DDHHMM */
        if (!StringUtils.isEmpty(oup.getUserDateTimeStamp())) {
            userDateTimeStamp = oup.getUserDateTimeStamp().toUpperCase();
            if (userDateTimeStamp.length() != 6) {
                msg = "Error: User date time stamp is wrong length\n";
                statusHandler.error("User date time stamp is wrong length");
                resp.setMessage(msg);
                return;
            }
        }

        /*
         * Initialize the product identifier
         */
        String awipsWanPil = oup.getAwipsWanPil();
        statusHandler.debug("awipsWanPil = " + awipsWanPil);

        /*
         * Extract the category ( NNN of CCCCNNNXXX ) from the awips ID
         */
        String prodCategory = getCategory(awipsWanPil);
        statusHandler.debug("Product Category = " + prodCategory);

        /*
         * Determine the transmission priority for WAN distribution
         */
        int priority = getPriority(prodCategory);
        oup.setPriority(priority);
        statusHandler.debug("Priority = " + priority);

        /*
         * Retrieve the contents of the product
         */
        String contents = oup.getProductText();
        String productId = contents.split("\n")[0].strip();

        /*
         * Locally store OUP in text database and archive
         */
        resp.setSendLocalSuccess(true);

        String awipsPathname = createTargetFile(
                contents.getBytes(StandardCharsets.UTF_8),
                OUT_DIR + "/" + oup.getFilename());
        if (StringUtils.isEmpty(awipsPathname)) {
            /* must have awipsPathname for any sends */
            statusHandler.debug("Unable to store product to text database:");
            msg = "Product " + awipsWanPil
                    + " failed to be ingested and archived.\n";
            statusHandler.debug(msg);
            resp.setMessage(msg);
            resp.setSendLocalSuccess(false);
            resp.setSendWANSuccess(false);
            return;
        } else if (!test) {
            try {
                if (MessageGenerator.getInstance()
                        .sendFileToIngest(awipsPathname, INGEST_ROUTE)) {
                    msg = "Product " + awipsWanPil
                            + " successfully ingested and archived locally.\n";
                    resp.setSendLocalSuccess(true);
                    statusHandler.info(msg);
                } else {
                    msg = "Product " + awipsWanPil
                            + " failed to be ingested and archived.\n";
                    statusHandler.error(msg);
                    resp.setSendLocalSuccess(false);
                }
            } catch (Exception e) {
                msg = "Product " + awipsWanPil
                        + " failed to be ingested and archived properly. Reason:\n"
                        + e.getLocalizedMessage();
                statusHandler.error(msg, e);
                resp.setSendLocalSuccess(false);
            }
        }
        String attachedFilename = oup.getAttachedFilename();
        byte[] attachedFile = oup.getAttachedFile();
        if (!StringUtils.isEmpty(attachedFilename) && attachedFile != null
                && attachedFile.length != 0) {
            /* spaces will screw up the command line string */
            attachedFilename = attachedFilename.replace(" ", "");

            attachedFilename = createTargetFile(new byte[0],
                    OUT_DIR + "/" + attachedFilename);
            File f = new File(attachedFilename);
            try (FileOutputStream fos = new FileOutputStream(f)) {
                fos.write(attachedFile);
                fos.flush();
            }
        }

        if (test) {
            try {
                Files.delete(Paths.get(awipsPathname));
            } catch (Exception e) {
                /* ignore */
            }
            if (!StringUtils.isEmpty(attachedFilename)) {
                try {
                    Files.delete(Paths.get(attachedFilename));
                } catch (Exception e) {
                    /* ignore */
                }
            }
            resp.setSendWANSuccess(true);
            return;
        }

        String messageIdToAcknowledge = null;
        /*
         * Check if product should be distributed over WAN via NCF
         */
        String wmoID = slice(contents, 0, 6);
        String[] splitAddr = address.split(",");
        for (String addr : splitAddr) {
            if (!"000".equals(addr)) { /* 000 is local only */
                statusHandler.info("Addressee is " + addr);
                /*
                 * Check if product should be sent to the NWWS for uplink
                 */
                if (addr.contains("NWWSUP")) {
                    if (isNWWSProduct(awipsWanPil, afosID, wmoID, SITE_ID)) {
                        /*
                         * Send OUP to its designated NWWS primary and backup
                         * sites for up-link
                         */
                        String code = "NWWS_UPLINK";
                        if ("TextWS".equals(source)) {
                            code = "42";
                        }
                        boolean sendResult = sendWANMsg(productId,
                                awipsPathname, addr, code, userDateTimeStamp,
                                priority, wmoTypeString, source, resp, afosID,
                                attachedFilename);
                        if (!sendResult) {
                            /* failure of some kind so return */
                            return;
                        }
                    } else {
                        statusHandler.debug(
                                "Product is not an NWWS product.  Not sending product over NWWS up-link.");
                    }
                } else {
                    if (isLegalWANProduct(awipsWanPil, afosID, wmoID,
                            SITE_ID)) {
                        /*
                         * Send OUP to the NCF
                         */
                        String code = "0";
                        if ("DEFAULTNCF".equals(addr)) {
                            code = "134";
                        }
                        if ("TextWS".equals(source)) {
                            if (("ADR".equals(prodCategory)
                                    || "ADM".equals(prodCategory)
                                    || "ADA".equals(prodCategory))
                                    && !StringUtils.isEmpty(attachedFilename)) {
                                code = "7";
                            } else {
                                code = "4";
                            }
                        }
                        /*
                         * NOTE: This observer functionality was introduced for
                         * the sole purpose of sending text products to Radar
                         * Product Generators. handleOUP.py doesn't need this
                         * because it imports the RadarTextDataManager directly
                         * at runtime. We can't do that here due to build order
                         * constraints.
                         */
                        for (OUPDisseminatorObserver observer : observers) {
                            try {
                                observer.beforeSendToWAN(oup);
                            } catch (Exception e) {
                                statusHandler.error(e.getLocalizedMessage(), e);
                            }
                        }
                        boolean sendResult = sendWANMsg(productId,
                                awipsPathname, addr, code, userDateTimeStamp,
                                priority, wmoTypeString, source, resp, afosID,
                                attachedFilename);
                        if (!sendResult) {
                            /* failure of some kind so return */
                            return;
                        }
                        /*
                         * Copy this now as the values may change in another
                         * loop iteration
                         */
                        if (resp.getNeedAcknowledgment()
                                && messageIdToAcknowledge == null) {
                            messageIdToAcknowledge = resp.getMessageId();
                        }
                    } else {
                        statusHandler.info(
                                "Product is not authorized for distribution.");
                        statusHandler.info("Not sending product to NCF.");
                        msg = "Warning: Product is not authorized for distribution.\n";
                        resp.setMessage(msg);
                        return;
                    }
                } /* !addr.contains("NWWSUP") */
            } /* !"000".equals(addr) */
        } /* splitAddr loop */

        if (!StringUtils.isEmpty(messageIdToAcknowledge)) {
            resp.setNeedAcknowledgment(true);
            resp.setMessageId(messageIdToAcknowledge);
            if (ackMgr != null) {
                statusHandler.info("Waiting for acknowledgement of "
                        + messageIdToAcknowledge);
                ackMgr.waitAck(messageIdToAcknowledge, address, resp,
                        afosID + " " + userDateTimeStamp);
                statusHandler.info(String.format(
                        "Finished waiting for acknowledgment of %s: %s",
                        messageIdToAcknowledge,
                        (resp.isAcknowledged() ? "ACK" : resp.getMessage())));
                if (!resp.isAcknowledged()) {
                    /*
                     * NOTE: refactored into separate method for testing
                     * purposes.
                     */
                    sendITOAlarm(resp, messageIdToAcknowledge);
                }
            } else { /* ackMgr == null */
                statusHandler.error(
                        "Acknowledgement requirement, but ackMgr is null");
            }
        } /* !StringUtils.isEmpty(messageIdToAcknowledge) */
        statusHandler.debug("Script done...");
    }

    /**
     * Send ITO alarm. This exists as a separate method to be overridden for
     * testing purposes.
     */
    protected void sendITOAlarm(OUPResponse resp,
            String messageIdToAcknowledge) {
        try {
            Process process = new ProcessBuilder("/opt/OV/bin/OpC/opcmsg",
                    "application=MHS", "object=MHS",
                    String.format("msg_text=%s (msgid %s)", resp.getMessage(),
                            messageIdToAcknowledge),
                    "severity=Critical", "msg_grp=AWIPS").start();
            int ec = process.waitFor();
            if (ec != 0) {
                statusHandler
                        .error("Error sending ITO alarm: exit code = " + ec);
            }

        } catch (Throwable t) {
            statusHandler.error(
                    "Error sending ITO alarm: " + t.getLocalizedMessage(), t);
        }
    }

    /**
     * Determines the product category from the AWIPS identifier.
     *
     * @param awipsID
     *            AWIPS product identifier (CCCCNNNXXX)
     * @return 3-letter product category (NNN of CCCCNNNXXX)
     */
    protected static String getCategory(String awipsID) {
        statusHandler.debug("getCategory():");
        return slice(awipsID, 4, 7);
    }

    /**
     * Returns the priority level of the product based on its category.
     *
     * @param nnn
     *            3 letter product category (NNN)
     * @return Priority level [0,1,2] where 2 = highest
     * @throws IOException
     */
    private int getPriority(String nnn) throws IOException {
        statusHandler.debug("getPriority():");

        String priority = "0";
        String path = pathMgr
                .getStaticFile(DISSEMINATION_DIR + "awipsPriorities.txt")
                .getPath();
        try (BufferedReader pfile = new BufferedReader(new FileReader(path))) {
            String line;
            while ((line = pfile.readLine()) != null) {
                if (Objects.equals(nnn, slice(line, 0, 3))) {
                    statusHandler.debug(line);
                    if (line.length() < 5) {
                        /*
                         * NOTE: handleOUP.py would have gotten an empty string
                         * here and then thrown ValueError when attempting to
                         * convert to int. This alternative is more polite.
                         */
                        throw new RuntimeException(
                                "invalid line (too short) in awipsPriorities.txt: \n"
                                        + line);
                    } else {
                        priority = line.substring(4).strip();
                    }
                    break;
                }
            }
        }
        return Integer.parseInt(priority);
    }

    /**
     * Determines whether the product is a legal WAN product.
     *
     * Reads the site-specific WAN exclusionary list which contains a list of
     * product ids representing products which are not meant for distribution
     * over WAN via NCF. The AWIPS id, the AFOS id, and the WMO id, are
     * acceptable representations of the product id.
     *
     * If the exclusionary file either does not exist, is empty, or cannot be
     * read, then the product will be distributed.
     *
     * @param myAwipsId
     *            AWIPS identifier (CCCCNNNXXX)
     * @param myAfosId
     *            AFOS identifier (CCCNNNXXX)
     * @param myWmoId
     *            (TTAAII)
     * @param siteID
     * @return true or false
     * @throws IOException
     */
    private boolean isLegalWANProduct(String myAwipsId, String myAfosId,
            String myWmoId, String siteID) throws IOException {
        statusHandler.debug("isLegalWANProduct():");
        /* Read the WAN exclusionary file */
        String fileName = "WAN_exclude_" + siteID + ".txt";
        File locFile = pathMgr.getStaticFile(DISSEMINATION_DIR + fileName);
        String filePath = null;
        if (locFile != null) {
            filePath = locFile.getPath();
        }
        if (filePath != null && Files.isRegularFile(Paths.get(filePath))) {
            /*
             * NOTE: same as in handleOUP.py, a race condition is possible here,
             * theoretically. If something happens to the file after the call to
             * Files.isRegularFile, the caller will get an IOException. This is
             * fantastically unlikely.
             */
            try (BufferedReader wanExcludeFile = new BufferedReader(
                    new FileReader(filePath))) {
                String line;
                while ((line = wanExcludeFile.readLine()) != null) {
                    if (!line.startsWith("#")) {
                        String productId = line.strip();
                        if (Objects.equals(productId, myAwipsId)
                                || Objects.equals(productId, myAfosId)
                                || Objects.equals(productId, myWmoId)) {
                            statusHandler.info(
                                    "Product found in WAN exclude list as "
                                            + productId);
                            return false;
                        }
                    }
                }
            }
            /*
             * Otherwise, product did not appear on the exclude list and
             * therefore, product is meant for distribution
             */
            statusHandler.info(myAwipsId + " is a legal WAN product.");
            return true;
        } else {
            statusHandler
                    .info(fileName + " does not exist or is empty.  Sending "
                            + "product over WAN.");
            return true;
        }
    }

    /*
     * NOTE: The Python version of this method had default arguments
     * subject=null and attachedFilename=null. This method is private (in the
     * Java version only), so that is now irrelevant.
     */
    /**
     * Distributes an OUP to a specified receiving site over the WAN.
     *
     * @return true if successful message submission, false if unsuccessful.
     * @throws MhsSubmitException
     */
    private boolean sendWANMsg(String productId, String prodPathName,
            String receivingSite, String handling, String userDateTimeStamp,
            int priority, String wmoTypeString, String source, OUPResponse resp,
            String subject, String attachedFilename) throws MhsSubmitException {
        statusHandler.info("sendWANMsg " + prodPathName + " addr="
                + receivingSite + " code=" + handling + " source=" + source);
        int code;
        try {
            code = Integer.parseInt(handling);
        } catch (Throwable t) {
            code = Integer.parseInt(ACTION_CODES.get(handling));
        }

        /*
         * set acknowledgement from receiver if message is high priority and is
         * from TextWS
         */
        MhsMessage mhsMsg = new MhsMessage(code);

        if (!StringUtils.isEmpty(subject)) {
            mhsMsg.setSubject(subject);
        }
        if (!StringUtils.isEmpty(attachedFilename)) {
            mhsMsg.addEnclosure(attachedFilename);
        }

        mhsMsg.addEnclosure(prodPathName);
        MhsMessagePriority jpriority;
        if (priority == 0) {
            jpriority = MhsMessagePriority.Default;
        } else if (priority == 1) {
            jpriority = MhsMessagePriority.Medium;
        } else if (priority == 2) {
            jpriority = MhsMessagePriority.High;
        } else {
            /*
             * NOTE: handleOUP.py did not have this, instead it would have
             * thrown a NameError when attempting to setPriority(jpriority)
             */
            throw new RuntimeException("priority must be one of [0,1,2], "
                    + "not \"" + priority + "\"");
        }
        mhsMsg.setPriority(jpriority);

        if (priority == 2 && "TextWS".equals(source)) {
            resp.setNeedAcknowledgment(true);
            mhsMsg.addAckAddressee(receivingSite);
            mhsMsg.setTimeoutTime(300);
        } else {
            /* No need to get acknowledgement from receiver */
            resp.setNeedAcknowledgment(false);
            mhsMsg.addAddressee(receivingSite);
        }

        String result = sendMhsMessage(mhsMsg);

        if (StringUtils.isEmpty(result)) {
            /*
             * Not sure if this block can ever be reached; MhsMessage.send()
             * claims to throw an exception on error and only return if
             * successful. We include it anyway because handleOUP.py has it.
             */
            result = "Error sending product " + productId + " to "
                    + receivingSite + ". Check server logs for more detail.\n";
            statusHandler.error(result);
            resp.setSendWANSuccess(false);
            resp.setMessage(result);
            return false;
        } else {
            resp.setSendWANSuccess(true);
            if (resp.getNeedAcknowledgment()) {
                resp.setMessageId(result);
            }

            statusHandler.info("Successful send of " + result);
        }
        return true;
    }

    /**
     * Send the MhsMessage. This exists as a separate method to be overridden
     * for testing purposes.
     *
     * @throws MhsSubmitException
     */
    protected String sendMhsMessage(MhsMessage m) throws MhsSubmitException {
        statusHandler.info("Calling mhsMsg.send()");
        return m.send();
    }

    /**
     * Determines whether the product is a valid NWWS product.
     *
     * Reads the site-specific NWWS exclusionary list which contains a list of
     * product ids representing products which are not meant for distribution
     * over the NWWS up-link. The AWIPS id, the AFOS id, and the WMO id, are
     * acceptable representations of the product id.
     *
     * If the exclusionary file either does not exist, is empty, or cannot be
     * read, then the product will be up-linked.
     *
     * @param myAwipsId
     *            AWIPS identifier (CCCCNNNXXX)
     * @param myAfosId
     *            AFOS identifier (CCCNNNXXX)
     * @param myWmoId
     *            WMO identifier (TTAAII)
     * @param siteID
     * @return true or false
     * @throws IOException
     */
    private boolean isNWWSProduct(String myAwipsId, String myAfosId,
            String myWmoId, String siteID) throws IOException {
        statusHandler.debug("isNWWSProduct():\n");
        /* Read the NWWS exclusionary file */
        String fileName = "NWWS_exclude_" + siteID + ".txt";
        File locFile = pathMgr.getStaticFile(DISSEMINATION_DIR + fileName);
        String filePath = null;
        if (locFile != null) {
            filePath = locFile.getPath();
        }
        if (filePath != null && Files.isRegularFile(Paths.get(filePath))) {
            /*
             * NOTE: same as in handleOUP.py, a race condition is possible here,
             * theoretically. If something happens to the file after the call to
             * Files.isRegularFile, the caller will get an IOException. This is
             * fantastically unlikely.
             */
            try (BufferedReader nwwsExcludeFile = new BufferedReader(
                    new FileReader(filePath))) {
                String line;
                while ((line = nwwsExcludeFile.readLine()) != null) {
                    /*
                     * If entry is found, then product should not be distributed
                     * over the NWWS uplink
                     */
                    if (!line.startsWith("#")) { /* skips comment lines */
                        String productId = line.strip();
                        if (Objects.equals(productId, myAwipsId)
                                || Objects.equals(productId, myAfosId)
                                || Objects.equals(productId, myWmoId)) {
                            statusHandler.info(
                                    "Product found in NWWS exclude list as "
                                            + productId);
                            return false;
                        }
                    }
                }
            }
            /*
             * Otherwise, product did not appear on the exclude list and
             * therefore, product is meant for distribution
             */
            statusHandler.info(myAwipsId + " is an NWWS product.");
            return true;
        } else {
            statusHandler
                    .info(fileName + " does not exist or is empty.  Sending "
                            + "product over NWWS uplink.");
            return true;
        }
    }

    /**
     * Creates a product file in the named target directory.
     *
     * @param fileData
     * @param targetPathname
     * @return The output path (which may differ from targetPathname)
     * @throws IOException
     */
    protected static String createTargetFile(byte[] fileData,
            String targetPathname) throws IOException {
        statusHandler.debug("createTargetFile():");
        statusHandler.debug("target product pathname = " + targetPathname);

        String pathToUse = targetPathname;
        int i = 0;
        SeekableByteChannel byteChannel = null;
        /*
         * NOTE: handleOUP.py had "while True:" and relied on a break to exit
         * the loop if the file was created. This is a little bit nicer.
         */
        while (byteChannel == null) {
            try {
                /*
                 * This weird method is apparently the only way in Java to
                 * create a file, open it for writing, and set file permissions,
                 * all together as a single atomic operation.
                 */
                byteChannel = Files.newByteChannel(Paths.get(pathToUse),
                        Set.of(StandardOpenOption.CREATE_NEW,
                                StandardOpenOption.WRITE),
                        o666);
            } catch (FileAlreadyExistsException e) {
                /*
                 * NOTE: handleOUP.py caught OSError and checked if errno ==
                 * EEXIST and if not, it would rethrow the exception. We assume
                 * this is equivalent.
                 */
                i += 1;
                pathToUse = targetPathname + "." + i;
            }
        }
        try {
            if (i > 0) {
                statusHandler.info("Renamed target file to " + pathToUse);
            }
            ByteBuffer buffer = ByteBuffer.wrap(fileData);
            byteChannel.write(buffer);
        } finally {
            /*
             * NOTE: handleOUP.py had this outside of a finally block, but
             * failing to close the file if the write fails seems unambiguously
             * bad.
             */
            byteChannel.close();
        }
        return pathToUse;
    }

    /**
     * Emulates Python-style string slicing. Negative first argument is not
     * supported and will throw an IndexOutOfBoundsException.
     *
     * @param s
     *            The string
     * @param start
     *            Start index (inclusive)
     * @param stop
     *            Stop index (exclusive)
     * @return The substring
     */
    protected static String slice(String s, int start, int stop) {
        if (stop < 0) {
            stop = s.length() + stop;
        }
        if (start >= s.length() || start >= stop) {
            return "";
        }
        if (stop >= s.length()) {
            return s.substring(start);
        }
        return s.substring(start, stop);
    }
}
