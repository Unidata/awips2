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
package com.raytheon.uf.edex.plugin.text.subscription.services;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFilePermission;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.message.Header;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;
import com.raytheon.uf.common.util.RunProcess;
import com.raytheon.uf.common.util.file.IOPermissionsHelper;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.plugin.text.dbsrv.TextDBSrv;

/**
 *
 * A script runner for Text Trigger execution scripts.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 4, 2016  5203       tjensen     Initial creation
 * Jan 18, 2016 4562       tjensen     Moved from edex.plugin.text to
 *                                     edex.plugin.text.subscription
 * Mar 4, 2016  4716       rferrel     {@link #createProductRequestMessage(String)} determines if AWIPS or AFOS command.
 * Jul 24, 2017 6333       tgurney     Set file permissions on write
 * Aug 17, 2017 6392       tgurney     Move scriptLauncher to edex bin
 * Feb 15, 2018 6895       tgurney     Move trigger file dir to a final field
 *
 * </pre>
 *
 * @author tjensen
 */

public class TextTriggerHandler {

    public static final String TRIGGER_DIR = Paths
            .get(System.getenv("FXA_DATA"), "trigger").toString();

    private static final PosixFilePermission[] POSIX_DIRECTORY_PERMISSIONS = new PosixFilePermission[] {
            PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE,
            PosixFilePermission.OWNER_EXECUTE, PosixFilePermission.GROUP_READ,
            PosixFilePermission.GROUP_WRITE,
            PosixFilePermission.GROUP_EXECUTE };

    private static final FileAttribute<Set<PosixFilePermission>> POSIX_DIRECTORY_ATTRIBUTES = IOPermissionsHelper
            .getPermissionsAsAttributes(POSIX_DIRECTORY_PERMISSIONS);

    private static final PosixFilePermission[] POSIX_FILE_PERMISSIONS = new PosixFilePermission[] {
            PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE,
            PosixFilePermission.GROUP_READ, PosixFilePermission.GROUP_WRITE };

    private static final Set<PosixFilePermission> POSIX_FILE_SET = IOPermissionsHelper
            .getPermissionsAsSet(POSIX_FILE_PERMISSIONS);

    private static final String SCRIPT_LAUNCHER_PATH = Paths
            .get(EDEXUtil.getEdexBin(), "scriptLauncher").toString();

    /**
     * Logger instance for system logging.
     */
    protected transient Logger logger = LoggerFactory.getLogger(getClass());

    /**
     * Triggering AWIPS commands have length of 10 while the AFOS is 9.
     */
    private static final int AWIPS_CMD_LEN = 10;

    public TextTriggerHandler() {
        super();
    }

    public void execute(String script, String trigger) throws TriggerException {
        /*
         * <pre> For script running, the script has format <path>/script_name
         * <trigger pid>
         *
         * This will need to basically replicate the script running capability.
         * Initially, 1) retrieve the text product from the database (using the
         * pid) 2) write the text product to a file using the standard file name
         * env("FXA_DATA")/trigger/pid 3) execute the script </pre>
         */
        if (trigger == null || "".equals(trigger.trim())) {
            throw new TriggerException(
                    "Invalid trigger, unable to execute script: " + script);
        }

        String prodID = null;
        try {
            prodID = trigger.split(" ")[0];
        } catch (Exception e) {
            throw new TriggerException(
                    "Unable to extract product ID, unable to execute script: "
                            + script,
                    e);
        }

        /* retrieve text product from textDB */
        try {
            String contents = retrieveTextProduct(prodID);
            writeProductToFile(prodID, contents);
            executeTriggerScript(script);
            logger.info("Trigger: " + script);
        } catch (Exception e) {
            throw new TriggerException("unable to execute script " + script, e);
        }
    }

    /**
     * Retrieves the product from the text database. The retrieval is performed
     * via the Text Database Service.
     *
     * @param prodID
     *            AWIPS or AFOS PIL of the product to retrieve
     *
     * @return the latest product for the AWIPS or AFOS PIL
     */
    private String retrieveTextProduct(String prodID) {
        Message message = createProductRequestMessage(prodID);
        TextDBSrv server = new TextDBSrv();
        Message response = server.processMessage(message);
        return extractProductFromResponse(response);
    }

    /**
     * Generates the Text Database Server product request message. The message
     * created will be similar to<BR>
     *
     * <PRE>
     *    {@literal <message>}
     *       {@literal <header>}
     *          {@literal <properties name="VIEW" value="text"/>}
     *          {@literal <properties name="OP" value="GET"/>}
     *          {@literal <properties name="SUBOP" value="PROD"/>}
     *          {@literal <properties name="AFOSCMD" value="{prodID}"/>}
     *       {@literal </header>}
     *    {@literal </message>}
     * </PRE>
     *
     * @param prodID
     *            AWIPS or AFOS PIL of the product to retrieve
     *
     * @return the product request message
     *
     */
    private Message createProductRequestMessage(String prodID) {
        Message message = new Message();
        Header header = new Header();
        List<Property> properties = new ArrayList<>();
        properties.add(new Property("VIEW", "text"));
        properties.add(new Property("OP", "GET"));
        properties.add(new Property("SUBOP", "PROD"));
        properties.add(new Property(
                prodID.length() == AWIPS_CMD_LEN ? "AWIPSCMD" : "AFOSCMD",
                prodID));
        header.setProperties(properties.toArray(new Property[] {}));
        message.setHeader(header);
        return message;
    }

    /**
     * Extracts the product from the response. The response message will be
     * similar to<BR>
     *
     * <PRE>
     *    {@literal <message>}
     *       {@literal <header>}
     *          {@literal <properties name="STDOUT" value=
    "{the text of the product}">}
     *       {@literal </header>}
     *    {@literal </message>}
     * </PRE>
     *
     * @param response
     *            the response from the server
     *
     * @return the product text
     */
    private String extractProductFromResponse(Message reply) {
        StringBuilder retVal = new StringBuilder();
        Header header = reply.getHeader();
        if (header != null && header.getProperties() != null) {
            for (Property property : header.getProperties()) {
                if ("stdout".equalsIgnoreCase(property.getName())) {
                    retVal.append(property.getValue()).append("\n");
                }
            }
        }

        return retVal.toString();
    }

    /**
     * Writes the product to the file system. The product is written to
     * $FXA_DATA/trigger/{prodID} -- $FXA_DATA is obtained from the environment
     * and {prodID} is the AWIPS or AFOS PIL of the product.
     *
     * @param prodID
     *            AWIPS or AFOS PIL of the product
     * @param product
     *            the product to export
     */
    private void writeProductToFile(String prodID, String product)
            throws Exception {
        Path p = Paths.get(TRIGGER_DIR, prodID);
        com.raytheon.uf.common.util.file.Files.createDirectories(p.getParent(),
                POSIX_DIRECTORY_ATTRIBUTES);
        Files.write(p, product.getBytes());
        IOPermissionsHelper.applyFilePermissions(p, POSIX_FILE_SET);
    }

    private void executeTriggerScript(String script) throws Exception {
        logger.info("Executing script: script= " + script);
        Map<String, String> env = System.getenv();
        List<String> strEnv = new ArrayList<>();
        for (Entry<String, String> entry : env.entrySet()) {
            strEnv.add(entry.getKey() + "=" + entry.getValue());
        }

        File f = new File(SCRIPT_LAUNCHER_PATH);
        f.setExecutable(true);
        script = f.getAbsolutePath() + ' ' + script;
        RunProcess.getRunProcess().exec(script,
                strEnv.toArray(new String[] {}));
    }

}
