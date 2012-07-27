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
package com.raytheon.edex.uengine.runners;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.bind.annotation.adapters.HexBinaryAdapter;

import com.raytheon.edex.uengine.exception.MicroEngineException;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.message.Header;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;
import com.raytheon.uf.common.message.response.AbstractResponseMessage;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.RunProcess;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * A &mu;Engine script runner for LDAD dissemination scripts.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08Dec2008    1709       MW Fegan    Initial creation.
 * 14Apr2009    2045       MW Fegan    Implemented AFOS PIL (LDAD)
 *                                      script running functionality.
 * 06Jan2010    4166       MW Fegan    Use EDEXUtil for messaging.
 * 25May2011    8686       cjeanbap    Replaced how trigger is retrieved.
 * 28May2011    8686       bkowal      Replaced useless try ... catch block.
 *                                     Added logic to utilize the trigger.
 * Sep 19, 2011 10955      rferrel     Use RunProcess
 * 05/25/2012   DR 15015   D. Friedman Use helper script to launch triggers.
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public class LdadMicroEngine extends AMicroEngine {
    private static final String TEXT_DB_QUEUE = "textdbsrvinternal";
    private static final String SCRIPT_LAUNCHER_PATH = "util/scriptLauncher";

    /**
     * Constructor.
     */
    public LdadMicroEngine() {
        super();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.runners.AMicroEngine#execute()
     */
    @Override
    public void execute() throws MicroEngineException {
        /* empty list indicating no results */
        this.result = new ArrayList<AbstractResponseMessage>();
        /*
         * For LDAD script running, the script has format <path>/script_name
         * <trigger pid>
         * 
         * This will need to basically replicate the LDAD script running
         * capability. Initially, 1) retrieve the text product from the database
         * (using the pid) 2) write the text product to a file using the
         * standard file name env("FXA_DATA")/trigger/pid 3) execute the script
         * using the system uEngine
         */
        if (this.trigger == null || this.trigger.trim().equalsIgnoreCase("")) {
            throw new MicroEngineException(
                    "Invalid trigger, unable to execute script: " + this.script);
        }

        String prodID = null;
        try {
            prodID = (this.trigger.split(" "))[0];
        } catch (Exception e) {
            throw new MicroEngineException(
                    "Unable to extract product ID, unable to execute script: "
                            + this.script);
        }

        /* retrieve text product from textDB */
        try {
            String contents = retrieveTextProduct(prodID);
            writeProductToFile(prodID, contents);
            executeLdadScript(this.script);
            logger.info("Trigger: " + this.script);
        } catch (Exception e) {
            throw new MicroEngineException("unable to execute script "
                    + this.script, e);
        }

    }

    /**
     * Retrieves the product from the text database. The retrieval is performed
     * via JMS to the Text Database Service.
     * 
     * @param prodID
     *            AFOS PIL of the product to retrieve
     * 
     * @return the latest product for the AFOS PIL
     * 
     * @throws Exception
     *             if any error occurs
     */
    private String retrieveTextProduct(String prodID) throws Exception {
        Message message = createProductRequestMessage(prodID);
        String xml = SerializationUtil.marshalToXml(message);
        Object response = EDEXUtil.getMessageProducer().sendSync(TEXT_DB_QUEUE,
                xml);
        return extractProductFromResponse(response.toString());
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
     *            AFOS PIL of the product to retrieve
     * 
     * @return the product request message
     * 
     * @throws Exception
     *             if any error occurs
     */
    private Message createProductRequestMessage(String prodID) throws Exception {
        Message message = new Message();
        Header header = new Header();
        List<Property> properties = new ArrayList<Property>();
        properties.add(new Property("VIEW", AsciiToHex("text")));
        properties.add(new Property("OP", AsciiToHex("GET")));
        properties.add(new Property("SUBOP", AsciiToHex("PROD")));
        properties.add(new Property("AFOSCMD", AsciiToHex(prodID)));
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
     *          {@literal <properties name="STDOUT" value="{the text of the product}">}
     *       {@literal </header>}
     *    {@literal </message>}
     * </PRE>
     * 
     * @param response
     *            the response from the server
     * 
     * @return the product text
     * 
     * @throws Exception
     *             if an error occurs
     */
    private String extractProductFromResponse(String response) throws Exception {
        StringBuffer retVal = new StringBuffer();
        Message reply = (Message) SerializationUtil.unmarshalFromXml(response);
        Header header = reply.getHeader();
        if (header != null && header.getProperties() != null) {
            for (Property property : header.getProperties()) {
                if (property.getName().equalsIgnoreCase("stdout")) {
                    String value = hexToAscii(property.getValue());
                    retVal.append(value).append("\n");
                }
            }
        }

        return retVal.toString();
    }

    /**
     * Writes the product to the file system. The product is written to
     * $FXA_DATA/trigger/{prodID} -- $FXA_DATA is obtained from the environment
     * and {prodID} is the AFOS PIL of the product.
     * 
     * @param prodID
     *            AFOS PIL of the product
     * @param product
     *            the product to export
     * 
     * @throws Exception
     *             if any error occurs
     */
    private void writeProductToFile(String prodID, String product)
            throws Exception {
        String fxaData = System.getenv("FXA_DATA");
        File fl = new File(fxaData, "trigger/" + prodID);
        FileUtil.bytes2File(product.getBytes(), fl);
    }

    /**
     * 
     * @param script
     * @throws Exception
     */
    private void executeLdadScript(String script) throws Exception {
        logger.info("Executing script: script= " + script);
        Map<String, String> env = System.getenv();
        List<String> strEnv = new ArrayList<String>();
        for (Entry<String, String> entry : env.entrySet()) {
            strEnv.add(entry.getKey() + "=" + entry.getValue());
        }
        String launcher = getScriptLauncher();
        if (launcher != null)
            script = launcher + ' ' + script;
        // DR#10955
        RunProcess.getRunProcess()
                .exec(script, strEnv.toArray(new String[] {}));
    }

    /**
     * Converts an ASCII string to a binary representation. This conversion is
     * required as Text Database Server expects all Header Property values to be
     * HEX encoded.
     * 
     * @param string
     *            the string to convert
     * @return the converted string
     */
    private String AsciiToHex(String string) {
        return new HexBinaryAdapter().marshal(string.getBytes());
    }

    /**
     * Converts a HEX encoded string to plain ASCII. This conversion is required
     * as Text Database Server expects all Header Property values to be HEX
     * encoded.
     * 
     * @param hexString
     *            the HEX encoded string
     * 
     * @return the plain text string
     */
    private String hexToAscii(String hexString) {
        byte[] b = new HexBinaryAdapter().unmarshal(hexString);
        return new String(b);
    }
    
    /**
     * Finds a helper script that should be used to launch triggers. The helper
     * script terminates immediately so that resources can be freed even if the
     * trigger takes a long time to run.
     * 
     * @return The absolute path of the help script or null if not found.
     */
    private String getScriptLauncher() {
        File f = null;
        try {
            f = PathManagerFactory.getPathManager().getStaticFile(SCRIPT_LAUNCHER_PATH);
            if (f != null) {
                f.setExecutable(true);
                return f.getAbsolutePath();
            } else {
                logger.warn("Script launcher not found");
                return null;
            }
        } catch (Exception e) {
            logger.error("Error finding script launcher", e);
            return null;
        }
    }

}
