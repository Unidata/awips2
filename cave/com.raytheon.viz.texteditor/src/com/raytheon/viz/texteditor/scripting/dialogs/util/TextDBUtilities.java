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
package com.raytheon.viz.texteditor.scripting.dialogs.util;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.adapters.HexBinaryAdapter;

import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.message.Header;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.VizApp;

/**
 * Contains utility methods for interactions with the text database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 22, 2009            mfegan     Initial creation
 * Jul 13, 2010 2187       cjeanbap   Add opertional mode functionality.
 * 02Aug2010    2187       cjeanbap    Update variable/method signature to be consistent.
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public final class TextDBUtilities {
    public static final String TYPE_PROD = "PROD";

    public static final String TYPE_INFO = "INFO";

    /**
     * 
     */
    private TextDBUtilities() {
        // no class instances
    }

    public static String writeProductToDatabase(String prodID, String contents, boolean operationalMode)
            throws Exception {
        Message message = createProductStoreMessage(prodID, contents, operationalMode);
        String xml = SerializationUtil.marshalToXml(message);
        String address = VizApp.getHttpServer() + "/textdbsrv";

        String response = HttpClient.getInstance().post(address, xml);
        message = (Message) SerializationUtil.unmarshalFromXml(response);
        StringBuffer sb = new StringBuffer();
        for (Property property : message.getHeader().getProperties()) {
            String value = hexToAscii(property.getValue());
            if (value.matches("^NORMAL")) {
                sb.append(value.split(":")[1]).append("\n");
            } else {
                throw new Exception("Received error from product retrieval - "
                        + value);
            }
        }
        return sb.toString().trim();
    }

    public static String[] readProductFromDatabase(String prodID, String type, boolean operationalMode)
            throws Exception {
        Message message = createProductRequestMessage(prodID, type, operationalMode);
        String xml = SerializationUtil.marshalToXml(message);
        String address = VizApp.getHttpServer() + "/textdbsrv";

        String response = HttpClient.getInstance().post(address, xml);

        message = (Message) SerializationUtil.unmarshalFromXml(response);
        List<String> products = new ArrayList<String>();
        Property[] properties = message.getHeader().getProperties();
        if (properties == null) {
            return null;
        }
        for (Property property : message.getHeader().getProperties()) {
            if ("stdout".equalsIgnoreCase(property.getName())) {
                products.add(hexToAscii(property.getValue()));
            } else {
                throw new Exception(property.getValue());
            }
        }

        return products.toArray(new String[] {});
    }

    private static Message createProductRequestMessage(String prodID,
            String type, boolean operationalMode) throws Exception {
        Message message = new Message();
        Header header = new Header();
        List<Property> properties = new ArrayList<Property>();
        properties.add(new Property("VIEW", AsciiToHex("text")));
        properties.add(new Property("OP", AsciiToHex("GET")));
        properties.add(new Property("SUBOP", AsciiToHex(type)));
        properties.add(new Property("AFOSCMD", AsciiToHex(prodID)));
        properties.add(new Property("OPERATIONAL", AsciiToHex(new Boolean(operationalMode).toString())));
        header.setProperties(properties.toArray(new Property[] {}));
        message.setHeader(header);
        return message;
    }

    private static Message createProductStoreMessage(String prodID,
            String product, boolean operationalMode) throws Exception {
        Message message = new Message();
        Header header = new Header();
        List<Property> properties = new ArrayList<Property>();
        properties.add(new Property("VIEW", AsciiToHex("text")));
        properties.add(new Property("OP", AsciiToHex("PUT")));
        properties.add(new Property("PRODID", AsciiToHex(prodID)));
        properties.add(new Property("product", AsciiToHex(product)));
        properties.add(new Property("OPERATIONAL", AsciiToHex(new Boolean(operationalMode).toString())));
        header.setProperties(properties.toArray(new Property[] {}));
        message.setHeader(header);
        return message;
    }

    private static String AsciiToHex(String string) {
        return new HexBinaryAdapter().marshal(string.getBytes());
    }

    private static String hexToAscii(String hexString) {
        byte[] b = new HexBinaryAdapter().unmarshal(hexString);
        return new String(b);
    }

}
