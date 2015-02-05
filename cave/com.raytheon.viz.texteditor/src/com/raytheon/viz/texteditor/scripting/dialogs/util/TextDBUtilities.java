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

import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;
import com.raytheon.viz.texteditor.TextDBQuery;

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
 * Feb 04, 2015 4086       njensen     Resurrected class and changed to use TextDBQuery
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

    public static String writeProductToDatabase(String prodID, String contents,
            boolean operationalMode) throws Exception {
        TextDBQuery query = createProductStoreMessage(prodID, contents,
                operationalMode);
        Message message = query.executeQuery();
        StringBuffer sb = new StringBuffer();
        for (Property property : message.getHeader().getProperties()) {
            String value = property.getValue();
            if (value.matches("^NORMAL")) {
                sb.append(value.split(":")[1]).append("\n");
            } else {
                throw new Exception("Received error from product retrieval - "
                        + value);
            }
        }
        return sb.toString().trim();
    }

    public static String[] readProductFromDatabase(String prodID, String type,
            boolean operationalMode) throws Exception {
        TextDBQuery query = createProductRequestMessage(prodID, type,
                operationalMode);
        Message message = query.executeQuery();
        List<String> products = new ArrayList<String>();
        Property[] properties = message.getHeader().getProperties();
        if (properties == null) {
            return null;
        }
        for (Property property : message.getHeader().getProperties()) {
            if ("stdout".equalsIgnoreCase(property.getName())) {
                products.add(property.getValue());
            } else {
                throw new Exception(property.getValue());
            }
        }

        return products.toArray(new String[] {});
    }

    private static TextDBQuery createProductRequestMessage(String prodID,
            String type, Boolean operationalMode) {
        TextDBQuery query = new TextDBQuery();
        query.setQueryViewName("text");
        query.setQueryOpName("GET");
        query.setQuerySubObName(type);
        query.setQueryAfosCmd(prodID);
        query.setQueryOperationalMode(operationalMode.toString());
        return query;
    }

    private static TextDBQuery createProductStoreMessage(String prodID,
            String product, Boolean operationalMode) {
        TextDBQuery query = new TextDBQuery();
        query.setQueryViewName("text");
        query.setQueryOpName("PUT");
        query.setQueryProduct(product);
        query.addProductId(prodID);
        query.setQueryOperationalMode(operationalMode.toString());
        return query;
    }

}
