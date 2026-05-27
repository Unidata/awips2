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
package com.raytheon.viz.gfe.product;

import java.util.List;

import com.raytheon.uf.common.dataplugin.text.StdTextProductContainer;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.request.ExecuteAfosCmdRequest;
import com.raytheon.uf.common.dataplugin.text.request.WriteProductRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * Store and retrieve text product to/from text database.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10 FEB 2010  4132        ryu        Initial creation
 * 20 Apr 2010  4734       mhuang      Use StdTextProductContainer for 
 *                                      comunicating Edex server
 * 01Jun2010    2187       cjeanbap    Added operational mode functionality.
 * 07Jul2010    2187       cjeanbap    Added additional operational mode functionality.
 * 02Aug2010    2187       cjeanbap    Update variable/method signature to be consistent.
 * 
 * </pre>
 * 
 * @author ryu
 * @version 1.0
 * 
 */
public class TextDBUtil {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TextDBUtil.class);

    /**
     * Store the product to text database.
     */
    static public void storeProduct(String textdbId, String productText,
            boolean operationalMode) {
        WriteProductRequest req = new WriteProductRequest();
        req.setProductId(textdbId);
        req.setReportData(productText);
        req.setOperationalMode(!operationalMode);
        try {
            ThriftClient.sendRequest(req);
            Priority p = Priority.EVENTA;
            statusHandler.handle(p, "Product " + textdbId + " has been saved");
        } catch (VizException e) {
            statusHandler.handle(Priority.CRITICAL, "Failed to store product: "
                    + textdbId + " to textdb", e);
        }
    }

    /**
     * Retrieve a text product from text database.
     * 
     * @param operationalMode
     *            True, if CAVE is in "operational" mode. False, if CAVE is in
     *            "practice" mode
     */
    static public String retrieveProduct(String textdbId,
            boolean operationalMode) {
        return executeAfosCmd(textdbId, operationalMode);
    }

    /**
     * Retrieve a text product from text database.
     * 
     * @param textdbId
     * @param version
     * @param operationalMode
     *            True, if CAVE is in "operational" mode. False, if CAVE is in
     *            "practice" mode
     * @return
     */
    static public String retrieveProduct(String textdbId, int version,
            boolean operationalMode) {
        String afosCmd;
        if (version > 0) {
            afosCmd = "-" + version + ":" + textdbId;
        } else {
            afosCmd = textdbId;
        }
        return executeAfosCmd(afosCmd, operationalMode);
    }

    /**
     * Retrieve a text product from text database with an AFOS command.
     */
    static public String executeAfosCmd(String afosCommand,
            boolean operationalMode) {
        ExecuteAfosCmdRequest req = new ExecuteAfosCmdRequest();
        req.setAfosCommand(afosCommand);
        req.setOperationalMode(operationalMode);
        StringBuilder product = new StringBuilder();
        try {
            Object resp = ThriftClient.sendRequest(req);
            List<StdTextProduct> productList = ((StdTextProductContainer) resp)
                    .getProductList();
            for (int i = 0; i < productList.size(); i++) {
                product.append(productList.get(i).getProduct());
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.CRITICAL, "AFOS command \""
                    + afosCommand + "\" failed", e);
        }
        return product.toString();
    }
}
