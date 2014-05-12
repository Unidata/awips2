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
package com.raytheon.uf.edex.plugin.text.subscription;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.textdb.dbapi.impl.TextDB;
import com.raytheon.edex.textdb.fax.AutoFaxDao;
import com.raytheon.edex.textdb.fax.FaxSender;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.text.AutoFaxContainer;
import com.raytheon.uf.common.dataplugin.text.db.AutoFaxRecord;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.plugin.text.TextRecord;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 15, 2010            bfarmer     Initial creation
 * 
 * </pre>
 * 
 * @author bfarmer
 * @version 1.0
 */

public class AutoFaxManager {

    AutoFaxDao faxdao;

    private transient Log logger = LogFactory.getLog(getClass());

    AutoFaxManager() {
        try {
            faxdao = new AutoFaxDao();
        } catch (PluginException e) {
            logger.error("Error creating AutoFaxDao", e);
        }
    }

    public void processEvent(Object event) throws EdexException {
        List<String> trigger = new ArrayList<String>();
        if (event instanceof PluginDataObject[]) {
            PluginDataObject[] pdos = (PluginDataObject[]) event;
            for (PluginDataObject pdo : pdos) {
                try {
                    if (pdo instanceof TextRecord) {
                        TextRecord tr = (TextRecord) pdo;
                        String prodID = tr.getProductId();
                        if (logger.isDebugEnabled()) {
                            logger.debug("Processing trigger: " + prodID
                                    + ", class = "
                                    + pdo.getClass().getSimpleName());
                        }
                        trigger.add(prodID);
                    }
                } catch (Exception e) {
                    logger.warn("Unable to extract product information from ["
                            + pdo.toString() + "] skipping...");
                }
            }
        }
        sendFaxes(trigger);
    }

    private void sendFaxes(List<String> trigger) {
        for (String afosPil : trigger) {
            AutoFaxContainer faxrecords = faxdao.getAllRecordsForPil(afosPil);
            if (faxrecords.getAutoFaxList() != null) {
                for (AutoFaxRecord faxRecord : faxrecords.getAutoFaxList()) {
                    // Get the last text product on this pil from textdb
                    TextDB textdb = new TextDB();
                    List<StdTextProduct> prodList = textdb.executeAFOSCommand(
                            afosPil, null, true);
                    String faxText = "";
                    if (prodList.size() > 0) {
                        faxText = prodList.get(0).getProduct();
                    }
                    if (faxText != "") {
                        // Shove it out to all the autofax subscribers to this
                        // PIL.
                        try {
                            FaxSender.sendFax(faxRecord.getCompany(), faxRecord
                                    .getId().getFaxNumber(), faxRecord
                                    .getRecipient(), faxText, faxRecord.getId()
                                    .getAfosPil());
                        } catch (IOException e) {
                            logger.warn("Error sending fax for AFOS PIL: "
                                    + afosPil);
                        }
                    }
                }
            }
        }
    }

}
