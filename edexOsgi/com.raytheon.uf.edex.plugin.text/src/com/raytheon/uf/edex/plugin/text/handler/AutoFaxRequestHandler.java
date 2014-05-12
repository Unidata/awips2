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
package com.raytheon.uf.edex.plugin.text.handler;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.textdb.fax.AutoFaxDao;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.text.db.AutoFaxRecord;
import com.raytheon.uf.common.dataplugin.text.request.AutoFaxRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 28, 2010            bfarmer     Initial creation
 * 
 * </pre>
 * 
 * @author bfarmer
 * @version 1.0
 */

public class AutoFaxRequestHandler implements IRequestHandler<AutoFaxRequest> {
    private Log logger = LogFactory.getLog(getClass());

    public Object handleRequest(AutoFaxRequest request) {
        AutoFaxRecord req = new AutoFaxRecord(request.getAfosPil(),
                request.getFaxNumber(), request.getPhoneNumber(),
                request.getRecipient(), request.getCompany());
        try {
            AutoFaxDao faxdao = new AutoFaxDao();
            if (request.isDeleteRecord()) {
                faxdao.removeAutoFaxRecord(req);
            } else {
                faxdao.addAutoFaxRecord(req);
            }
        } catch (PluginException e) {
            if (request.isDeleteRecord()) {
                logger.error("Error attempting to delete AutoFax record.", e);
            } else {
                logger.error("Error attempting to add AutoFax record.", e);
            }
        }
        return null;
    }
}
