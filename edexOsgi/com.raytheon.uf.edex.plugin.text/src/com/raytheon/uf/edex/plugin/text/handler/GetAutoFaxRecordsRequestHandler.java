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
import com.raytheon.uf.common.dataplugin.text.AutoFaxContainer;
import com.raytheon.uf.common.dataplugin.text.request.GetAutoFaxRecordsRequest;
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
 * Nov 2, 2010            bfarmer     Initial creation
 * 
 * </pre>
 * 
 * @author bfarmer
 * @version 1.0
 */

public class GetAutoFaxRecordsRequestHandler implements
        IRequestHandler<GetAutoFaxRecordsRequest> {
    private Log logger = LogFactory.getLog(getClass());

    public Object handleRequest(GetAutoFaxRecordsRequest request) {
        AutoFaxContainer retval = null;
        try {
            AutoFaxDao faxdao = new AutoFaxDao();
            retval = faxdao.getAllAutoFaxRecords();
        } catch (PluginException e) {
            logger.error("Error attempting to retrieve all AutoFax records.", e);
        }
        return retval;
    }
}
