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
package com.raytheon.uf.edex.plugin.text.dbsrv.handler;

import java.util.List;

import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.request.ExecuteWmoCmdRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.plugin.text.db.TextDB;

/**
 * Request handler for ExecuteAwipsCmdRequests. Forwards request to textdb.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 05, 2011            rferrel     Initial creation
 * May 20, 2014 2536       bclement    moved from edex.textdb to edex.plugin.text
 * Jan 18, 2016 4562       tjensen     Moved from edex.plugin.text to 
 *                                     edex.plugin.text.dbsrv
 * Aug 28, 2016 5839       rferrel     Added past version.
 * Feb 14, 2017 6111       njensen     Corrected generics
 * 
 * </pre>
 * 
 * @author rferrel
 */
public class ExecuteWmoCmdHandler implements
        IRequestHandler<ExecuteWmoCmdRequest> {

    @Override
    public Object handleRequest(ExecuteWmoCmdRequest request)
            throws Exception {
        String wmoId = request.getWmoId();
        String site = request.getSite();
        String abbrId = request.getNnnXxx();
        String lastHrs = null;
        String hdrTime = request.getHdrTime();
        String pastVersion = request.getPastVersion();
        String bbbId = request.getBbb();
        boolean operationalMode = request.isOperationalMode();
        boolean fullDataRead = request.isFullDataRead();
        int intlProd = 0;

        TextDB textDB = new TextDB();
        List<StdTextProduct> resp = textDB.readAwips(wmoId, site, intlProd,
                abbrId, lastHrs, hdrTime, pastVersion, bbbId, fullDataRead,
                operationalMode);
        return resp;
    }

}
