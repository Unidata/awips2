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

import java.util.List;

import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.request.ExecuteAwipsCmdRequest;
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
 * Aug 4, 2011            rferrel     Initial creation
 * May 20, 2014 2536       bclement    moved from edex.textdb to edex.plugin.text
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class ExecuteAwipsCmdHandler implements
        IRequestHandler<ExecuteAwipsCmdRequest> {

    @Override
    public Object handleRequest(ExecuteAwipsCmdRequest request)
            throws Exception {
        String wmoId = request.getWmoId();
        String site = request.getSite();
        String abbrId = request.getNnnXxx();
        String lastHrs = request.getLastHrs();
        String hdrTime = request.getHdrTime();
        String bbbId = request.getBbb();
        boolean fullDataRead = request.isFullDataRead();
        boolean operationalMode = request.isOperationalMode();
        int intlProd = 0;

        TextDB textDB = new TextDB();
        List<StdTextProduct> resp = textDB.readAwips(wmoId, site, intlProd,
                abbrId, lastHrs, hdrTime, bbbId, fullDataRead, operationalMode);
        return resp;
    }

}
