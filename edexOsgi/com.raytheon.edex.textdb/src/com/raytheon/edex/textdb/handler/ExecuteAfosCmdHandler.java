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
package com.raytheon.edex.textdb.handler;

import com.raytheon.edex.textdb.dbapi.impl.TextDB;
import com.raytheon.uf.common.dataplugin.text.StdTextProductContainer;
import com.raytheon.uf.common.dataplugin.text.request.ExecuteAfosCmdRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 2, 2010             njensen     Initial creation
 * Apri 14, 2010 4734      mhuang      Returned StdTextProductContainer object
 * May 23, 2012  14952     rferrel     Added refTime.
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ExecuteAfosCmdHandler implements
        IRequestHandler<ExecuteAfosCmdRequest> {

    @Override
    public Object handleRequest(ExecuteAfosCmdRequest request) throws Exception {
        TextDB textdb = new TextDB();
        StdTextProductContainer resp = new StdTextProductContainer();
        resp.setProductList(textdb.executeAFOSCommand(request.getAfosCommand(),
                request.getAfosLocale(), request.isOperationalMode(),
                request.isReftimeMode(), request.getRefTime()));
        return resp;
    }

}
