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

import java.util.Date;

import com.raytheon.uf.common.dataplugin.text.request.InsertStdTextProductRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.plugin.text.AlarmAlertUtil;
import com.raytheon.uf.edex.plugin.text.TextDecoder;
import com.raytheon.uf.edex.plugin.text.db.TextDB;

/**
 * Handles InsertStdTextProductRequests by placing them onto the LDAD queue and
 * sending product alarm alerts as specified by the request
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 12, 2009            mschenke     Initial creation
 * 01Jun2010               cjeanbap    Added operational mode functionality.
 * 08Jul2010    2187       cjeanbap    Added operational mode functionality.
 * May 12, 2014 2536       bclement    removed unused import
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class InsertStdTextProductHandler implements
        IRequestHandler<InsertStdTextProductRequest> {

    private TextDB dao;

    public InsertStdTextProductHandler() {
        dao = new TextDB();
    }

    @Override
    public Object handleRequest(InsertStdTextProductRequest request)
            throws Exception {
        Date d = new Date();

        d.setTime(dao.writeProduct(request.getAfosId(), request.getProduct(),
                request.getOperationalMode(), null));

        if (d.getTime() != Long.MIN_VALUE) {
            if (request.isNotifySubscriptions()) {
                if (request.getOperationalMode()) {
                    TextDecoder.sendTextToQueue(request.getAfosId());
                }
            }
            if (request.isNotifyAlarmAlert()) {
                AlarmAlertUtil.sendProductAlarmAlert(request.getAfosId(),
                        String.valueOf(d.getTime()),
                        request.getOperationalMode());
            }
        }
        return d;
    }
}
