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

import com.raytheon.uf.common.dataplugin.text.db.OperationalStdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.PracticeStdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.request.StdTextProductServerRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.plugin.text.AlarmAlertUtil;
import com.raytheon.uf.edex.plugin.text.TextDecoder;
import com.raytheon.uf.edex.plugin.text.db.TextDB;

/**
 * Handles StdTextProductServerRequest by placing them onto the LDAD queue and
 * sending product alarm alerts as specified by the request
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 19May2010               cjeanbap    Initial creation
 * 07Jul2010    2187       cjeanbap    Submit correct Afos Id.
 * 07Jul2010    2187       cjeanbap    Submit AfosId to AlarmAlertUtil.
 * 02Aug2010    2187       cjeanbap    Move AlarmAlertUtil.sendProductAlarmAlert() 
 *                                     outside of if-statement.
 * May 12, 2014 2536       bclement    removed unused import
 * Sep 30, 2015 4860       skorolev    Corrected misspelling.
 * 
 * </pre>
 * 
 * @author cjeanbap
 * @version 1.0
 */

public class StdTextProductRequestHandler implements
        IRequestHandler<StdTextProductServerRequest> {

    private final TextDB dao;

    public StdTextProductRequestHandler() {
        dao = new TextDB();
    }

    @Override
    public Object handleRequest(StdTextProductServerRequest request)
            throws Exception {
        Date d = new Date();
        String wmoid = request.getWmoid();
        String site = request.getSite();
        String cccid = request.getCccid();
        String nnnid = request.getNnnid();
        String xxxid = request.getXxxid();
        String hdrtime = request.getHdrtime();
        String bbbid = request.getBbbid();
        Long createtime = request.getCreatetime();
        String product = request.getProduct();
        boolean operationalFlag = request.isOperationalFlag();

        StdTextProduct text = (operationalFlag ? new OperationalStdTextProduct()
                : new PracticeStdTextProduct());
        text.setWmoid(wmoid);
        text.setSite(site);
        text.setCccid(cccid);
        text.setNnnid(nnnid);
        text.setXxxid(xxxid);
        text.setHdrtime(hdrtime);
        text.setBbbid(bbbid);
        if (createtime == null) {
            createtime = new Date().getTime();
        }
        text.setRefTime(createtime);
        text.setProduct(product);
        boolean success = dao.writeProduct(text);

        if (success) {
            String afosId = text.getCccid() + text.getNnnid() + text.getXxxid();
            if (operationalFlag) {
                TextDecoder.sendTextToQueue(afosId);
            }

            AlarmAlertUtil.sendProductAlarmAlert(afosId, new Date(createtime),
                    operationalFlag);
        }
        return d;
    }
}
