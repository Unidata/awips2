package com.raytheon.uf.edex.plugin.text.handler;

import com.raytheon.uf.common.dataplugin.text.db.TextProductInfo;
import com.raytheon.uf.common.dataplugin.text.request.TextProductInfoCreateRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.plugin.text.dao.TextProductInfoDao;

/**
 * TODO Add Description Register request and handler beans in text-request.xml.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 19May2010               cjeanbap    Initial creation
 * 02Aug2010    2187       cjeanbap    Move AlarmAlertUtil.sendProductAlarmAlert() 
 *                                     outside of if-statement.
 * 
 * </pre>
 * 
 * @author cjeanbap
 * @version 1.0
 */
public class TextProductInfoCreateHandler implements
        IRequestHandler<TextProductInfoCreateRequest> {

    private TextProductInfoDao textDao = new TextProductInfoDao();

    @Override
    public Object handleRequest(TextProductInfoCreateRequest request)
            throws Exception {
        Boolean rval = Boolean.TRUE;

        String cccid = request.getCccid();
        String nnnid = request.getNnnid();
        String xxxid = request.getXxxid();
        TextProductInfo info = textDao.find(cccid, nnnid, xxxid);
        if (info == null) {
            info = new TextProductInfo(cccid, nnnid, xxxid);
            rval = textDao.write(info);
        }

        return rval;
    }
}
