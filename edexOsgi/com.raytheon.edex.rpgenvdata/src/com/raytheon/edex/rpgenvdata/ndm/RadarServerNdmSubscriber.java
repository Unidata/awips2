package com.raytheon.edex.rpgenvdata.ndm;

import java.io.File;

import com.raytheon.edex.rpgenvdata.RcmClient;
import com.raytheon.rcm.mqsrvr.ReqObj.SendConfigFile;
import com.raytheon.uf.common.site.ingest.INationalDatasetSubscriber;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Sends NDM configuration files to the RadarServer.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2014-02-03   DR 14762    D. Friedman Created.
 * </pre>
 * 
 */
public class RadarServerNdmSubscriber implements INationalDatasetSubscriber {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarServerNdmSubscriber.class);

    // TODO: duplicate..
    private class MyRcmClient extends RcmClient {

        @Override
        public void onFailure(String detail, Exception e) {
            statusHandler.handle(Priority.SIGNIFICANT, detail, e);
        }
    }

    @Override
    public void notify(String fileName, File file) {
        MyRcmClient rcmClient = null;
        try {
            try {
                rcmClient = new MyRcmClient();
                rcmClient.initialize();
                SendConfigFile req = new SendConfigFile();
                req.fileName = file.getName();
                req.fileData = FileUtil.file2bytes(file);
                rcmClient.sendCheckedAndHandled(req);
            } catch (Exception e) {
                // TODO: maybe rely on caller exception handling?
                rcmClient = null;
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Error initializing RadarServer connection", e);
            }
        } finally {
            if (rcmClient != null)
                rcmClient.close();
        }
    }
}
