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
package com.raytheon.uf.common.cache;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 8, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class DiskCacheWriter extends Thread {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DiskCacheWriter.class.getPackage().getName(), "CAVE",
                    "WORKSTATION");

    protected boolean run = true;

    protected int maxPending = 25;

    protected LinkedBlockingQueue<String> pendingWrites = new LinkedBlockingQueue<String>();

    /**
     * Keep the ids as a separate set to allow quick look ups of any pending
     * writes.
     */
    private Map<String, PendingWrite> dataMap = new HashMap<String, PendingWrite>();

    private class PendingWrite {
        Object dataObj;

        Object syncObj;

        public PendingWrite(Object dataObj, Object syncObj) {
            this.dataObj = dataObj;
            this.syncObj = syncObj;
        }
    }

    public void asyncWrite(String path, Object dataObj, Object syncObj) {
        synchronized (dataMap) {
            while (dataMap.size() > maxPending) {
                try {
                    dataMap.wait();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }

            dataMap.put(path, new PendingWrite(dataObj, syncObj));
        }

        pendingWrites.offer(path);
    }

    public Object cancelWrite(String path) {
        Object rval = null;
        synchronized (dataMap) {
            PendingWrite pw = dataMap.remove(path);
            if (pw != null) {
                rval = pw.dataObj;
                dataMap.notifyAll();
                pw.dataObj = null;
            }
        }

        return rval;
    }

    @Override
    public void run() {
        while (run) {
            try {
                String path = pendingWrites.poll(1, TimeUnit.MINUTES);
                if (path != null) {
                    PendingWrite pw = null;

                    synchronized (dataMap) {
                        pw = dataMap.remove(path);
                    }

                    // verify write wasn't cancelled
                    if (pw != null) {
                        try {
                            synchronized (pw.syncObj) {
                                File f = new File(path);

                                if (pw.dataObj != null) {
                                    // serialize object
                                    byte[] data = SerializationUtil
                                            .transformToThrift(pw.dataObj);

                                    // write data to disk
                                    FileUtil.bytes2File(data, f);
                                    f.deleteOnExit();
                                } else {
                                    // delete file
                                    f.delete();
                                }
                            }
                        } finally {
                            synchronized (dataMap) {
                                dataMap.notifyAll();
                            }
                        }
                    }
                }
            } catch (Throwable e) {
                statusHandler.handle(Priority.ERROR,
                        "Error occurred writing data to disk cache", e);
            }
        }
    }
}
