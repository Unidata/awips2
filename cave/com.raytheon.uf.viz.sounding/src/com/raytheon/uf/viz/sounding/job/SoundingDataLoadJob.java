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
package com.raytheon.uf.viz.sounding.job;

import java.util.List;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.sounding.Activator;

/**
 * Job to load sounding data for popup skewT.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 30, 2011            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SoundingDataLoadJob extends Job {

    private static SoundingDataLoadJob instance;

    private ConcurrentLinkedQueue<LoadSoundingRequest> requestQueue;

    private SoundingDataLoadJob() {
        super("Loading sounding data...");
        this.requestQueue = new ConcurrentLinkedQueue<LoadSoundingRequest>();
    }

    /**
     * Get instance
     * 
     * @return
     */
    public static synchronized SoundingDataLoadJob getInstance() {
        if (instance == null) {
            instance = new SoundingDataLoadJob();
            instance.setSystem(false);
            instance.schedule();
        }

        return instance;
    }

    public void request(LoadSoundingRequest request) {
        this.requestQueue.add(request);

        if (this.getState() != Job.RUNNING) {
            this.schedule();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {

        LoadSoundingRequest req;
        while ((req = this.requestQueue.poll()) != null) {
            try {
                if (req.isCanceled() || req.getSoundingArr() != null) {
                    ;// request has been canceled or soundings have been loaded
                } else {
                    VerticalSounding[] soundingArr = loadData(req.getObjects());
                    req.setSoundingArr(soundingArr);
                }
            } catch (Throwable e) {
                return new Status(Status.ERROR, Activator.PLUGIN_ID,
                        "Error loading sounding data", e);

            }
        }

        return Status.OK_STATUS;
    }

    private VerticalSounding[] loadData(List<PluginDataObject> pdos) {
        int size = 0;
        long t0 = System.currentTimeMillis();
        try {
            DataCubeContainer.getDataRecords(pdos, Request.ALL, null);
            for (PluginDataObject pdo : pdos) {
                IDataRecord[] drs = (IDataRecord[]) pdo.getMessageData();
                FloatDataRecord fdr = (FloatDataRecord) drs[0];
                float[] data = fdr.getFloatData();
                size = data.length;
                pdo.setMessageData(data);
            }
        } catch (VizException e) {
            e.printStackTrace();
        }
        System.out.println("loadData: " + (System.currentTimeMillis() - t0));
        return new VerticalSounding[size];
    }

}
