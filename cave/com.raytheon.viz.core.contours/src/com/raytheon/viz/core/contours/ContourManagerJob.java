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
package com.raytheon.viz.core.contours;

import java.util.concurrent.ConcurrentLinkedQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.viz.core.contours.ContourSupport.ContourGroup;

/**
 * ContourManagerJob
 * 
 * Provides a job that can create contours asynchronously
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 * Date          Ticket#   Engineer   Description
 * ------------- -------- ----------- --------------------------
 * Oct 24, 2007           chammack    Initial Creation.
 * Feb 27, 2014  2791     bsteffen    Switch from IDataRecord to DataSource
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ContourManagerJob extends Job {

    private static ContourManagerJob instance;

    private ConcurrentLinkedQueue<ContourCreateRequest> requestQueue;

    private ContourManagerJob() {
        super("Contouring...");
        this.requestQueue = new ConcurrentLinkedQueue<ContourCreateRequest>();
    }

    /**
     * Get instance
     * 
     * @return
     */
    public static synchronized ContourManagerJob getInstance() {
        if (instance == null) {
            instance = new ContourManagerJob();
            instance.setSystem(false);
            instance.schedule();
        }

        return instance;
    }

    /**
     * Request a contour group
     * 
     * @param identifier
     * @param record
     * @param level
     * @param extent
     * @param currentDensity
     * @param worldGridToCRSTransform
     * @param imageGridGeometry
     * @param mapGridGeometry
     * @param target
     * @param descriptor
     * @param prefs
     * @return
     */
    public void request(ContourCreateRequest request) {
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

        ContourCreateRequest req;
        while ((req = this.requestQueue.poll()) != null) {
            this.setName("Contouring");

            try {
                if (req.isCanceled() || req.getContourGroup() != null) {
                    ;// request has been canceled or contours exist
                } else {
                    long t0 = System.currentTimeMillis();
                    ContourGroup cg = null;
                    cg = ContourSupport.createContours(req.getSource(),
                            req.getLevel(), req.getPixelExtent(),
                            req.getCurrentDensity(),
                            req.getCurrentMagnification(),
                            req.getImageGridGeometry(), req.getTarget(),
                            req.getDescriptor(), req.getPrefs(), req.getZoom());
                    // setContourGroup will check if cg needs to be disposed
                    req.setContourGroup(cg);
                    System.out.println("Total time taken: "
                            + (System.currentTimeMillis() - t0));
                }
            } catch (Throwable e) {
                return new Status(Status.ERROR, ContourManagerJob.class
                        .getPackage().getName(),
                        "Error creating contours", e);

            }
        }

        return Status.OK_STATUS;
    }
}
