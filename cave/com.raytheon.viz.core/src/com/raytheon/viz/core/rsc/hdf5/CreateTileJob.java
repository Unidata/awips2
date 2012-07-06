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
package com.raytheon.viz.core.rsc.hdf5;

import org.apache.commons.collections.keyvalue.MultiKey;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IImage;

/* 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 2, 2009            mschenke     Initial creation
 *
 * </pre>
 *
 * @author mschenke
 */
/**
 * Job for creating tiles
 * 
 * @version 1.0
 */
public class CreateTileJob extends Job implements Runnable {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CreateTileJob.class);

    protected boolean canceled = true;

    private Integer i;

    private Integer j;

    protected Integer level;

    protected IGraphicsTarget target = null;

    protected AbstractTileSet tileSet;

    public CreateTileJob(String name) {
        super(name);
    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        run();
        return org.eclipse.core.runtime.Status.OK_STATUS;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Runnable#run()
     */
    @Override
    public void run() {
        canceled = false;
        try {
            IImage image = tileSet.createTile(target, level, i, j);

            if (image != null) {
                tileSet.addImage(image, level, i, j);
                image.stage();
            } else {
                image = null;
            }
        } catch (Exception e) {
            statusHandler
                    .handle(Priority.SIGNIFICANT, "Error creating tile", e);
        }
        tileSet.remove(new MultiKey(level, i, j));
        canceled = true;
    }

    @Override
    protected void canceling() {
        canceled = true;
        tileSet.cancelRequest(level, i, j);
    }

    public void setI(Integer i) {
        this.i = i;
    }

    public void setJ(Integer j) {
        this.j = j;
    }

    public void setLevel(Integer level) {
        this.level = level;
    }

    public void setTarget(IGraphicsTarget target) {
        this.target = target;
    }

    public void setTileSet(AbstractTileSet tileSet) {
        this.tileSet = tileSet;
    }

    public boolean isCanceled() {
        return canceled;
    }

}
