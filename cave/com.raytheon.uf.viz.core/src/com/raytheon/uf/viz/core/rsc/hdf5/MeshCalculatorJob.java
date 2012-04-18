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
package com.raytheon.uf.viz.core.rsc.hdf5;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.IMeshCallback;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory;

/**
 * Persistent job used to calculate mesh tiles outside of the rendering thread
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Mar 1, 2007              chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author cnh
 * @version 1
 */
public class MeshCalculatorJob extends Job {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(MeshCalculatorJob.class);

    private static MeshCalculatorJob instance;

    private ConcurrentLinkedQueue<PixelCoverage> requests;

    private ConcurrentHashMap<PixelCoverage, MeshParameters> map;

    private MeshCalculatorJob() {
        super("Mesh Calculator");
        requests = new ConcurrentLinkedQueue<PixelCoverage>();
        map = new ConcurrentHashMap<PixelCoverage, MeshParameters>();
    }

    /**
     * Request to have the mesh be calculated in the mesh thread
     * 
     * @param mesh
     * @param tile
     * @param preTransform
     */
    public synchronized void requestLoad(IMesh mesh, ImageTile tile,
            MathTransform preTransform) {
        if (!requests.contains(tile.coverage)) {
            MeshParameters params = new MeshParameters();
            params.tile = tile;
            params.mesh = mesh;
            params.preTransform = preTransform;

            map.put(tile.coverage, params);
            requests.add(tile.coverage);
        }
        instance.schedule();
    }

    /**
     * Request to have the mesh be calculated in the mesh thread, with the
     * callback being notified after the mesh has been calculated
     * 
     * @param callback
     * @param mesh
     * @param tile
     * @param preTransform
     */
    public synchronized void requestLoad(IMeshCallback callback, IMesh mesh,
            ImageTile tile, MathTransform preTransform) {
        if (!requests.contains(tile.coverage)) {
            MeshParameters params = new MeshParameters();
            params.tile = tile;
            params.mesh = mesh;
            params.preTransform = preTransform;
            params.callback = callback;

            map.put(tile.coverage, params);
            requests.add(tile.coverage);
        }
        instance.schedule();
    }

    public static synchronized MeshCalculatorJob getInstance() {
        if (instance == null) {
            instance = new MeshCalculatorJob();
            instance.setSystem(true);
        }
        return instance;
    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {

        while (requests.size() > 0) {
            PixelCoverage coverage = requests.peek();
            try {
                MeshParameters p = map.get(coverage);

                coverage.setMesh(null);
                p.mesh.calculateMesh(coverage, p.tile, p.preTransform);
                coverage.setMesh(p.mesh);

                if (p.callback != null) {
                    p.callback.meshCalculated(p.tile);
                }
            } catch (Throwable t) {
                statusHandler.handle(Priority.PROBLEM, "An error occured during mesh calculations, some images may not be properly reprojected.", t);
            }
            map.remove(coverage);
            requests.remove();
        }
        return Status.OK_STATUS;
    }

    private class MeshParameters {
        // public MapDescriptor mapDescriptor;

        public IMesh mesh;

        public ImageTile tile;

        public MathTransform preTransform;

        public IMeshCallback callback;
    }

}
