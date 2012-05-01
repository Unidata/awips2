/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package gov.noaa.nws.ncep.viz.rsc.ncgrid.contours;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.contours.ContourSupport;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.contours.ContourSupport.ContourGroup;

/**
 * ContourManagerJob
 * 
 * Provides a job that can create contours asynchronously
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Oct 24, 2007             chammack    Initial Creation.
 *    Mar 01, 2010 #164		   M. Li	   Applied to NC Perspective
 *    Feb 15, 2012             X. Guo      Cached contour information
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ContourManagerJob extends Job {

    private static ContourManagerJob instance;

    private final Map<String, Request> requestMap;
    
    private final Map<String, ContourGroup> responseMap;

    private static class Request {

		public Request(IDataRecord record, int level, IExtent pixelExtent,
                double currentDensity, MathTransform worldGridToCRSTransform,
                GeneralGridGeometry imageGridGeometry,
                GeneralGridGeometry mapGridGeometry, IGraphicsTarget target,
                IMapDescriptor descriptor, ContourAttributes attr, String name, float zoom,
                ContourGroup contourGp) {
            super();
            this.record = record;
            this.level = level;
            this.pixelExtent = pixelExtent;
            this.worldGridToCRSTransform = worldGridToCRSTransform;
            this.imageGridGeometry = imageGridGeometry;
            this.mapGridGeometry = mapGridGeometry;
            this.target = target;
            this.descriptor = descriptor;
			this.attr = attr;
            this.zoom = zoom;
            this.currentDensity = currentDensity;
            this.name = name;
            this.contourGroup = contourGp;
        }

        IDataRecord record;

        int level;

        IExtent pixelExtent;

        double currentDensity;

        MathTransform worldGridToCRSTransform;

        GeneralGridGeometry imageGridGeometry;

        GeneralGridGeometry mapGridGeometry;

        IGraphicsTarget target;

        IMapDescriptor descriptor;

        ContourAttributes attr;
        
        String name;

        float zoom;
        
        ContourGroup contourGroup;

    }

    private ContourManagerJob() {
        super("Contouring...");
        this.responseMap = new HashMap<String, ContourGroup>();
        this.requestMap = new HashMap<String, Request>();
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
    public ContourGroup request(String identifier, IDataRecord record,
            int level, IExtent extent, double currentDensity,
            MathTransform worldGridToCRSTransform,
            GeneralGridGeometry imageGridGeometry,
            GeneralGridGeometry mapGridGeometry, IGraphicsTarget target,
            IMapDescriptor descriptor, ContourAttributes attr, String name, float zoom,
            ContourGroup contourGp) {

        synchronized (ContourManagerJob.class) {

            // Response is ready
            if (responseMap.get(identifier) != null) {
                return responseMap.remove(identifier);
            }

            if (responseMap.size() > 5) {
                responseMap.clear();
            }

            // Request is already made, but not serviced yet
            if (requestMap.containsKey(identifier)) {
                return null;
            }

            // Request needs to be made, and will be available asynchronously
            // later
            Request req = new Request(record, level, extent, currentDensity,
                    worldGridToCRSTransform, imageGridGeometry,
                    mapGridGeometry, target, descriptor, attr, name, zoom,contourGp);
            this.requestMap.put(identifier, req);

            if (this.getState() != Job.RUNNING) {
                this.schedule();
            }
        }

        return null;

    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {

        while (!requestMap.isEmpty()) {

            this.setName("Contouring");
            String keyToProcess;
            synchronized (ContourManagerJob.class) {
                keyToProcess = requestMap.keySet().iterator().next();
            }

            Request req = requestMap.get(keyToProcess);
//            try {
/*                long t0 = System.currentTimeMillis();
                ContourSupport.ContourGroup cg = ContourSupport.createContours(req.record,
                        req.level, req.pixelExtent, req.currentDensity,
                        req.worldGridToCRSTransform, req.imageGridGeometry,
                        req.mapGridGeometry, req.target, req.descriptor, req.attr, req.name, req.zoom,
                        req.contourGroup);*/
                ContourSupport cntrSp = new ContourSupport(req.record,
                        req.level, req.pixelExtent, req.currentDensity,
                        req.worldGridToCRSTransform, req.imageGridGeometry,
                        req.mapGridGeometry, req.target, req.descriptor, req.attr, req.name, req.zoom,
                        req.contourGroup);
                ContourSupport.ContourGroup cg = cntrSp.getContours ();
//                System.out.println("Total time taken: "
//                        + (System.currentTimeMillis() - t0));

                synchronized (ContourManagerJob.class) {
                    responseMap.put(keyToProcess, cg);
                    requestMap.remove(keyToProcess);
                }
//            } catch (VizException e) {
//            	System.err.println("Error creating " + req.attr.getGdpfun().trim().toUpperCase() + " contours: ");
//                return new Status(Status.ERROR, Activator.PLUGIN_ID,
//                        "Error creating " + gfunc.toUpperCase() + " contours: ", e);
//                
//            }
            
        }

        return Status.OK_STATUS;
    }
}
