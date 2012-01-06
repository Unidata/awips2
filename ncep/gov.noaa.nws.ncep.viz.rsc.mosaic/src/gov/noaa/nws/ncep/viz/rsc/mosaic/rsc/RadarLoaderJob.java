package gov.noaa.nws.ncep.viz.rsc.mosaic.rsc;

import gov.noaa.nws.ncep.edex.plugin.mosaic.uengine.MosaicTiler;

import java.awt.Rectangle;
import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.data.prep.CMDataPreparerManager;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IImage;

/**
 * Provide Radar Mosaic raster rendering support 
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer     Description
 *  ------------ ----------  -----------  --------------------------
 *  01/2010	  	   204 	 	  M. Li       Initial Creation.
 *  03/2010                   B. Hebbard  Port TO11D6->TO11DR3; CMDataPreparerManager replaces AbstractNumDataPreparer
 * 
 * </pre>
 * 
 * @author mli
 * @version 1
 */

public class RadarLoaderJob extends Job {

    private static RadarLoaderJob instance;

    private ConcurrentLinkedQueue<Request> requests;

    private ConcurrentHashMap<Request, IImage> servicedRequests;

    private boolean run;

    private RadarLoaderJob() {
        super("Radar Mosaic Loader Job");
        this.requests = new ConcurrentLinkedQueue<Request>();
        this.servicedRequests = new ConcurrentHashMap<Request, IImage>();

    }

    public static synchronized RadarLoaderJob getInstance() {
        if (instance == null) {
            instance = new RadarLoaderJob();
            instance.setSystem(true);
            instance.schedule();
        }
        return instance;
    }

    public void shutdown() {
        run = false;
    }

    private class Request {
        public int i;

        public int j;

        public int level;

        public IGraphicsTarget target;

        public MosaicTiler tiler;

        public ColorMapParameters colorMapParameters;

        public int[] dims;

        public int tileSize;

        public Request(int i, int j, int level, int tileSize,
                IGraphicsTarget target, MosaicTiler tiler,
                ColorMapParameters colorMapParameters, int[] dims) {
            super();
            this.i = i;
            this.j = j;
            this.level = level;
            this.tileSize = tileSize;
            this.target = target;
            this.tiler = tiler;
            this.colorMapParameters = colorMapParameters;
            this.dims = dims;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime
                    * result
                    + ((colorMapParameters == null) ? 0 : colorMapParameters
                            .hashCode());
            result = prime * result + Arrays.hashCode(dims);
            result = prime * result + i;
            result = prime * result + j;
            result = prime * result + level;
            result = prime * result
                    + ((target == null) ? 0 : target.hashCode());
            result = prime * result + ((tiler == null) ? 0 : tiler.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final Request other = (Request) obj;
            if (colorMapParameters == null) {
                if (other.colorMapParameters != null) {
                    return false;
                }
            } else if (!colorMapParameters.equals(other.colorMapParameters)) {
                return false;
            }
            if (!Arrays.equals(dims, other.dims)) {
                return false;
            }
            if (i != other.i) {
                return false;
            }
            if (j != other.j) {
                return false;
            }
            if (level != other.level) {
                return false;
            }
            if (target == null) {
                if (other.target != null) {
                    return false;
                }
            } else if (!target.equals(other.target)) {
                return false;
            }
            if (tiler == null) {
                if (other.tiler != null) {
                    return false;
                }
            } else if (!tiler.equals(other.tiler)) {
                return false;
            }
            return true;
        }

    }

    public IImage requestLoad(int i, int j, int level, int tileSize,
            IGraphicsTarget target, MosaicTiler tiler,
            ColorMapParameters colorMapParameters, int[] dims) {
        Request request = new Request(i, j, level, tileSize, target, tiler,
                colorMapParameters, dims);
        IImage response = servicedRequests.get(request);
        if (response != null) {
            servicedRequests.remove(request);
            return response;
        }

        if (!requests.contains(request)) {
            requests.add(request);
        }

        return null;

    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        run = true;

        while (run == true) {

            while (!requests.isEmpty()) {

                Request req = requests.peek();
                // long t0 = System.currentTimeMillis();
                ByteBuffer byteBuffer = req.tiler.createTile(req.i, req.j,
                        req.level, true);
                // long t = System.currentTimeMillis() - t0;

                byteBuffer.rewind();

                servicedRequests.put(req,
                		req.target.initializeRaster(
                		        CMDataPreparerManager.getDataPreparer(byteBuffer,
                                        new Rectangle(req.tiler.getWidth(), req.tiler.getHeight()),
                                        null),
                                req.colorMapParameters));
                requests.remove();
            }

            try {
                Thread.sleep(10);
            } catch (InterruptedException e) {
                // ignore
            }
        }
        return null;
    }
}
