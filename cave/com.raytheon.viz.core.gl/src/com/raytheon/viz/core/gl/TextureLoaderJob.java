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

package com.raytheon.viz.core.gl;

import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.LinkedBlockingQueue;

import javax.media.opengl.GLContext;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.images.AbstractGLImage;

/**
 * Job that demand-loads textures into GL.
 * <P>
 * 
 * If a multiple ImageIO.reads are incurred simultaneously, the performance is
 * horrible. By having only one texture load occur at a time in a different
 * (non-blocking) thread, with adequate pause between texture loads, this
 * problem is alleviated.
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------	----------	-----------	--------------------------
 *    7/1/06                    chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * 
 */
public class TextureLoaderJob extends Job {

    /** Number of things to keep on the queue before starting to remove some */
    private static final int MAX_QUEUE_SIZE = 256;

    /** The instance */
    private static TextureLoaderJob instance;

    /** Time (in ms) for the texture job to sleep (NEVER SET TO ZERO) */
    private static final int TEXTURE_JOB_SLEEP_TIME = 2;

    /** A flag to indicate whether the job should continue running */
    private boolean isRunning = true;

    /** A queue of images to load */
    private final Queue<AbstractGLImage> texturesToLoad;

    /** A thread safe list of images to load */
    private final ConcurrentLinkedQueue<Request> texturesToLoadToGPU;

    /**
     * Internal datastructure: used to populate the queues
     */
    private class Request {
        public AbstractGLImage image;

        public GLContext ctx;
    }

    /**
     * Get the currently running instance of the texture loader
     * 
     * @return
     */
    public static TextureLoaderJob getInstance() {
        if (instance == null) {
            instance = new TextureLoaderJob();
        }
        return instance;
    }

    /**
     * Use getInstance() instead of constructor
     * 
     */
    private TextureLoaderJob() {
        super("Texture Loader");
        this.texturesToLoad = new LinkedBlockingQueue<AbstractGLImage>();
        this.texturesToLoadToGPU = new ConcurrentLinkedQueue<Request>();
    }

    /**
     * Request an image to be loaded
     * 
     * @param img
     *            the image
     */
    public void requestLoad(AbstractGLImage img) {
        if (!this.texturesToLoad.contains(img))
            this.texturesToLoad.add(img);
    }

    public void requestLoadIntoTexture(AbstractGLImage img, GLContext ctx) {
        Request req = new Request();
        req.image = img;
        req.ctx = ctx;

        this.texturesToLoadToGPU.add(req);

    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {

        while (this.isRunning) {
            while (this.texturesToLoad.size() > MAX_QUEUE_SIZE) {
                this.texturesToLoad.poll();
            }

            while (this.texturesToLoad.size() > 0) {
                AbstractGLImage image = this.texturesToLoad.remove();
                if (image.getStatus() == com.raytheon.uf.viz.core.drawables.IImage.Status.STAGED
                        || image.getStatus() == com.raytheon.uf.viz.core.drawables.IImage.Status.LOADED) {
                    continue;
                }
                try {
                    image.stageTexture();
                } catch (VizException e) {
                    Activator
                            .getDefault()
                            .getLog()
                            .log(new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                                    "Staging texture failed", e));
                }
            }
            try {
                Thread.sleep(TEXTURE_JOB_SLEEP_TIME);
            } catch (InterruptedException e) {
            }

            // while (this.texturesToLoadToGPU.size() > MAX_QUEUE_SIZE) {
            // this.texturesToLoadToGPU.remove();
            // }
            //
            // while (this.texturesToLoadToGPU.size() > 0) {
            // final Request request = this.texturesToLoadToGPU.remove();
            //
            // if (request.image.getStatus() == IImage.Status.LOADED
            // || request.image.getStatus() == IImage.Status.FAILED) {
            // continue;
            // }
            // VizApp.runSync(new Runnable() {
            //
            // public void run() {
            // try {
            // request.image.loadTexture(request.ctx);
            // } catch (VizException e) {
            // // Set an exception that the drawing subsystem will
            // // pick
            // // up and log properly/display to user
            // request.image.setStatus(IImage.Status.FAILED);
            // request.image.setFailedMessage(e);
            // }
            // }
            //
            // });
            //
            // }
            // try {
            // Thread.sleep(TEXTURE_JOB_SLEEP_TIME);
            // } catch (InterruptedException e) {
            // }

        }
        return Status.OK_STATUS;
    }

    /**
     * Request the job to be shut down
     * 
     */
    public void shutdown() {
        this.isRunning = false;
    }

}
