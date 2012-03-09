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

package com.raytheon.uf.viz.core.drawables.ext;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.ui.services.IDisposable;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.Activator;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.jobs.JobPool;

/**
 * Class that loads data for AbstractGLImages asynchronously
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
public class TextureLoader {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TextureLoader.class);

    /** The instance */
    private static TextureLoader instance;

    private JobPool loaderPool;

    private List<IImage> texturesToLoad;

    /**
     * Get the currently running instance of the texture loader
     * 
     * @return
     */
    public static synchronized TextureLoader getInstance() {
        if (instance == null) {
            instance = new TextureLoader();
        }
        return instance;
    }

    /**
     * Use getInstance() instead of constructor
     * 
     */
    private TextureLoader() {
        this.texturesToLoad = new ArrayList<IImage>();
        this.loaderPool = new JobPool("Texture Loader", Runtime.getRuntime()
                .availableProcessors(), true);
        // Make sure we get shutdown properly
        Activator.getDefault().registerDisposable(new IDisposable() {
            @Override
            public void dispose() {
                shutdown();
            }
        });
    }

    /**
     * Request an image to be loaded
     * 
     * @param img
     *            the image
     */
    public void requestLoad(final IImage img) {
        if (!texturesToLoad.contains(img)) {
            texturesToLoad.add(img);
            loaderPool.schedule(new Runnable() {
                @Override
                public void run() {
                    try {
                        try {
                            img.stage();
                        } catch (Throwable t) {
                            statusHandler.handle(
                                    Priority.PROBLEM,
                                    "Error staging texture: "
                                            + t.getLocalizedMessage(), t);
                        }
                    } finally {
                        texturesToLoad.remove(img);
                    }
                }
            });
        }
    }

    /**
     * Request the job to be shut down
     * 
     */
    public void shutdown() {
        loaderPool.cancel();
    }

}
