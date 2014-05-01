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

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.LinkedBlockingQueue;

import javax.media.opengl.GL;

/**
 * 
 * This class provides a convenient way of disposing of GL resources on an
 * active GLContext.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GLDisposalManager {

    private static ReferenceQueue<Object> refQueue = new ReferenceQueue<Object>();

    private static List<GLAutoDisposer> autoDisposers = new LinkedList<GLAutoDisposer>();

    private static Queue<GLDisposer> disposeQueue = new LinkedBlockingQueue<GLDisposer>();

    /**
     * This method should be used to dispose of a GL resource, at the end of a
     * frame the target will make the context current and dispose of all
     * resources.
     * 
     * @param disposer
     *            a disposer object that is ready to dispose of a gl resource.
     */
    private static void dispose(GLDisposer disposer) {
        disposeQueue.add(disposer);
    }

    /**
     * Provide a backup mechanism to dispose. This uses WeakReference objects to
     * call the disposer after the object is garbage collected. For this to work
     * the disposer must have no references to object. Object should be the only
     * thing using these GL resources, this will not work for anything which
     * might be shared by multiple objects. This will also result in the dispose
     * method of the disposer being called more than once so it should clear any
     * 
     * @param disposer
     *            - a disposer that will be called when object is garbage
     *            collected
     * @param object
     *            - an object which uses a gl resource.
     */
    public static void autoDispose(GLDisposer disposer, Object object) {
        autoDisposers.add(new GLAutoDisposer(object, disposer));
    }

    /**
     * For use by the target only, the target should call this to dispose all
     * unneeded resources
     * 
     * @param gl
     */
    public static void performDispose(GL gl) {
        GLDisposer disposer = disposeQueue.poll();
        while (disposer != null) {
            disposer.dispose(gl);
            disposer = disposeQueue.poll();
        }
        GLAutoDisposer autoDisposer = (GLAutoDisposer) refQueue.poll();
        while (autoDisposer != null) {
            autoDisposers.remove(autoDisposer);
            autoDisposer.disposer.dispose();
            autoDisposer = (GLAutoDisposer) refQueue.poll();
        }
    }

    public static abstract class GLDisposer {
        protected abstract void dispose(GL gl);

        final public void dispose() {
            GLDisposalManager.dispose(this);
        }
    }

    private static class GLAutoDisposer extends WeakReference<Object> {

        private final GLDisposer disposer;

        public GLAutoDisposer(Object referent, GLDisposer disposer) {
            super(referent, refQueue);
            this.disposer = disposer;
        }

    }

}
