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
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.media.opengl.GL;

/**
 * 
 * This class uses WeakReference to guarantee that all vbos are deallocated even
 * if dispose is never called. It is still better to call dispose to avoid
 * having "dead" vbos on the graphics card, which can slow down rendering.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 9, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GLVBOCleaner extends WeakReference<Object> {

    /**
     * This is needed so when unallocate is called we can set the vboId to -1
     * and stop it from being redeleted
     */
    private static List<GLVBOCleaner> cleaners = new LinkedList<GLVBOCleaner>();

    /**
     * THis queue will contain all garbage collected vbo objects, if everyone
     * deallocates all vboIds will be -1.
     */
    private static ReferenceQueue<Object> queue = new ReferenceQueue<Object>();

    /**
     * get a new vboId from gl which will be linked to object, if object is
     * garbage collected then the vbo will still be freed. This should only be
     * called on the main thread.
     * 
     * @param object
     * @param target
     * @return
     */
    public static int allocateVBO(Object object, IGLTarget target) {
        target.makeContextCurrent();
        clean(target.getGl());
        GLVBOCleaner cleaner = new GLVBOCleaner(object, target.getGl());

        target.handleError(target.getGl().glGetError());

        // verify successful
        if (cleaner.vboId > 0) {
            cleaners.add(cleaner);
        }
        return cleaner.vboId;
    }

    /**
     * Immediately free a vbo which has been allocated, any method that uses
     * allocate should also use deallocate. object must be the same as the
     * object passed to allocate.
     * 
     * @param object
     * @param vboId
     * @param target
     */
    public static void unallocateVBO(Object object, int vboId, IGLTarget target) {
        target.makeContextCurrent();
        clean(target.getGl());
        if (vboId <= 0) {
            return;
        }
        Iterator<GLVBOCleaner> iter = cleaners.iterator();
        while (iter.hasNext()) {
            GLVBOCleaner cleaner = iter.next();
            if (cleaner.get() == object && cleaner.vboId == vboId) {
                cleaner.freeVbo(target.getGl());
                iter.remove();
            }
        }
    }

    private static void clean(GL gl) {
        GLVBOCleaner cleaner = (GLVBOCleaner) queue.poll();
        while (cleaner != null) {
            if (cleaner.vboId > 0) {
                System.err
                        .println("An object with a gl vertex buffer object has been garbage collected without being disposed. This can cause a delay in freeing vbos which can cause severe performance problems.");
                cleaner.freeVbo(gl);
            }
            cleaners.remove(cleaner);
            cleaner = (GLVBOCleaner) queue.poll();
        }
    }

    private int vboId;

    private GLVBOCleaner(Object referent, GL gl) {
        super(referent, queue);
        int[] vbos = new int[1];
        gl.glGenBuffers(1, vbos, 0);
        this.vboId = vbos[0];
    }

    private void freeVbo(GL gl) {
        gl.glDeleteBuffers(1, new int[] { this.vboId }, 0);
        this.vboId = -1;
    }

}
