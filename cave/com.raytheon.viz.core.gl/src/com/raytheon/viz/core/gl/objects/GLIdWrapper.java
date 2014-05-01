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
package com.raytheon.viz.core.gl.objects;

import javax.media.opengl.GL;
import javax.media.opengl.glu.GLU;

import com.raytheon.viz.core.gl.GLDisposalManager;
import com.raytheon.viz.core.gl.GLDisposalManager.GLDisposer;

/**
 * 
 * Most objects in GL are represented in Java as an integer id. This class
 * provides some of the generic code needed to manage these and dispose of these
 * GL Objects
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
public abstract class GLIdWrapper extends GLDisposer {

    protected int id;

    public GLIdWrapper(Object parent) {
        this();
        GLDisposalManager.autoDispose(this, parent);
    }

    public GLIdWrapper() {
        int[] arr = new int[1];
        genId(GLU.getCurrentGL(), arr);
        this.id = arr[0];
    }

    public boolean isValid() {
        return id != -1;
    }

    public int getId() {
        return id;
    }

    @Override
    protected void dispose(GL gl) {
        if (id != -1) {
            deleteId(gl, new int[] { id });
        }
        this.id = -1;
    }

    /**
     * Generate an id for this object, arr is an array of length 1 and id should
     * be set to the only element in the array
     * 
     * @param gl
     * @param arr
     */
    protected abstract void genId(GL gl, int[] arr);

    /**
     * Delete an id for this object, arr is an array of length 1 and will
     * contain the id
     * 
     * @param gl
     * @param arr
     */
    protected abstract void deleteId(GL gl, int[] arr);

}
