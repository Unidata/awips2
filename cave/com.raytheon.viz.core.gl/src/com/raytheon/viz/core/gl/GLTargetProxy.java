package com.raytheon.viz.core.gl;

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

import org.eclipse.swt.widgets.Canvas;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.internal.GLTarget;

/**
 * Provide an interface proxy to GLTarget
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Apr 6, 2007              chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class GLTargetProxy {

    /**
     * Hide Constructor
     * 
     */
    private GLTargetProxy() {

    }

    /**
     * Construct a new GLTarget
     * 
     * This generally should not be called. This is intended to be an accessor
     * for other internal components.
     * 
     * @param canvas
     * @param width
     * @param height
     */
    public static GLTarget constructGLTarget(Canvas canvas, float width,
            float height) throws VizException {
        return new GLTarget(canvas, width, height);
    }

    /**
     * Construct a new GLTarget for offscreen rendering
     * 
     * @param width
     * @param height
     * @return
     * @throws VizException
     */
    public static GLTarget constructOffScreenTarget(float width, float height)
            throws VizException {
        return new GLTarget(width, height);
    }
}
