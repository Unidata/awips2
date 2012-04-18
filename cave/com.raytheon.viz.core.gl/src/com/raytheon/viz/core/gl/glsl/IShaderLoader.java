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

package com.raytheon.viz.core.gl.glsl;

/**
 * Define common interface for vertex and fragement shaders.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  
 * 
 * </pre>
 * 
 * @author estrabal
 * @version 1.0
 */

import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.IGLTarget;

public interface IShaderLoader {

    /**
     * 
     * @return
     */
    public abstract String getName();

    /**
     * 
     * @param name
     */
    public abstract void setName(String name);

    /**
     * load shader variables using IImage and PaintProperties
     * 
     * @param target
     * @param program
     * @param image
     * @param paintProps
     * @throws VizException
     */
    public abstract void loadData(IGLTarget target, GLShaderProgram program,
            IImage image, PaintProperties paintProps) throws VizException;
}
