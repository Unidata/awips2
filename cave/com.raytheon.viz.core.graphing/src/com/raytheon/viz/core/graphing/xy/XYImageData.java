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
package com.raytheon.viz.core.graphing.xy;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IImage;

/**
 * XYData that contains an image to draw
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 15, 2007            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class XYImageData extends XYData {

    protected IImage image;

    protected RGB color;

    protected IGraphicsTarget target;

    public XYImageData(Object ax, Object ay) {
        super(ax, ay);
    }

    /**
     * @return the iImage
     */
    public IImage getImage() {
        if (image == null && target != null && color != null) {
            generateImage();
        }
        return image;
    }

    public void setColor(RGB color) {
        if (this.color == null || !this.color.equals(color)) {
            this.color = color;
            this.dispose();
        }
    }

    public void setTarget(IGraphicsTarget target) {
        if (this.target == null || !this.target.equals(target)) {
            this.target = target;
            this.dispose();
        }
    }

    public void dispose() {
        if (image != null) {
            image.dispose();
            image = null;
        }
    }

    protected abstract void generateImage();

    public abstract int[] getDefaultSize();

}
