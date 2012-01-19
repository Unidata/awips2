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
package com.raytheon.viz.radar.rsc;

import com.raytheon.uf.viz.core.drawables.PaintProperties;

/**
 * Paint properties for mosaic, includes a force repaint argument
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 19, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class MosaicPaintProperties extends PaintProperties {

    private boolean forceRepaint = false;

    /**
     * 
     * @param paintProps
     * @param forceRepaint
     */
    public MosaicPaintProperties(PaintProperties paintProps,
            boolean forceRepaint) {
        super(paintProps);
        this.forceRepaint = forceRepaint;
    }

    /**
     * 
     * @param paintProps
     */
    public MosaicPaintProperties(PaintProperties paintProps) {
        super(paintProps);
    }

    public boolean isForceRepaint() {
        return forceRepaint;
    }

    public void setForceRepaint(boolean forceRepaint) {
        this.forceRepaint = forceRepaint;
    }

}
