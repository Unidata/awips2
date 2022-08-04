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
package com.raytheon.viz.gfe.rsc.colorbar;

import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 1, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public interface IColorBarDisplay extends IRenderable {
    public void dispose();

    /**
     * Get the value at a certain coordinate.
     * 
     * @param coord
     * @param mouseButton
     * @return
     */
    public WxValue getValueAt(double[] coord, int mouseButton);
}
