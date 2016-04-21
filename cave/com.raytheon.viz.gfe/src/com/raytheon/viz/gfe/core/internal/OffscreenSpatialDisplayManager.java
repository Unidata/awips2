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
package com.raytheon.viz.gfe.core.internal;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.viz.gfe.core.DataManager;

/**
 * Spatial display manager for working offscreen without a display, i.e. with
 * IFP Image
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 30, 2009            njensen     Initial creation
 * Apr 09, 2009 1288       rjpeter     Added new method stubs for ISpatialDisplayManager.
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class OffscreenSpatialDisplayManager extends
        AbstractSpatialDisplayManager {

    private IRenderableDisplay display;

    public OffscreenSpatialDisplayManager(IRenderableDisplay display,
            DataManager mgr) {
        super(mgr);
        this.display = display;
    }

    @Override
    protected IDescriptor[] getDescriptors() {
        return new IDescriptor[] { display.getDescriptor() };
    }

}
