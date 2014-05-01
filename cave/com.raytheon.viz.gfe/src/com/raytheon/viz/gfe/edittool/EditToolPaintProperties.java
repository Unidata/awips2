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
package com.raytheon.viz.gfe.edittool;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.viz.gfe.core.DataManager;

/**
 * A set of properties that are passed to any edit tools.
 * 
 * This extends the regular paint properties to pass GFE-specific properties to
 * the tools.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/01/2008              chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class EditToolPaintProperties extends PaintProperties {

    private IDescriptor descriptor;

    private DataManager dataManager;

    public EditToolPaintProperties(PaintProperties props) {
        super(props);
        if (props instanceof EditToolPaintProperties) {
            EditToolPaintProperties eprops = ((EditToolPaintProperties) props);
            this.descriptor = eprops.descriptor;
            this.dataManager = eprops.dataManager;
        }
    }

    /**
     * @return the descriptor
     */
    public IDescriptor getDescriptor() {
        return descriptor;
    }

    /**
     * @param descriptor
     *            the descriptor to set
     */
    public void setDescriptor(IDescriptor descriptor) {
        this.descriptor = descriptor;
    }

    /**
     * @return the dataManager
     */
    public DataManager getDataManager() {
        return dataManager;
    }

    /**
     * @param dataManager
     *            the dataManager to set
     */
    public void setDataManager(DataManager dataManager) {
        this.dataManager = dataManager;
    }

}
