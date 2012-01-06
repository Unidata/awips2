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

package com.raytheon.viz.gfe.actions;

import com.raytheon.viz.ui.cmenu.ChangeColorMapAction;

/**
 * 
 * Dialog action for setting color map range.
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *  Nov 15, 2011    7890       ryu         Initial Creation.
 * 
 * </pre>
 * 
 * @author ryu
 * @version 1
 */
public class SetColorMapRange extends ChangeColorMapAction {

    public SetColorMapRange() {
        super("Set Range...");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.cmenu.AbstractRightClickAction#isHidden()
     */
    @Override
    public boolean isHidden() {
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.cmenu.AbstractRightClickAction#getOverriddenActionClass
     * ()
     */
    // @Override
    // public Class<? extends AbstractRightClickAction>
    // getOverriddenActionClass() {
    // if (getSelectedRsc() instanceof GFEResource) {
    // return ChangeColorMapAction.class;
    // }
    // return null;
    // }
    //
}
