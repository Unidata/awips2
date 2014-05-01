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
package com.raytheon.viz.ui.cmenu;

import org.eclipse.swt.SWT;

import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.viz.ui.input.EditableManager;

/**
 * Right click action for toggling the editableness of the resource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 19, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ToggleEditableAction extends AbstractRightClickAction {

    public ToggleEditableAction() {
        super(SWT.TOGGLE);
    }

    @Override
    public String getText() {
        return "Editable";
    }

    @Override
    public void run() {
        AbstractVizResource<?, ?> rsc = getTopMostSelectedResource();
        if (rsc != null && rsc.hasCapability(EditableCapability.class)) {
            EditableManager.makeEditable(rsc,
                    !rsc.getCapability(EditableCapability.class).isEditable());
        }
    }

    @Override
    public void setSelectedRsc(ResourcePair selectedRsc) {
        super.setSelectedRsc(selectedRsc);
        AbstractVizResource<?, ?> rsc = getTopMostSelectedResource();
        if (rsc != null && rsc.hasCapability(EditableCapability.class)) {
            setChecked(rsc.getCapability(EditableCapability.class).isEditable());
        }
    }

}
