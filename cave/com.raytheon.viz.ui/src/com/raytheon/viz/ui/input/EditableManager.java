/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package com.raytheon.viz.ui.input;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;

/**
 * Class for managing editableness of resources on a display container
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class EditableManager {

    private static Map<IDisplayPaneContainer, EditableManager> managerMap = new HashMap<IDisplayPaneContainer, EditableManager>();

    private IDisplayPaneContainer container;

    private EditableManager(IDisplayPaneContainer container) {
        this.container = container;
    }

    public static void makeEditable(AbstractVizResource<?, ?> rsc,
            boolean editable) {
        IDisplayPaneContainer container = rsc.getResourceContainer();
        if (container != null) {
            EditableManager mgr = managerMap.get(rsc.getResourceContainer());
            if (mgr == null) {
                mgr = new EditableManager(container);
                managerMap.put(container, mgr);
            }
            mgr.makeEditableInternal(rsc, editable);
        }
    }

    private void makeEditableInternal(AbstractVizResource<?, ?> rsc,
            boolean editable) {
        if (rsc.getCapability(EditableCapability.class).isEditable() == editable
                && editable == false) {
            return;
        }

        for (IDisplayPane pane : container.getDisplayPanes()) {
            for (ResourcePair pair : pane.getDescriptor().getResourceList()) {
                if (pair.getResource() != null
                        && pair.getResource().hasCapability(
                                EditableCapability.class)) {
                    if (pair.getResource().getResourceData()
                            .equals(rsc.getResourceData()) == false) {
                        pair.getResource()
                                .getCapability(EditableCapability.class)
                                .setEditable(false);
                    } else {
                        pair.getResource()
                                .getCapability(EditableCapability.class)
                                .setEditable(editable);
                    }
                }
            }
        }
    }

}
