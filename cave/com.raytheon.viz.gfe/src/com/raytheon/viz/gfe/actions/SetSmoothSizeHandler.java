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

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.core.DataManagerUIFactory;

/**
 * Set Smooth Size Handler
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 21, 2010           randerso  Initial creation
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 *
 * </pre>
 *
 * @author randerso
 */

public class SetSmoothSizeHandler extends AbstractHandler
        implements IElementUpdater {

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        String size = event.getParameter("size");

        try {
            int currentSize = Integer.parseInt(size);
            GFEPreference.setPreference("SmoothSize", currentSize);

            DataManagerUIFactory.getCurrentInstance().getParmOp()
                    .setSmoothSize(currentSize);
        } catch (NumberFormatException e) {
            Activator.getDefault().getLog().log(new Status(IStatus.ERROR,
                    Activator.PLUGIN_ID, "Invalid smooth size: " + size));
        }
        return null;
    }

    @Override
    public void updateElement(UIElement element,
            @SuppressWarnings("rawtypes") Map parameters) {

        int currentSize = GFEPreference.getInt("SmoothSize");

        int size = Integer.parseInt((String) parameters.get("size"));
        element.setChecked(currentSize == size);
    }
}
