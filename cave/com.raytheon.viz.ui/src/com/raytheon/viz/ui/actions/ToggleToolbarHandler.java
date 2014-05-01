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
package com.raytheon.viz.ui.actions;

import java.util.Map;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.ui.ISources;
import org.eclipse.ui.internal.handlers.ToggleCoolbarHandler;
import org.eclipse.ui.menus.UIElement;
import org.eclipse.ui.services.IEvaluationService;

/**
 * Handles Toolbar display update events.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 15, 2009            bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

@SuppressWarnings("restriction")
public class ToggleToolbarHandler extends ToggleCoolbarHandler {

    /**
     * 
     */
    public ToggleToolbarHandler() {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.internal.handlers.ToggleCoolbarHandler#updateElement(org
     * .eclipse.ui.menus.UIElement, java.util.Map)
     */

    @Override
    @SuppressWarnings("rawtypes")
    public void updateElement(UIElement element, Map parameters) {
        IEvaluationService service = (IEvaluationService) element
                .getServiceLocator().getService(IEvaluationService.class);
        IEvaluationContext appState = service.getCurrentState();
        Boolean visible = (Boolean) appState
                .getVariable(ISources.ACTIVE_WORKBENCH_WINDOW_IS_COOLBAR_VISIBLE_NAME);
        element.setChecked(visible != null ? visible : false);
    }
}
