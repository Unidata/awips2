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
package com.raytheon.viz.ui.personalities.awips;

import java.util.List;

import org.eclipse.e4.core.di.annotations.Execute;
import org.eclipse.e4.ui.model.application.MApplication;
import org.eclipse.e4.ui.model.application.ui.advanced.MPerspective;
import org.eclipse.e4.ui.model.application.ui.advanced.MPerspectiveStack;
import org.eclipse.e4.ui.workbench.modeling.EModelService;

import com.raytheon.uf.viz.core.ProgramArguments;

/**
 * 
 * Switch the perspective in the e4 model when a user has requested a specific
 * perspective. The same type of functionality is found in the internal eclipse
 * class CommandLineOptionModelProcessor, however in that case the perspective
 * is only matched by id. This class is more lenient and will also match the
 * perspective label.
 * 
 * Note this processor is only necessary if the model is being restored and
 * currently has a different perspective selected. For the case where the model
 * is new {@link AWIPSWorkbenchAdvisor} is responsible for telling the
 * application which perspective to choose on startup.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Sep 23, 2016  5897     bsteffen  Initial creation
 * 
 * </pre>
 *
 * @author bsteffen
 */
public class ForcedPerspectiveProcessor {

    @Execute
    public void selectForcedPerspective(MApplication application,
            EModelService modelService) {
        String forcedPerspectiveId = ProgramArguments.getInstance()
                .getString("-perspective");
        if (forcedPerspectiveId == null) {
            return;
        }

        List<MPerspectiveStack> perspStackList = modelService.findElements(application, null,
                MPerspectiveStack.class, null);

        if (perspStackList.isEmpty()) {
            return;
        }

        MPerspectiveStack perspStack = perspStackList.get(0);
        MPerspective selected = perspStack.getSelectedElement();

        if (selected != null && perspectiveMatches(forcedPerspectiveId, selected)) {
            return;
        }

        for (MPerspective persp : perspStack.getChildren()) {
            if (perspectiveMatches(forcedPerspectiveId, persp)) {
                perspStack.setSelectedElement(persp);
                return;
            }
        }
        /*
         * If the forced perspective does not exist in the stack it is likely
         * that it has never been opened. In this case the current selected
         * perspective would be partially restored before the workbench advisor
         * causes the forced perspective to open. This results in an
         * inconsistent state for the initially selected perspective and if the
         * user returns to that perspective then exceptions occur. To avoid any
         * inconsistent state it is better to just clear out all perspectives
         * currently in the model which will allow the workbench advisor to
         * initialize with the forced perspective.
         */
        perspStack.getChildren().clear();
        perspStack.setSelectedElement(null);
    }

    /**
     * Determine if the perspective id or label of the perspective matches the
     * forced perspective id.
     * 
     * @param forcedPerspectiveId
     *            the perspective id to check
     * @param perspective
     *            the perspective to check against.
     * @return true if the perspective id or label of the perspective matches
     *         the forced perspective id.
     */
    protected boolean perspectiveMatches(String forcedPerspectiveId,
            MPerspective perspective) {
        if (perspective.getElementId().equals(forcedPerspectiveId)) {
            return true;
        } else if (perspective.getLabel()
                .equalsIgnoreCase(forcedPerspectiveId)) {
            return true;
        } else {
            return false;
        }

    }
}
