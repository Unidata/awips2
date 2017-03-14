package com.raytheon.viz.satellite.action;

import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.viz.satellite.rsc.SatResource;
import com.raytheon.viz.satellite.rsc.SatResourceData;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * Provides toggle action for incomplete frames
 * 
 * This action allows SatResource to be able to toggle the incomplete frames
 * showing via the SatResourceData object. This action is registered as a
 * product specific contextual menu. The menu item is a checkbox within the
 * contextual menu.
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date          Ticket#   Engineer    Description
 *  ------------- --------  ----------- --------------------------
 *  Aug 12, 2016  DCS 18781 jburks      Intial creation.
 * 
 * </pre>
 * 
 * @author jburks
 * @version 1
 */
public class IncompleteFramesToggleAction extends AbstractRightClickAction {

    @Override
    public void run() {
        super.run();
        SatResource resc = (SatResource) getSelectedRsc();
        SatResourceData rescData = resc.getResourceData();
        boolean isShowIncompleteFrames = !rescData.isShowIncompleteFrames();
        rescData.setShowIncompleteFrames(isShowIncompleteFrames);
        this.setChecked(!isShowIncompleteFrames);
        resc.redoMainTimeMatch();
    }

    @Override
    public void setSelectedRsc(ResourcePair selectedRsc) {
        super.setSelectedRsc(selectedRsc);
        SatResourceData rescData = (SatResourceData) selectedRsc
                .getResourceData();
        boolean isShowIncompleteFrames = !rescData.isShowIncompleteFrames();
        this.setChecked(!isShowIncompleteFrames);
    }

    @Override
    public String getText() {
        return "Show incomplete frames";
    }

}
