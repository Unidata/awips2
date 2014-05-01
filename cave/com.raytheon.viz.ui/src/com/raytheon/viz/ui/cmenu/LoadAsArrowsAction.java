package com.raytheon.viz.ui.cmenu;

import com.raytheon.uf.viz.core.rsc.DisplayType;

public class LoadAsArrowsAction extends LoadAsDisplayTypeAction {

    @Override
    public String getText() {
        return "Load as Arrows";
    }

    @Override
    protected DisplayType getDisplayType() {
        return DisplayType.ARROW;
    }

}
