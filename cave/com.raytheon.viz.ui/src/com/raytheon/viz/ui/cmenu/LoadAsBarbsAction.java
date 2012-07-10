package com.raytheon.viz.ui.cmenu;

import com.raytheon.uf.viz.core.rsc.DisplayType;

public class LoadAsBarbsAction extends LoadAsDisplayTypeAction {

    @Override
    public String getText() {
        return "Load as Wind Barbs";
    }

    @Override
    protected DisplayType getDisplayType() {
        return DisplayType.BARB;
    }

}
