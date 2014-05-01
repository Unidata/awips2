package com.raytheon.viz.ui.cmenu;

import com.raytheon.uf.viz.core.rsc.DisplayType;

public class LoadAsGraphicsAction extends LoadAsDisplayTypeAction {

    @Override
    public String getText() {
        return "Load as Graphics";
    }

    @Override
    protected DisplayType getDisplayType() {
        return DisplayType.CONTOUR;
    }

}
