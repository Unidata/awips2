package com.raytheon.viz.ui.cmenu;

import com.raytheon.uf.viz.core.rsc.DisplayType;

public class LoadAsImageAction extends LoadAsDisplayTypeAction {

    @Override
    protected DisplayType getDisplayType() {
        return DisplayType.IMAGE;
    }

}
