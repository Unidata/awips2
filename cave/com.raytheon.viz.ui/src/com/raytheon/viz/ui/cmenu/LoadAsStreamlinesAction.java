package com.raytheon.viz.ui.cmenu;

import com.raytheon.uf.viz.core.rsc.DisplayType;

public class LoadAsStreamlinesAction extends LoadAsDisplayTypeAction {

    @Override
    public String getText() {
        return "Load as Streamlines";
    }

    @Override
    protected DisplayType getDisplayType() {
        return DisplayType.STREAMLINE;
    }

}
