package com.raytheon.uf.viz.ui.menus.xml;

import org.eclipse.jface.action.IMenuListener;

public interface IVizMenuManager {
    public void addMenuListener(IMenuListener listener);

    public void removeMenuListener(IMenuListener listener);
}
