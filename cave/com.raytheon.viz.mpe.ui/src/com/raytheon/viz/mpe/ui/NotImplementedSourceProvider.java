package com.raytheon.viz.mpe.ui;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.ui.AbstractSourceProvider;

public class NotImplementedSourceProvider extends AbstractSourceProvider {

    private static final String[] MENU_ENABLED = new String[] { "com.raytheon.viz.mpe.ui.notImplemented" };

    private final boolean enabled = false;

    private final Map<String, Boolean> sourceMap = new HashMap<String, Boolean>();

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.ISourceProvider#dispose()
     */
    @Override
    public void dispose() {
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.ISourceProvider#getCurrentState()
     */
    @Override
    public Map<String, Boolean> getCurrentState() {
        // System.out.println("test");
        sourceMap.put(MENU_ENABLED[0], enabled);
        return sourceMap;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.ISourceProvider#getProvidedSourceNames()
     */
    @Override
    public String[] getProvidedSourceNames() {
        // System.out.println("test1");
        // TODO Auto-generated method stub
        return MENU_ENABLED;
    }

}
