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
package com.raytheon.viz.gfe;

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.dialogs.GFEConfigDialog;
import com.raytheon.viz.gfe.procedures.ProcedureJob;
import com.raytheon.viz.gfe.smarttool.script.SmartToolJob;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin implements BundleActivator {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(Activator.class);

    // The plug-in ID
    public static final String PLUGIN_ID = "com.raytheon.viz.gfe";

    // The shared instance
    private static Activator plugin;

    private PythonPreferenceStore pythonPrefs;

    private GFEConfigDialog cfgDlg;

    /**
     * The constructor
     */
    public Activator() {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext
     * )
     */
    @Override
    public void start(BundleContext context) throws Exception {
        super.start(context);
        plugin = this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext
     * )
     */
    @Override
    public void stop(BundleContext context) throws Exception {
        plugin = null;
        ProcedureJob.shutdown();
        SmartToolJob.shutdown();
        super.stop(context);
    }

    /**
     * Returns the shared instance
     * 
     * @return the shared instance
     */
    public static Activator getDefault() {
        return plugin;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.plugin.AbstractUIPlugin#getPreferenceStore()
     */
    @Override
    public PythonPreferenceStore getPreferenceStore() {
        return pythonPrefs;
    }

    
    public void setPreferenceStore(PythonPreferenceStore prefs) {
        this.pythonPrefs = prefs;
    }
    
    public void createInitPreferenceStore() {
    	/*
    	 * Check if GFE perspective is opened
    	 */
    	IWorkbenchWindow window = PlatformUI.getWorkbench()
    		.getActiveWorkbenchWindow();

    	VizPerspectiveListener listener = VizPerspectiveListener
    		.getInstance(window);
    	
    	if (listener != null) {
    		AbstractVizPerspectiveManager manager = listener
    			.getPerspectiveManager("com.raytheon.viz.ui.GFEPerspective");
    		
    		if (manager == null || !manager.isOpened()) {
    			pythonPrefs = null;
    		}	
    	}	

    	/*
    	 * If GFE is not opened, pop up the config dialog.
    	 */
    	synchronized (this) {
    		if (pythonPrefs == null) {
    			cfgDlg = new GFEConfigDialog(
    					new Shell(Display.getDefault()));
    			cfgDlg.setBlockOnOpen(true);
    			cfgDlg.open();
    			String config = cfgDlg.getConfig();

    			pythonPrefs = new PythonPreferenceStore(config);
    			statusHandler.handle(Priority.EVENTA,
    					"GFE started with configuration: " + config);
    		} 
    	}
    }
    
}
