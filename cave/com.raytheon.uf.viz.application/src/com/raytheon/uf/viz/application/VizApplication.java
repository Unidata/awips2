/**
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

package com.raytheon.uf.viz.application;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.equinox.app.IApplication;
import org.eclipse.equinox.app.IApplicationContext;

import com.raytheon.uf.viz.application.component.IStandaloneComponent;

/**
 * Defines the Application context
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 18, 2007            chammack    Initial Creation.
 * Dec 03, 2007 461        bphillip    Added persistence of workstation time to localization
 * Oct 07, 2008 1433       chammack    Added alertviz startup
 * Jan 23, 2014            njensen     Added shutdown hook and printout
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class VizApplication implements IApplication {

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.equinox.app.IApplication#start(org.eclipse.equinox.app.
     * IApplicationContext)
     */
    @Override
    public Object start(IApplicationContext context) throws Exception {

        String appToRun = ProgramArguments.getInstance()
                .getString("-component");

        IStandaloneComponent component = null;

        if (appToRun == null) {
            System.out.println("No component specified, defaulting to 'viz'");
            appToRun = "viz";
        }

        try {
            component = getComponent(appToRun);
        } catch (Exception e) {
            e.printStackTrace();
        }

        if (component == null) {
            System.err.println("No component by name: " + appToRun
                    + ", exiting...");
            return IApplication.EXIT_OK;
        }

        addShutdownHook();

        return component.startComponent(appToRun);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.equinox.app.IApplication#stop()
     */
    @Override
    public void stop() {

    }

    /**
     * Run a component registered with the given name
     * 
     * @param component
     *            the component key for the extension point
     *            com.raytheon.uf.viz.core.component
     */
    private IStandaloneComponent getComponent(String component)
            throws Exception {
        IStandaloneComponent standalone = null;
        // Search for a key that matches what was passed in
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint point = registry
                .getExtensionPoint("com.raytheon.uf.viz.core.component");
        if (point != null) {
            IExtension[] extensions = point.getExtensions();
            for (int i = 0; i < extensions.length && standalone == null; i++) {
                IConfigurationElement[] config = extensions[i]
                        .getConfigurationElements();

                for (int j = 0; j < config.length && standalone == null; j++) {
                    String key = config[j].getAttribute("key");
                    if (key != null && key.equalsIgnoreCase(component)) {
                        standalone = (IStandaloneComponent) config[j]
                                .createExecutableExtension("class");
                    }
                }
            }
        }

        return standalone;
    }

    protected void addShutdownHook() {
        Runtime.getRuntime().addShutdownHook(new Thread(new Runnable() {
            @Override
            public void run() {
                /*
                 * This may seem pointless but is actually quite helpful to
                 * confirm how the process exited. If the process is killed by a
                 * kill command on a terminal, the console output will have this
                 * message but not the normal safe shutdown output (see
                 * com.raytheon.uf.viz.core.Activator's stop() and
                 * VizWorkbenchAdvisor's preShutdown()). In contrast, a
                 * spontaneous death of the process or force kill will not have
                 * this printout.
                 */
                SimpleDateFormat sdf = new SimpleDateFormat(
                        "yyyy-MM-dd HH:mm:ss");
                sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
                System.out.println(sdf.format(new Date())
                        + " VizApplication's runtime shutdown hook triggered");
            }
        }) {

        });
    }

}
