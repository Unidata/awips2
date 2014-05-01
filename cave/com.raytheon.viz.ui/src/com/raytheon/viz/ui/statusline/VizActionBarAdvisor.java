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

package com.raytheon.viz.ui.statusline;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.action.ICoolBarManager;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.jface.action.SubStatusLineManager;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.actions.ActionFactory.IWorkbenchAction;
import org.eclipse.ui.application.ActionBarAdvisor;
import org.eclipse.ui.application.IActionBarConfigurer;

/**
 * Provide toolbar configuration
 * 
 * <pre>
 * 
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 7/1/06                   chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
/**
 * @author randerso
 * 
 */
public class VizActionBarAdvisor extends ActionBarAdvisor {

    private static Map<IWorkbenchWindow, VizActionBarAdvisor> instanceMap = new HashMap<IWorkbenchWindow, VizActionBarAdvisor>();

    public static VizActionBarAdvisor getInstance(IWorkbenchWindow window) {
        VizActionBarAdvisor instance = instanceMap.get(window);
        return instance;
    }

    private ICoolBarManager coolBar;

    private IMenuManager menuMgr;

    private IStatusLineManager statusLine;

    private SubStatusLineManager subStat;

    private IWorkbenchAction saveAction;

    private IWorkbenchAction saveAllAction;

    /**
     * Default constructor
     * 
     * @param configurer
     */
    public VizActionBarAdvisor(IActionBarConfigurer configurer) {
        super(configurer);

        instanceMap.put(configurer.getWindowConfigurer().getWindow(), this);
    }

    @Override
    public void dispose() {
        super.dispose();

        if (subStat != null) {
            subStat.disposeManager();
        }

        saveAllAction = saveAction = null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.application.ActionBarAdvisor#fillCoolBar(org.eclipse.jface
     * .action.ICoolBarManager)
     */
    @Override
    protected void fillCoolBar(ICoolBarManager coolBar) {
        super.fillCoolBar(coolBar);
        this.coolBar = coolBar;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.application.ActionBarAdvisor#fillMenuBar(org.eclipse.jface
     * .action.IMenuManager)
     */
    @Override
    protected void fillMenuBar(IMenuManager menuBar) {
        super.fillMenuBar(menuBar);
        this.menuMgr = menuBar;
    }

    @Override
    protected void fillStatusLine(IStatusLineManager statusLine) {
        super.fillStatusLine(statusLine);
        this.statusLine = statusLine;
    }

    public ICoolBarManager getCoolBar() {
        return this.coolBar;
    }

    public IMenuManager getMenuManager() {
        return this.menuMgr;
    }

    public IStatusLineManager getStatusLine() {
        return statusLine;
    }

    @Override
    protected void makeActions(IWorkbenchWindow window) {
        super.makeActions(window);
        saveAction = ActionFactory.SAVE.create(window);
        register(saveAction);

        saveAllAction = ActionFactory.SAVE_ALL.create(window);
        register(saveAllAction);
    }

}
