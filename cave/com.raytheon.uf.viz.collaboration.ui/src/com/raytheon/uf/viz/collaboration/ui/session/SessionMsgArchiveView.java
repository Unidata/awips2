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
package com.raytheon.uf.viz.collaboration.ui.session;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.actions.PrintLogActionContributionItem;
import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.viz.ui.views.CaveFloatingView;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2012            bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class SessionMsgArchiveView extends CaveFloatingView implements
        IPrintableView {

    private SessionMsgArchiveBrowser browser;

    @Override
    public void createPartControl(Composite parent) {
        super.createPartControl(parent);
        String secondaryId = getViewSite().getSecondaryId();
        LocalizationFile logDir = SessionMsgArchive.getArchiveDir(secondaryId);

        browser = new SessionMsgArchiveBrowser(parent, SWT.NONE);
        browser.setDir(logDir);

        setPartName(browser.getBrowserName());
        parent.setLayout(new GridLayout(1, false));
        parent.layout();

        // unfortunately this code cannot be a part of createToolbarButton
        // because I cannot instantiate the ACI until after the StyledText
        // control in SessionMsgArchiveBrowser is instantiated which happens
        // above
        IContributionItem printAction = new PrintLogActionContributionItem(this);
        ToolBarManager mgr = (ToolBarManager) getViewSite().getActionBars()
                .getToolBarManager();
        mgr.add(printAction);

        Action searchAction = new Action("Search") {
            @Override
            public void run() {
                browser.toggleSearch();
            }
        };
        searchAction.setImageDescriptor(IconUtil.getImageDescriptor(Activator
                .getDefault().getBundle(), "find.gif"));
        mgr.add(searchAction);
    }

    @Override
    public void setFocus() {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.IPrintableView#getStyledText
     * ()
     */
    @Override
    public StyledText getStyledText() {
        return browser.getStyledText();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.IPrintableView#getHeaderText
     * ()
     */
    @Override
    public String getHeaderText() {
        return "Conversation log from " + browser.getLogDate();
    }
}
