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

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.localization.LocalizationFile;
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

public class SessionMsgArchiveView extends CaveFloatingView {

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
    }

    @Override
    public void setFocus() {
    }
}
