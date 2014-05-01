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
package com.raytheon.viz.ui.dialogs;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

/**
 * Cave SWT Wizard Page, based off of org.eclipse.jface.wizard.WizardPage
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 19, 2010            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public abstract class CaveSWTWizardPage {

    /**
     * Not used at this time but may be used in the future...
     */
    private CaveSWTWizard wizard;

    private String title = "";

    private String description = "";

    private String errorMessage = "Page is incomplete.";

    Shell shell;

    public CaveSWTWizardPage(String title) {
        setTitle(title);
    }

    /**
     * Set the page title
     * 
     * @param title
     */
    public void setTitle(String title) {
        this.title = title;
    }

    /**
     * Get the page title
     * 
     * @return
     */
    public String getTitle() {
        return this.title;
    }

    /**
     * Set the description to display
     * 
     * @param description
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * Get the description to display
     * 
     * @return
     */
    public String getDescription() {
        return this.description;
    }

    /**
     * Set the wizard this page is associated with, package level only
     * 
     * @param wizard
     */
    void setWizard(CaveSWTWizard wizard) {
        this.wizard = wizard;
    }

    /**
     * Get the wizard this page is associated with
     * 
     * @return
     */
    public CaveSWTWizard getWizard() {
        return this.wizard;
    }

    /**
     * Method to determine if the page is complete as is, default always returns
     * true, extending classes should override but not required
     * 
     * @return
     */
    public boolean isPageComplete() {
        return true;
    }

    /**
     * Set the error message to display when page is incomplete
     * 
     * @param errorMessage
     */
    public void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }

    /**
     * Get the error message to display
     * 
     * @return
     */
    public String getErrorMessage() {
        return errorMessage;
    }

    /**
     * Get the shell the wizard page is on
     * 
     * @return page's shell
     */
    public Shell getShell() {
        return shell;
    }

    /**
     * Create controls on the page, composite passed in was specifically created
     * for this page
     * 
     * @param parentComp
     */
    public abstract void createPageControls(Composite parentComp);
}
