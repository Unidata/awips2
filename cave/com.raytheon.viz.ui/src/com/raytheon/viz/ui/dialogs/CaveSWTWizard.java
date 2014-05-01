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

import java.util.ArrayList;
import java.util.List;

/**
 * TODO Add Description
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
public abstract class CaveSWTWizard {
    private List<CaveSWTWizardPage> pages = new ArrayList<CaveSWTWizardPage>();

    private CaveSWTWizardPage currentPage;

    private int curWizPageIdx = 0;

    private String title;

    protected CaveSWTWizard(String title) {
        this.title = title;
    }

    public String getTitle() {
        return title;
    }

    public void addPage(CaveSWTWizardPage wizardPage) {
        pages.add(wizardPage);
        wizardPage.setWizard(this);

        if (pages.size() == 1) {
            currentPage = pages.get(0);
        }
    }

    public List<CaveSWTWizardPage> getWizardPages() {
        return pages;
    }

    public void nextPage() {
        if (curWizPageIdx < pages.size() - 1) {
            ++curWizPageIdx;
            currentPage = pages.get(curWizPageIdx);
        }
    }

    public boolean hasNextPage() {
        if (curWizPageIdx < pages.size() - 1) {
            return true;
        }

        return false;
    }

    public void previousPage() {
        if (curWizPageIdx > 0) {
            --curWizPageIdx;
            currentPage = pages.get(curWizPageIdx);
        }
    }

    public boolean hasPreviousPage() {
        if (curWizPageIdx > 0) {
            return true;
        }

        return false;
    }

    public CaveSWTWizardPage getCurrentPage() {
        return currentPage;
    }

    public int getCurrentPageIndex() {
        return curWizPageIdx;
    }

    public boolean canFinish() {
        // Default implementation is to check if all pages are complete.
        for (CaveSWTWizardPage wizardPage : pages) {
            if (wizardPage.isPageComplete() == false) {
                return false;
            }
        }
        return true;
    }

    /**
     * Add pages to the wizard.
     */
    public abstract void addPages();

    public abstract boolean performFinish();
}
