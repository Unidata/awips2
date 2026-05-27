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
package com.raytheon.uf.viz.grib.wizard;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.jface.wizard.WizardPage;

import com.raytheon.uf.common.grib.GribCoverageStore;
import com.raytheon.uf.common.grib.GribModelLookup;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.grib.wizard.page.GribAreaWizardPage;
import com.raytheon.uf.viz.grib.wizard.page.GribExportPage;
import com.raytheon.uf.viz.grib.wizard.page.GribImportPage;
import com.raytheon.uf.viz.grib.wizard.page.GribModelWizardPage;
import com.raytheon.uf.viz.grib.wizard.page.GribVbWizardPage;
import com.raytheon.uf.viz.grib.wizard.save.LocalizationGribWizardSave;
import com.raytheon.uf.viz.grib.wizard.save.ZipGribWizardSave;

/**
 * 
 * Wizard for configuring a new grib model.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Apr 19, 2016  5572     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GribWizard extends Wizard {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GribWizard.class);

    private final GribWizardData data = new GribWizardData();

    private final GribModelLookup modelLookup;

    private final GribCoverageStore coverageStore;

    public GribWizard() throws Exception {
        this.coverageStore = new GribCoverageStore(
                PathManagerFactory.getPathManager());
        this.modelLookup = new GribModelLookup();
        modelLookup.setCoverageNameLookup(coverageStore);
    }

    @Override
    public void addPages() {
        List<IWizardPage> pages = new ArrayList<>();

        /*
         * create all pages before adding any so that errors don't result in a
         * partial wizard.
         */
        IWizardContainer container = getContainer();
        try {
            pages.add(new GribImportPage(coverageStore, data));
            pages.add(new GribModelWizardPage(modelLookup, coverageStore, data));
            pages.add(new GribAreaWizardPage(coverageStore, data));
            pages.add(new GribVbWizardPage(data));
            pages.add(new GribExportPage(data));
        } catch (Exception e) {
            statusHandler.error("Unable to create Grib Wizard", e);
            if (container instanceof WizardDialog) {
                ((WizardDialog) container).close();
            } else {
                /*
                 * There are not currently any other containers, but just in
                 * case try to indicate an error. There will be no pages so it
                 * should just be an empty wizard.
                 */
                setWindowTitle("Error opening Grib Wizard");
            }
            return;
        }
        for (IWizardPage page : pages) {
            addPage(page);
        }
        setWindowTitle("New Grib Model");
    }

    @Override
    public boolean performFinish() {
        try {
            if (data.getExportPath() != null) {
                ZipGribWizardSave save = new ZipGribWizardSave(
                        data.getExportPath());
                save.save(data);
                save.close();
            }

            if (data.getSaveContext() != null) {
                LocalizationGribWizardSave save = new LocalizationGribWizardSave(
                        PathManagerFactory.getPathManager(),
                        data.getSaveContext());
                save.save(data);
            }
        } catch (Exception e) {
            statusHandler.error(e.getLocalizedMessage(), e);
            IWizardPage page = getContainer().getCurrentPage();
            if (page instanceof WizardPage) {
                ((WizardPage) page).setErrorMessage(e.getLocalizedMessage());
            }
            return false;
        }
        return true;
    }

}