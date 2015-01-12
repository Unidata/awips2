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
package com.raytheon.uf.viz.derivparam.ui.dialogs;

import java.io.File;

import com.raytheon.uf.common.derivparam.library.DerivedParameterGenerator;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.localization.LocalizationPerspectiveUtils;
import com.raytheon.uf.viz.localization.service.ILocalizationService;
import com.raytheon.viz.ui.dialogs.CaveSWTWizard;

/**
 * Wizard for generating derived paramters
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 16, 2010            mschenke    Initial creation
 * Sep 17, 2013 2285       mschenke    Fixed serialization of DerivParamDesc
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DerivedParamWizard extends CaveSWTWizard {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DerivedParamWizard.class);

    private DerivedParamNewDefinitionPage newDefinitionPage;

    private DerivedParamNewFunctionPage newFunctionPage;

    public DerivedParamWizard() {
        super("New Derived Parameter");
    }

    @Override
    public void addPages() {
        newDefinitionPage = new DerivedParamNewDefinitionPage();
        addPage(newDefinitionPage);
        newFunctionPage = new DerivedParamNewFunctionPage();
        addPage(newFunctionPage);
    }

    /*
     * Max, I took this out but itwill go away with the SWT Wizard stuff.
     */

    // @Override
    // public void createPageControls(Composite pageContainer) {
    // pageContainer.setLayout(new GridLayout(1, false));
    // pageContainer.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
    // false));
    // super.createPageControls(pageContainer);
    // }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.Wizard#performFinish()
     */
    @Override
    public boolean performFinish() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext userCtx = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.USER);
        String functionContents = newFunctionPage.createFunction();
        LocalizationFile functionFile = null, definitionFile = null;
        if (functionContents != null) {
            String fileName = newFunctionPage.getFunctionName() + "."
                    + newFunctionPage.getSelectedFunctionType().getExtension();
            String path = DerivedParameterGenerator.FUNCTIONS_DIR
                    + File.separator + fileName;
            LocalizationFile file = pm.getLocalizationFile(userCtx, path);
            File f = file.getFile();
            if (f.exists()) {
                f.delete();
            }
            try {
                FileUtil.bytes2File(functionContents.getBytes(), f);
                file.save();
                functionFile = file;
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        String definitionContents = newDefinitionPage.createDefinition();
        if (definitionContents != null) {
            String fileName = newDefinitionPage.getAbbreviation() + ".xml";
            String path = DerivedParameterGenerator.XML_DIR
                    + IPathManager.SEPARATOR + fileName;
            LocalizationFile file = pm.getLocalizationFile(userCtx, path);
            File f = file.getFile();
            if (f.exists()) {
                f.delete();
            }
            try {
                FileUtil.bytes2File(definitionContents.getBytes(), f);
                file.save();
                definitionFile = file;
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

        if (functionFile != null || definitionFile != null) {
            ILocalizationService service = LocalizationPerspectiveUtils
                    .changeToLocalizationPerspective();
            if (functionFile != null) {
                service.openFile(functionFile);
            }
            if (definitionFile != null) {
                service.openFile(definitionFile);
            }
        }
        return true;
    }

    @Override
    public boolean canFinish() {
        return newFunctionPage.isPageComplete()
                || newDefinitionPage.isPageComplete();
    }

}
