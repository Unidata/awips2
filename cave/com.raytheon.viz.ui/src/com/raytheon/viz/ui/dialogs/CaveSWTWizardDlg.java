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

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

/**
 * Similar to JFace's wizard dialog, but works with cave functionality
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 19, 2010            lvenable     Initial creation
 * Oct 22, 2012 1229       rferrel     Dialog changed to non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class CaveSWTWizardDlg extends CaveSWTDialog {

    private Composite wizStackLayoutComp;

    private StackLayout wizStackLayout;

    private Label headerTitleLbl;

    private Label headerTextLbl;

    private Font headerTitleFont;

    private CaveSWTWizard swtWizzard;

    private Button previousBtn;

    private Button nextBtn;

    private Button finishBtn;

    private ArrayList<Composite> pageCompArray;

    public CaveSWTWizardDlg(Shell parent, CaveSWTWizard swtWizzard) {
        super(parent, SWT.APPLICATION_MODAL | SWT.MIN, CAVE.DO_NOT_BLOCK);
        this.swtWizzard = swtWizzard;
        if (swtWizzard.getTitle() != null) {
            setText(swtWizzard.getTitle());
        }
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.DEFAULT, true, false);
    }

    @Override
    protected void disposed() {
        headerTitleFont.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        swtWizzard.addPages();

        createHeaderLabels();
        addSeparator(shell);

        createWizardStackLayout();
        addSeparator(shell);

        createBottomButtons();

        updateHeaderLabels();
        evaluateButtons();
    }

    private void createHeaderLabels() {
        headerTitleFont = new Font(getDisplay(), "Sans", 9, SWT.BOLD);
        Composite headerComp = new Composite(shell, SWT.BORDER);
        headerComp.setBackground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        headerComp.setLayout(new GridLayout(1, false));
        headerComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        headerTitleLbl = new Label(headerComp, SWT.NONE);
        headerTitleLbl.setFont(headerTitleFont);
        headerTitleLbl.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        headerTitleLbl.setBackground(getDisplay().getSystemColor(
                SWT.COLOR_WHITE));

        headerTextLbl = new Label(headerComp, SWT.NONE);
        headerTextLbl.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        headerTextLbl.setBackground(getDisplay()
                .getSystemColor(SWT.COLOR_WHITE));
    }

    private void createWizardStackLayout() {
        // Create the stack layout composite
        Composite centerStackComp = new Composite(shell, SWT.NONE);
        centerStackComp.setLayout(new GridLayout(1, false));
        centerStackComp.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT,
                true, false));

        wizStackLayoutComp = new Composite(centerStackComp, SWT.NONE);
        wizStackLayout = new StackLayout();
        wizStackLayoutComp.setLayout(wizStackLayout);

        pageCompArray = new ArrayList<Composite>();

        List<CaveSWTWizardPage> wizardPages = swtWizzard.getWizardPages();

        for (int i = 0; i < wizardPages.size(); i++) {
            Composite c = new Composite(wizStackLayoutComp, SWT.NONE);
            c.setLayout(new GridLayout(1, false));
            c.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

            CaveSWTWizardPage page = wizardPages.get(i);
            page.shell = getShell();
            page.createPageControls(c);

            pageCompArray.add(c);
        }

        wizStackLayout.topControl = pageCompArray.get(0);
        wizStackLayoutComp.layout();
    }

    private void createBottomButtons() {
        GridData gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        Composite mainButtonComp = new Composite(shell, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(4, false));
        mainButtonComp.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        previousBtn = new Button(mainButtonComp, SWT.PUSH);
        previousBtn.setText("<< Previous");
        previousBtn.setLayoutData(gd);
        previousBtn.setEnabled(false);
        previousBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                handlePrevButtonAction();
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        nextBtn = new Button(mainButtonComp, SWT.PUSH);
        nextBtn.setText("Next >>");
        nextBtn.setLayoutData(gd);
        nextBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                handleNextButtonAction();
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        Button cancelBtn = new Button(mainButtonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        finishBtn = new Button(mainButtonComp, SWT.PUSH);
        finishBtn.setText("Finish");
        finishBtn.setLayoutData(gd);
        finishBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (checkCurrentPage()) {
                    swtWizzard.performFinish();
                    shell.dispose();
                }
            }
        });
    }

    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout) parentComp.getLayout();
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Sets bottom buttons to be enabled/disabled depending on state
     */
    private void evaluateButtons() {
        previousBtn.setEnabled(swtWizzard.hasPreviousPage());
        nextBtn.setEnabled(swtWizzard.hasNextPage());
        finishBtn.setEnabled(!swtWizzard.hasNextPage());
    }

    private void handlePrevButtonAction() {
        swtWizzard.previousPage();
        wizStackLayout.topControl = pageCompArray.get(swtWizzard
                .getCurrentPageIndex());
        wizStackLayoutComp.layout();

        updateHeaderLabels();
        evaluateButtons();
    }

    private boolean checkCurrentPage() {
        CaveSWTWizardPage currentWizardPage = swtWizzard.getCurrentPage();
        if (currentWizardPage.isPageComplete() == false) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Error");
            mb.setMessage(currentWizardPage.getErrorMessage());
            mb.open();
            return false;
        }
        return true;
    }

    private void handleNextButtonAction() {
        if (checkCurrentPage()) {
            swtWizzard.nextPage();
            wizStackLayout.topControl = pageCompArray.get(swtWizzard
                    .getCurrentPageIndex());
            wizStackLayoutComp.layout();

            updateHeaderLabels();
            evaluateButtons();
        }
    }

    private void updateHeaderLabels() {
        CaveSWTWizardPage currentWizardPage = swtWizzard.getCurrentPage();

        this.headerTitleLbl.setText(currentWizardPage.getTitle());
        this.headerTextLbl.setText(currentWizardPage.getDescription());
    }
}
