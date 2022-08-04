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
package com.raytheon.viz.mpe.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * The Group Edit Stations dialog. Allows for the selection of a Quality Code
 * that can be applied to one or multiple stations just by clicking on the Map
 * display.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 14, 2008            snaples     Initial creation
 * May 06, 2011  #8994     jpiatt      Added zeroGageRdo.
 * Aug 11, 2017  6148      bkowal      Cleanup. Utilize {@link IGroupEditHandler}.
 * Feb 21, 2018  7225      bkowal      Fixed order of quality radio options. Updates to ensure
 *                                     Questionable/Screened quality changes are applied correctly.
 * Jul 17, 2018  7139      tgurney     Fix the order of the radio buttons
 * Mar 27, 2019  7139      tgurney     Fix radio button values (fix merge with
 *                                     Omaha #7225)
 *
 * </pre>
 *
 * @author snaples
 */

public class GroupEditStationsDialog extends CaveSWTDialog {

    private DailyQcUtils dqc = DailyQcUtils.getInstance();

    private Button verifiedCodeRdo;

    private Button questCodeRdo;

    private Button screenCodeRdo;

    private Button badCodeRdo;

    private Button zeroGageRdo;

    public static int group_qual = 8;

    private final IGroupEditHandler handler;

    public GroupEditStationsDialog(Shell parent,
            final IGroupEditHandler handler) {
        super(parent);
        setText("Group Edit Stations Popup");
        this.handler = handler;
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        MPEDisplayManager.getCurrent().setGroupedt(false);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        // Initialize all of the controls and layouts
        initializeComponents();

        MPEDisplayManager.getCurrent().setGroupedt(true);
    }

    private void initializeComponents() {
        createGageFlagComp();
        Button[] radios = { verifiedCodeRdo, questCodeRdo, screenCodeRdo,
                badCodeRdo, zeroGageRdo };
        for (int i = 0; i < 4; i++) {

            if (dqc.func[i] == group_qual) {
                radios[i].setSelection(true);
            } else {
                radios[i].setSelection(false);
            }
        }
        createOkButtonComp();
    }

    /**
     * Create the data options group and controls.
     */
    private void createGageFlagComp() {
        // Create a container to hold the label and the combo box.
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite gageFlagComp = new Composite(shell, SWT.NONE);
        GridLayout gageFlagCompLayout = new GridLayout(2, false);
        gageFlagComp.setLayout(gageFlagCompLayout);
        gageFlagComp.setLayoutData(gd);

        verifiedCodeRdo = new Button(gageFlagComp, SWT.RADIO);
        verifiedCodeRdo.setText("Verified");
        verifiedCodeRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                change_group_qual(0);
            }
        });

        questCodeRdo = new Button(gageFlagComp, SWT.RADIO);
        questCodeRdo.setText("Questionable");
        questCodeRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                change_group_qual(1);
            }
        });

        screenCodeRdo = new Button(gageFlagComp, SWT.RADIO);
        screenCodeRdo.setText("Screened (Force)");
        screenCodeRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                change_group_qual(2);
            }
        });

        badCodeRdo = new Button(gageFlagComp, SWT.RADIO);
        badCodeRdo.setText("Bad");
        badCodeRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                change_group_qual(3);
            }
        });

        zeroGageRdo = new Button(gageFlagComp, SWT.RADIO);
        zeroGageRdo.setText("Set Precipitation Value to 0.0");
        zeroGageRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                change_group_qual(4);
            }
        });
    }

    private void createOkButtonComp() {
        // Create a container to hold the button.
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite okBtnComp = new Composite(shell, SWT.NONE);
        GridLayout okBtnCompLayout = new GridLayout(1, false);
        okBtnComp.setLayout(okBtnCompLayout);
        okBtnComp.setLayoutData(gd);

        GridData bd = new GridData(110, 25);
        Button okBtn = new Button(okBtnComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(bd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handler.handleGroupEdit();
                close();
            }
        });
    }

    public void change_group_qual(int j) {
        group_qual = dqc.func[j];
    }
}