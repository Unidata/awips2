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
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.actions.GroupEditCalls;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 14, 2008            snaples     Initial creation
 * May 06, 2011  #8994     jpiatt      Added zeroGageRdo.
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class GroupEditStationsDialog extends CaveSWTDialog {
    
    private DailyQcUtils dqc = DailyQcUtils.getInstance();

	private Font font;

	private Button verifiedCodeRdo;

	private Button questCodeRdo;

	private Button screenCodeRdo;

	private Button badCodeRdo;

	private Button zeroGageRdo;

	private Button okBtn;

	public static int group_qual = 8;

	int data = 0;

	GroupEditCalls gec = new GroupEditCalls();

	/**
	 * Constructor
	 * 
	 * @param parent
	 *            Parent shell
	 */
	public GroupEditStationsDialog(Shell parent, int dta) {
		super(parent);
		setText("Group Edit Stations Popup");

		data = dta;
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
		font.dispose();
	}

	@Override
	protected void initializeComponents(Shell shell) {
		font = new Font(shell.getDisplay(), "Courier", 10, SWT.NORMAL);

		// Initialize all of the controls and layouts
		initializeComponents();

		MPEDisplayManager.getCurrent().setGroupedt(true);
	}

	/**
	 * Initialize the dialog components.
	 */
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

		screenCodeRdo = new Button(gageFlagComp, SWT.RADIO);
		screenCodeRdo.setText("Screened (Force)");
		screenCodeRdo.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				change_group_qual(1);
			}
		});

		questCodeRdo = new Button(gageFlagComp, SWT.RADIO);
		questCodeRdo.setText("Questionable");
		questCodeRdo.addSelectionListener(new SelectionAdapter() {
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
		okBtn = new Button(okBtnComp, SWT.PUSH);
		okBtn.setText("OK");
		okBtn.setLayoutData(bd);
		okBtn.addSelectionListener(new SelectionAdapter() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
			 * .swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				System.out.println("Apply group data is : " + data);
				if (data == 0) {
					gec.apply_group();
				} else if (data == 1) {
					gec.apply_tgroup();
				}
				shell.dispose();
			}
		});
	}

	public void change_group_qual(int j) {
		group_qual = dqc.func[j];
	}

}
