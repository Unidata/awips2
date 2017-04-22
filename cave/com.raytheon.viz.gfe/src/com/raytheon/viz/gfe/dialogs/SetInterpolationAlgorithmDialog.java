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
package com.raytheon.viz.gfe.dialogs;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.interpolation.Interpolator.Algorithm;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * The interpolation algorithm dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Mar 27, 2008             Eric Babin  Initial Creation
 * Jun 4, 2008      #1161   randerso    Reworked
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class SetInterpolationAlgorithmDialog extends CaveJFACEDialog {

    private Composite top;

    private Map<Button, Parm> parmMap;

    private Map<Button, Algorithm> algorithmMap;

    private Map<Algorithm, Button> buttonMap;

    protected Parm selectedParm;

    private IParmManager parmMgr;

    public SetInterpolationAlgorithmDialog(Shell parent) {
        super(parent);
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);

        GridLayout layout = (GridLayout) top.getLayout();
        layout.numColumns = 2;

        initializeComponents();

        return top;
    }

    private void initializeComponents() {

        GridData data = new GridData();
        data.verticalAlignment = SWT.TOP;
        Group elementGroup = new Group(top, SWT.SHADOW_ETCHED_IN);
        elementGroup.setLayout(new GridLayout(1, false));
        elementGroup.setLayoutData(data);
        elementGroup.setText("Weather Element");

        parmMgr = DataManager.getCurrentInstance().getParmManager();

        Parm[] parms = parmMgr.getDisplayedParms();
        Arrays.sort(parms);
        parmMap = new HashMap<Button, Parm>(parms.length);
        for (Parm parm : parms) {
            if (parm.isMutable()
                    && (parm.getGridInfo().getGridType() == GridType.SCALAR)) {
                Button button = new Button(elementGroup, SWT.RADIO);
                parmMap.put(button, parm);
                if (parmMap.size() == 1) {
                    button.setSelection(true);
                    selectedParm = parm;
                }
                button.setText(parm.getParmID().getParmName());
                button.addSelectionListener(new SelectionAdapter() {

                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        buttonMap.get(
                                selectedParm.getParmState()
                                        .getInterpolateAlgorithm())
                                .setSelection(false);
                        selectedParm = parmMap.get(e.getSource());
                        buttonMap.get(
                                selectedParm.getParmState()
                                        .getInterpolateAlgorithm())
                                .setSelection(true);
                    }

                });
            }
        }

        data = new GridData();
        data.verticalAlignment = SWT.TOP;
        Group algorithmGroup = new Group(top, SWT.SHADOW_ETCHED_IN);
        algorithmGroup.setLayoutData(data);
        algorithmGroup.setLayout(new GridLayout(1, false));
        algorithmGroup.setText("Algorithm");

        algorithmMap = new HashMap<Button, Algorithm>(Algorithm.values().length);
        buttonMap = new HashMap<Algorithm, Button>(Algorithm.values().length);
        for (Algorithm algorithm : Algorithm.values()) {
            Button button = new Button(algorithmGroup, SWT.RADIO);
            algorithmMap.put(button, algorithm);
            buttonMap.put(algorithm, button);
            button.setText(algorithm.getDisplayString());
            button.addSelectionListener(new SelectionAdapter() {

                @Override
                public void widgetSelected(SelectionEvent e) {
                    selectedParm.getParmState().setInterpolateAlgorithm(
                            algorithmMap.get(e.getSource()));
                }

            });
        }

        buttonMap.get(selectedParm.getParmState().getInterpolateAlgorithm())
                .setSelection(true);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);

        shell.setText("Set Interpolation Dialog");
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.OK_ID, "Dismiss", true);
    }
}
