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
package com.raytheon.viz.avnconfig;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

/**
 * This class creates all of the buttons that corresponds to the available
 * methods. Example of use is generating the buttons with the various rules
 * methods in the Monitoring Rules configuration dialog.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 22 MAY 2008  1119       lvenable    Initial creation
 * 29 OCT 2010  7262       rferrel     Sort Rule method buttons.
 * 15 Mar 2016  5481       randerso    Fix GUI sizing problems
 * 02 Feb 2018  6584       tgurney     Add label above buttons
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class AvailMethodsComp extends Composite {
    /**
     * Array of method data.
     */
    private ArrayList<MethodData> methodArray;

    /**
     * Callback called when a button is pressed.
     */
    private IAvailMethodSelected callback;

    /**
     * Constructor.
     *
     * @param parent
     *            Parent composite.
     * @param methodArray
     *            Array of method data.
     * @param callback
     *            Callback interface.
     */
    public AvailMethodsComp(Composite parent,
            final ArrayList<MethodData> methodArray,
            IAvailMethodSelected callback) {
        super(parent, SWT.NONE);

        this.methodArray = methodArray;
        this.callback = callback;

        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        this.setLayout(gl);
        this.setLayoutData(gd);

        createButtonControls();
    }

    /**
     * Create the method buttons.
     */
    private void createButtonControls() {
        GridData gd;

        MethodData methodData;
        Map<String, Integer> methodIndex = new HashMap<>();
        ArrayList<String> keyList = new ArrayList<>();

        for (int x = 0; x < methodArray.size(); ++x) {
            String key = methodArray.get(x).getMethodName();
            methodIndex.put(key, x);
            keyList.add(key);
        }
        Collections.sort(keyList);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label availableMethods = new Label(this, SWT.NONE);
        availableMethods.setText("Available Methods");
        availableMethods.setLayoutData(gd);

        for (String key : keyList) {
            methodData = methodArray.get(methodIndex.get(key).intValue());

            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            Button tmpBtn = new Button(this, SWT.PUSH);
            tmpBtn.setText(methodData.getMethodName());
            tmpBtn.setToolTipText(methodData.getComment());
            tmpBtn.setLayoutData(gd);
            tmpBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    Button btn = (Button) event.getSource();
                    callback.methodSelected(btn.getText());
                }
            });
        }
    }
}