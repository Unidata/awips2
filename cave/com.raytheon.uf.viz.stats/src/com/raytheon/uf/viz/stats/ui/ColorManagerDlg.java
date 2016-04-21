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
package com.raytheon.uf.viz.stats.ui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;

import com.raytheon.uf.common.stats.data.GraphData;
import com.raytheon.viz.ui.dialogs.CaveSWTDialogBase;

/**
 * TODO Add Description
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 18, 2012            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */

public class ColorManagerDlg extends CaveSWTDialogBase {
    private Composite mainComp;

    private final IColorSelection callback;

    private Button individualRdo;

    private Tree selectionTree;

    private final GraphData graphData;

    private final List<Button> radioList = new ArrayList<Button>();

    /** Map of group -> composite */
    private final Map<String, Composite> compMap = new HashMap<String, Composite>();

    private final Map<String, Map<String, Label>> labelMap = new HashMap<String, Map<String, Label>>();

    /** A map of unique keys, RGB for the check boxes that are checked. */
    private final Map<String, RGB> keyRgbMap = new LinkedHashMap<String, RGB>();;

    protected ColorManagerDlg(Shell parentShell, GraphData graphData,
            IColorSelection callback) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.MIN | SWT.RESIZE,
                CAVE.DO_NOT_BLOCK | CAVE.MODE_INDEPENDENT
                        | CAVE.INDEPENDENT_SHELL);
        setText("Color Manager");
        this.callback = callback;
        this.graphData = graphData;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;

        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        mainComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 5;
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        mainComp.setLayout(gl);
        mainComp.setLayoutData(gd);

        createGroups();
        createActionControls();
        setupColorsAndKeyRgbMap();
    }

    private void createGroups() {
        ScrolledComposite scrolledComp = new ScrolledComposite(mainComp,
                SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        scrolledComp.setLayout(gl);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 220;
        gd.heightHint = 350;
        scrolledComp.setLayoutData(gd);

        gl = new GridLayout(1, false);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite composite = new Composite(scrolledComp, SWT.NONE);
        composite.setLayout(gl);
        composite.setLayoutData(gd);
        scrolledComp.setContent(composite);
        scrolledComp.setExpandHorizontal(true);
        scrolledComp.setExpandVertical(true);

        gl = new GridLayout(1, false);
        gd = new GridData(SWT.FILL, SWT.FILL, true, false);
        Composite indComp = new Composite(composite, SWT.NONE);
        indComp.setLayout(gl);
        indComp.setLayoutData(gd);
        individualRdo = new Button(indComp, SWT.RADIO);
        individualRdo.setText("Individual");
        individualRdo.setSelection(true);
        individualRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleIndividualSelection();
            }
        });

        Map<String, List<String>> groupMap = graphData.getGroupAndNamesMap();

        for (String group : groupMap.keySet()) {
            if (!labelMap.containsKey(group)) {
                labelMap.put(group, new HashMap<String, Label>());
            }

            gl = new GridLayout(1, false);
            gd = new GridData(SWT.FILL, SWT.FILL, true, true);
            Composite comp = new Composite(composite, SWT.NONE);
            comp.setLayout(gl);
            comp.setLayoutData(gd);

            Button btn1 = new Button(comp, SWT.RADIO);
            btn1.setText(group);
            btn1.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    handleGroupSelection(((Button) e.getSource()).getText());
                }
            });

            radioList.add(btn1);

            gd = new GridData(SWT.FILL, SWT.FILL, true, true);
            gl = new GridLayout(1, false);
            ScrolledComposite sc = new ScrolledComposite(comp, SWT.BORDER);
            sc.setLayout(gl);
            sc.setLayoutData(gd);
            sc.setExpandHorizontal(true);
            sc.setExpandVertical(true);
            compMap.put(group, sc);

            gd = new GridData(SWT.FILL, SWT.FILL, true, true);
            gl = new GridLayout(2, false);
            Composite memberComp = new Composite(sc, SWT.NONE);
            memberComp.setLayout(gl);
            memberComp.setLayoutData(gd);

            sc.setContent(memberComp);
            List<String> memberList = groupMap.get(group);
            for (String member : memberList) {
                gd = new GridData(20, 10);
                Label lbl = new Label(memberComp, SWT.BORDER);
                lbl.setLayoutData(gd);
                lbl.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mouseDown(MouseEvent e) {
                        Label lbl = (Label) e.getSource();
                        handleLabelClickEvent(lbl);
                    }
                });

                Label memberLbl = new Label(memberComp, SWT.NONE);
                memberLbl.setText(member);
                labelMap.get(group).put(member, lbl);
            }

            sc.layout();

        }

        scrolledComp.layout();
    }

    private void createActionControls() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, false));
        buttonComp.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true,
                false));

        int buttonWidth = 80;

        GridData gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleApply();
                close();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button applyBtn = new Button(buttonComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleApply();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }

    /**
     * Setup the colors for the labels and put the key, RGB in the selected map.
     */
    private void setupColorsAndKeyRgbMap() {
        ColorManager cm = new ColorManager();
        int count = 0;
        Color c;

        for (String key : labelMap.keySet()) {
            count = 0;
            for (String memberKey : labelMap.get(key).keySet()) {
                c = new Color(this.getDisplay(), cm.getColorAtIndex(count));
                labelMap.get(key).get(memberKey).setBackground(c);
                ++count;
                c.dispose();
                keyRgbMap.put(key + "::" + memberKey, labelMap.get(key).get(memberKey).getBackground().getRGB());
            }
        }

        for (String key : labelMap.keySet()) {
            for (String memberKey : labelMap.get(key).keySet()) {
                keyRgbMap.put(key + "::" + memberKey, labelMap.get(key).get(memberKey).getBackground().getRGB());
            }
        }
    }

    private void handleIndividualSelection() {
        for (Button rdo: this.radioList) {
            rdo.setSelection(false);
        }

        for (String key : this.compMap.keySet()) {
            compMap.get(key).setEnabled(false);
        }
    }

    private void handleGroupSelection(String group) {
        individualRdo.setSelection(false);
        for (Button rdo: this.radioList) {
            if (!rdo.getText().equals(group)) {
                rdo.setSelection(false);
            }
        }

        for (String compMapKey: compMap.keySet()) {
            if (compMapKey.equals(group)) {
                compMap.get(compMapKey).setEnabled(true);
            } else {
                compMap.get(compMapKey).setEnabled(false);
            }
        }

    }

    /**
     * Handle the color label that is being clicked.
     *
     * @param lbl
     *            Label that was clicked.
     */
    private void handleLabelClickEvent(Label lbl) {
        RGB rgb = lbl.getBackground().getRGB();
        String key = (String) lbl.getData();

        ColorDialog colorDlg = new ColorDialog(this.getShell());
        colorDlg.setRGB(rgb);
        colorDlg.setText("Select a Color");

        RGB returnRGB = colorDlg.open();

        if (returnRGB == null) {
            return;
        }

        Color c = new Color(this.getDisplay(), returnRGB);
        lbl.setBackground(c);
        c.dispose();

        if (keyRgbMap.containsKey(key) == true) {
            keyRgbMap.put(key, returnRGB);
        }
    }

    private void handleApply() {
        // TODO Auto-generated method stub
//        System.out.println("TODO:  Implement me");
    }

}
