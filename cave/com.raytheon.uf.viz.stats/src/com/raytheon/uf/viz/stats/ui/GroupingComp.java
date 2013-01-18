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

/**
 * TODO Add Description
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 16, 2012            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
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

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import com.raytheon.uf.common.stats.data.GraphData;

/**
 * Composites that contains the controls to change colors and to determine what
 * is displayed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 16, 2012            lvenable    Initial creation
 * Jan 11, 2013   1357     mpduff      Implement.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class GroupingComp extends Composite implements IGroupSelection {
    /** Parse pattern */
    private final Pattern colonPattern = Pattern.compile(":");

    /** Selection Manager Dialog */
    private SelectionManagerDlg selectionMangerDlg;

    /** Scrolled composite containing the control widgets composite. */
    private ScrolledComposite scrolledComp;

    /** Composite containing the control widgets. */
    private Composite controlComp;

    /** A map of unique keys, Label controls. */
    private Map<String, Label> labelMap;

    /** A map of unique keys, Check box controls. */
    private Map<String, Button> checkBtnMap;

    /** A map of unique keys, RGB for the check boxes that are checked. */
    private Map<String, RGB> keyRgbMap;

    /** The graph data */
    private final GraphData graphData;

    /** Grouping callback */
    private final IStatsGroup callback;

    /**
     * Constructor.
     * 
     * @param parentComp
     *            Parent composite.
     * @param swtStyle
     *            SWT style.
     * @param statsData
     *            Statistical data.
     */
    public GroupingComp(Composite parentComp, int swtStyle,
            GraphData graphData, IStatsGroup callback) {
        super(parentComp, swtStyle);
        this.graphData = graphData;
        this.callback = callback;

        init();
    }

    /**
     * Initialize the class.
     */
    private void init() {
        labelMap = new LinkedHashMap<String, Label>();
        checkBtnMap = new LinkedHashMap<String, Button>();
        keyRgbMap = new LinkedHashMap<String, RGB>();

        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);

        this.setLayout(gl);
        this.setLayoutData(gd);

        Label grpLbl = new Label(this, SWT.NONE);
        grpLbl.setText("Groups:");

        createScrolledComposite();
        setupColorsAndKeyRgbMap();
        addSelectionColorActionButtons();

        fireCallback();
    }

    /**
     * Create the scrolled composite and set the content.
     */
    private void createScrolledComposite() {

        scrolledComp = new ScrolledComposite(this, SWT.BORDER | SWT.H_SCROLL
                | SWT.V_SCROLL);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        scrolledComp.setLayout(gl);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 220;
        gd.heightHint = 350;
        scrolledComp.setLayoutData(gd);

        controlComp = new Composite(scrolledComp, SWT.NONE);
        controlComp.setLayout(new GridLayout(2, false));
        controlComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        createControls();

        controlComp.layout();

        scrolledComp.setContent(controlComp);
        scrolledComp.setExpandHorizontal(true);
        scrolledComp.setExpandVertical(true);
        scrolledComp.addControlListener(new ControlAdapter() {
            @Override
            public void controlResized(ControlEvent e) {
                scrolledComp.setMinSize(controlComp.computeSize(SWT.DEFAULT,
                        SWT.DEFAULT));
            }
        });
        scrolledComp.layout();
    }

    /**
     * Create the color labels and check box controls.
     */
    private void createControls() {
        List<String> keyArray = graphData.getKeysWithData();

        for (String key : keyArray) {
            GridData gd = new GridData(20, 10);
            Label lbl = new Label(controlComp, SWT.BORDER);
            lbl.setLayoutData(gd);
            lbl.setData(key);
            lbl.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseDown(MouseEvent e) {
                    Label lbl = (Label) e.getSource();
                    handleLabelClickEvent(lbl);
                }
            });

            Button btn = new Button(controlComp, SWT.CHECK);
            btn.setText(key);
            btn.setSelection(true);
            btn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    Button btn = (Button) e.getSource();
                    handleCheckEvent(btn);
                }
            });

            labelMap.put(key, lbl);
            checkBtnMap.put(key, btn);
        }
    }

    /**
     * Add the Selection and Color Manager buttons.
     */
    private void addSelectionColorActionButtons() {
        if (checkBtnMap.isEmpty()) {
            return;
        }

        Composite buttonComp = new Composite(this, SWT.NONE);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true,
                false));

        int buttonWidth = 160;
        GridData gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button selMgrBtn = new Button(buttonComp, SWT.PUSH);
        selMgrBtn.setText("Selection Manager...");
        selMgrBtn.setLayoutData(gd);
        selMgrBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                displaySelectionMgrDlg();
            }
        });
        // Not including this functionality in the branch.
        // gd = new GridData(buttonWidth, SWT.DEFAULT);
        // Button colorMgrBtn = new Button(buttonComp, SWT.PUSH);
        // colorMgrBtn.setText("Color Manager...");
        // colorMgrBtn.setLayoutData(gd);
        // colorMgrBtn.addSelectionListener(new SelectionAdapter() {
        // @Override
        // public void widgetSelected(SelectionEvent e) {
        // displayColorMgrDlg();
        // }
        // });
    }

    /**
     * Handle the check button event.
     * 
     * @param btn
     *            Check box being checked/unchecked.
     */
    private void handleCheckEvent(Button btn) {
        String key = btn.getText();

        if (btn.getSelection() == true) {
            keyRgbMap.put(key, labelMap.get(key).getBackground().getRGB());
        } else {
            keyRgbMap.remove(key);
        }

        fireCallback();
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

        fireCallback();
    }

    /**
     * Call the callback.
     */
    private void fireCallback() {
        callback.setGroupData(keyRgbMap);
    }

    /**
     * Setup the colors for the labels and put the key, RGB in the selected map.
     */
    private void setupColorsAndKeyRgbMap() {
        ColorManager cm = new ColorManager();
        int count = 0;
        Color c;

        for (String key : labelMap.keySet()) {
            c = new Color(this.getDisplay(), cm.getColorAtIndex(count));
            labelMap.get(key).setBackground(c);
            ++count;
            c.dispose();
        }

        for (String key : checkBtnMap.keySet()) {
            if (checkBtnMap.get(key).getSelection() == true) {
                keyRgbMap.put(key, labelMap.get(key).getBackground().getRGB());
            }
        }
    }

    /**
     * Display the Selection Manager dialog.
     */
    private void displaySelectionMgrDlg() {
        if (selectionMangerDlg == null || selectionMangerDlg.isDisposed()) {
            selectionMangerDlg = new SelectionManagerDlg(getShell(), graphData,
                    this);
        }

        selectionMangerDlg.open();
    }

    /**
     * Display the Color Manager dialog.
     */
    // Implementing in the next release.
    // private void displayColorMgrDlg() {
    // ColorManagerDlg dlg = new ColorManagerDlg(getShell(), graphData, this);
    // dlg.open();
    // }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setSelections(Map<String, Map<String, Boolean>> selectionMap) {
        List<String> keySequence = graphData.getKeySequence();

        Multimap<String, String> offMap = ArrayListMultimap.create();
        for (String key : selectionMap.keySet()) {
            for (String selection : selectionMap.get(key).keySet()) {
                if (!selectionMap.get(key).get(selection)) {
                    offMap.put(key, selection);
                }
            }
        }

        if (offMap.size() == 0) {
            for (String btnKey : checkBtnMap.keySet()) {
                checkBtnMap.get(btnKey).setSelection(true);
                keyRgbMap.put(btnKey, labelMap.get(btnKey).getBackground()
                        .getRGB());
            }
        } else {
            for (String btnKey : checkBtnMap.keySet()) {
                String[] parts = colonPattern.split(btnKey);

                for (String group : offMap.keySet()) {
                    for (String part : parts) {
                        int idx = keySequence.indexOf(part);
                        if (idx >= 0
                                && offMap.get(group).contains(
                                        keySequence.get(idx))) {
                            checkBtnMap.get(btnKey).setSelection(false);
                            keyRgbMap.remove(btnKey);
                        } else {
                            checkBtnMap.get(btnKey).setSelection(true);
                            keyRgbMap.put(btnKey, labelMap.get(btnKey)
                                    .getBackground().getRGB());
                        }
                    }
                }
            }
        }

        fireCallback();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setItemsOff(List<String> keys) {
        for (String key : keys) {
            if (checkBtnMap.containsKey(key)) {
                checkBtnMap.get(key).setSelection(false);
                keyRgbMap.remove(key);
            }
        }

        fireCallback();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Boolean> getStates() {
        Map<String, Boolean> stateMap = new HashMap<String, Boolean>();

        for (Map.Entry<String, Button> state : checkBtnMap.entrySet()) {
            stateMap.put(state.getKey(), state.getValue().getSelection());
        }

        return stateMap;
    }
}
