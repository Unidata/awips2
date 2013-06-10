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
package com.raytheon.viz.gfe.dialogs.formatterlauncher;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.gfe.type.Pair;
import com.raytheon.viz.gfe.ui.zoneselector.ZoneSelector;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.widgets.ToggleSelectList;

/**
 * Display the Shuffle Groups dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 18 APR 2008  ###        lvenable    Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class ShuffleZoneGroupsDialog extends CaveJFACEDialog {
    private String combinationName;

    private ToggleSelectList dataList;

    private ZoneSelector zoneSelector;

    private List<Pair<List<String>, String>> groupList;

    private List<String> labels;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param combinationName
     *            Combination name.
     */
    public ShuffleZoneGroupsDialog(Shell parent, ZoneSelector zoneSelector,
            String combinationName) {
        super(parent);

        this.zoneSelector = zoneSelector;
        this.combinationName = combinationName;
        this.groupList = encodeComboList(zoneSelector.getCombos());
        this.labels = makeLabels(this.groupList);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell newShell) {
        newShell.setText("Shuffle Combinations (" + combinationName + ")");
        super.configureShell(newShell);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        Composite comp = (Composite) super.createDialogArea(parent);
        GridLayout layout = (GridLayout) comp.getLayout();
        layout.numColumns = 2;

        // ----------------------------------------------------------------------
        // Create the Label and List controls on the left side of the dialog.
        // ----------------------------------------------------------------------
        Composite leftComp = new Composite(comp, SWT.NONE);
        leftComp.setLayout(new GridLayout(1, false));

        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label topLabel = new Label(leftComp, SWT.CENTER);
        topLabel.setText("Select Zone Groups to Shuffle");
        topLabel.setLayoutData(layoutData);

        dataList = new ToggleSelectList(leftComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL | SWT.H_SCROLL);

        layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        layoutData.heightHint = dataList.getItemHeight() * 10;
        layoutData.widthHint = this.convertWidthInCharsToPixels(100);
        dataList.setLayoutData(layoutData);
        for (int i = 0; i < this.labels.size(); i++) {
            dataList.add(labels.get(i));
            dataList.setData(labels.get(i), groupList.get(i));
        }

        // ----------------------------------------------------------------------
        // Create the button controls on the right side of the dialog.
        // ----------------------------------------------------------------------
        Composite rightComp = new Composite(comp, SWT.NONE);
        rightComp.setLayout(new GridLayout(1, false));
        rightComp.setLayoutData(new GridData(SWT.DEFAULT, SWT.CENTER, false,
                true));

        SelectionListener buttonListener = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                moveEntries((Integer) e.widget.getData());
            }
        };

        Button topBtn = new Button(rightComp, SWT.PUSH);
        topBtn.setText("Top");
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        topBtn.setLayoutData(layoutData);
        topBtn.setData(new Integer(-999));
        topBtn.addSelectionListener(buttonListener);

        Button upBtn = new Button(rightComp, SWT.PUSH);
        upBtn.setText("Up");
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        upBtn.setLayoutData(layoutData);
        upBtn.setData(new Integer(-1));
        upBtn.addSelectionListener(buttonListener);

        Button downBtn = new Button(rightComp, SWT.PUSH);
        downBtn.setText("Down");
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        downBtn.setLayoutData(layoutData);
        downBtn.setData(new Integer(1));
        downBtn.addSelectionListener(buttonListener);

        Button bottomBtn = new Button(rightComp, SWT.PUSH);
        bottomBtn.setText("Bottom");
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        bottomBtn.setLayoutData(layoutData);
        bottomBtn.setData(new Integer(999));
        bottomBtn.addSelectionListener(buttonListener);

        return comp;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.CANCEL_ID, "Dismiss", false);
    }

    private void moveEntries(int diff) {
        // moves the entries around in the listbox
        // diff + move down, diff - move up. Large number moves are considered
        // top/bottom.
        String[] selections = this.dataList.getSelection();
        if (selections.length == 0) {
            return;
        }

        // calculate indexes
        int[] indexes = new int[selections.length];
        for (int i = 0; i < selections.length; i++) {
            indexes[i] = this.labels.indexOf(selections[i]);
        }
        Arrays.sort(indexes);

        // determine how far we can move, and what we will move
        int maxUp = indexes[0];
        int maxDown = this.groupList.size() - indexes[indexes.length - 1] - 1;
        if (diff > 0) {
            if (diff > maxDown) {
                diff = maxDown;
            }
        } else if (diff < 0) {
            if (Math.abs(diff) > maxUp) {
                diff = -maxUp;
            }
        }

        // modified diff is how much we can move in the direction wanted
        if (diff == 0) {
            return;
        } // can't move, already at limit

        // now do del/inserts to shuffle the list
        if (diff > 0) {
            Collections.reverse(Arrays.asList(indexes));
        }

        for (int i : indexes) {
            Pair<List<String>, String> gl = this.groupList.get(i);
            this.groupList.remove(i);
            this.groupList.add(i + diff, gl);
        }

        this.labels = this.makeLabels(this.groupList);
        this.dataList.setItems(this.labels.toArray(new String[labels.size()]));
        for (int i : indexes) {
            this.dataList.select(i + diff);
        }

        this.updateZoneCombiner(this.groupList);
    }

    private void updateZoneCombiner(List<Pair<List<String>, String>> combolist) {
        Map<String, Integer> comboDict = new HashMap<String, Integer>();
        for (Pair<List<String>, String> pair : combolist) {
            List<String> zones = pair.getFirst();
            String label = pair.getSecond();
            int group = Integer.parseInt((label.substring(6, 8)));
            for (String z : zones) {
                comboDict.put(z, group);
            }
        }

        zoneSelector.updateCombos(comboDict);
    }

    private List<Pair<List<String>, String>> encodeComboList(
            Map<String, Integer> comboDict) {
        HashMap<String, List<String>> inverseComboDict = new HashMap<String, List<String>>();

        for (String k : comboDict.keySet()) {
            String group = String.format("Region%02d", comboDict.get(k)
                    .intValue());
            List<String> list = inverseComboDict.get(group);
            if (list == null) {
                list = new ArrayList<String>();
                inverseComboDict.put(group, list);
            }
            list.add(k);
        }

        List<Pair<List<String>, String>> comboList = new ArrayList<Pair<List<String>, String>>();
        List<String> keys = new ArrayList<String>(inverseComboDict.keySet());
        Collections.sort(keys);
        for (String k : keys) {
            comboList.add(new Pair<List<String>, String>(inverseComboDict
                    .get(k), k));
        }

        return comboList;
    }

    private List<String> makeLabels(List<Pair<List<String>, String>> comboList) {
        List<String> labels = new ArrayList<String>();

        for (int i = 0; i < comboList.size(); i++) {
            Pair<List<String>, String> pair = comboList.get(i);
            List<String> zones = pair.getFirst();
            String newLabel = String.format("Region%02d: ", i + 1)
                    + zones.toString();
            labels.add(newLabel);
            pair.setSecond(newLabel);
        }
        return labels;
    }
}
