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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.gfe.ui.zoneselector.ZoneSelector;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.widgets.ToggleSelectList;

/**
 * Display the Examine/Clear Combinations dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 18 APR 2008  ###        lvenable    Initial creation
 * 08 JUL 2011  9928       rferrel     Factor in "Include All Zones"
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class ClearZoneGroupsDialog extends CaveJFACEDialog {
    private String combinationName;

    private ToggleSelectList dataList;

    private ZoneSelector zoneSelector;

    private Map<Integer, List<String>> inverseComboDict;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param combinationName
     *            Combination name.
     */
    public ClearZoneGroupsDialog(Shell parent, ZoneSelector zoneSelector,
            String combinationName) {
        super(parent);

        this.zoneSelector = zoneSelector;
        this.combinationName = combinationName;
        encodeComboList();
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
        newShell.setText("Examine/Clear Combinations (" + combinationName + ")");
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

        Label topLabel = new Label(comp, SWT.CENTER);
        topLabel.setText("Select Zone Groups to Delete");

        dataList = new ToggleSelectList(comp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL | SWT.H_SCROLL);

        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        layoutData.heightHint = dataList.getItemHeight() * 10;
        layoutData.widthHint = this.convertWidthInCharsToPixels(100);
        dataList.setLayoutData(layoutData);

        for (Integer group : inverseComboDict.keySet()) {
            String label = String.format("Region%02d: ", group.intValue());
            label += inverseComboDict.get(group);
            dataList.add(label);
            dataList.setData(label, inverseComboDict.get(group));
        }

        dataList.setSelection(0);
        dataList.deselectAll();

        return comp;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#okPressed()
     */
    @SuppressWarnings("unchecked")
    @Override
    protected void okPressed() {
        Map<String, Integer> comboDict = zoneSelector.getCombos();
        String[] selections = dataList.getSelection();
        for (String s : selections) {
            for (String z : (List<String>) dataList.getData(s)) {
                comboDict.remove(z);
            }
        }
        zoneSelector.updateCombos(comboDict);

        super.okPressed();
    }

    private void encodeComboList() {
        Map<String, Integer> comboDict = zoneSelector.getCombos();
        inverseComboDict = new HashMap<Integer, List<String>>();

        for (String k : comboDict.keySet()) {
            Integer group = comboDict.get(k);
            List<String> list = inverseComboDict.get(group);
            if (list == null) {
                list = new ArrayList<String>();
                inverseComboDict.put(group, list);
            }
            list.add(k);
        }
    }
}
