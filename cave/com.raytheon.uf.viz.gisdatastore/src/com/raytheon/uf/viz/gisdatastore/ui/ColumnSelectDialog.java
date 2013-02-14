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
package com.raytheon.uf.viz.gisdatastore.ui;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.widgets.duallist.DualList;
import com.raytheon.viz.ui.widgets.duallist.DualListConfig;

/**
 * Dialog to allow user to select the columns to be displayed and the order in
 * which to display them
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 30, 2012            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ColumnSelectDialog extends CaveJFACEDialog {

    private String[] available;

    private List<String> selected;

    private DualList dualList;

    protected ColumnSelectDialog(Shell parentShell, String[] available,
            String[] selected) {
        super(parentShell);
        this.available = available;
        this.selected = new ArrayList<String>(Arrays.asList(selected));
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Select Columns");
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite comp = (Composite) super.createDialogArea(parent);

        DualListConfig config = new DualListConfig();
        config.setAvailableListLabel("Available");
        config.setFullList(Arrays.asList(available));
        config.setSelectedListLabel("Displayed");
        config.setSelectedList(selected);
        config.setListHeight(100);
        config.setListWidth(100);
        config.setShowUpDownBtns(true);
        dualList = new DualList(comp, SWT.NONE, config);

        return comp;
    }

    public String[] getSelectedColumns() {
        return dualList.getSelectedListItems();
    }
}
