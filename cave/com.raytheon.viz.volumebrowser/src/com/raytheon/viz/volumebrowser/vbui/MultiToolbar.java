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
package com.raytheon.viz.volumebrowser.vbui;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.menus.IMenuService;

/**
 * 
 * Implementation of multiline toolbars. Use add and populate to build a toolbar
 * and then it can be split with the splitToMultipleBars method. This
 * implementation is not smart enough to do any wrapping for you.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 2, 2011            bsteffen     Initial creation
 * Jul 31, 2012 #875       rferrel     Added add(IAction) method.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class MultiToolbar extends Composite {

    private final List<ToolBarManager> tbms = new ArrayList<ToolBarManager>();

    private final List<SelectionListener> toolItemSelectionListeners = new ArrayList<SelectionListener>();

    private int numBars = 1;

    // indicates that items have been added since the last split, calling update
    // will trigger a resplit.
    private boolean isDirty = false;

    public MultiToolbar(Composite parent, int style) {
        super(parent, style);
        setLayout(new GridLayout(1, false));
    }

    /**
     * Create a new toolbar that is centered on the composite.
     **/
    private ToolBarManager createToolBarManager() {
        ToolBarManager tbm = new ToolBarManager(SWT.NONE);
        GridData data = new GridData(GridData.CENTER, GridData.CENTER, true,
                false);
        data.horizontalAlignment = GridData.CENTER;
        tbm.createControl(this).setLayoutData(data);
        tbms.add(tbm);
        return tbm;
    }

    /**
     * add items to the bottom toolbar, or if split has not been called than add
     * to the only toolbar. If you need to relayout the toolbars after this then
     * call split again.
     **/
    public void add(IContributionItem item) {
        getToolBarManager().add(item);
    }

    /**
     * Add items to the bottom toolbar, or if split has not been called than add
     * to the only toolbar. If you need to relayout the toolbars after this then
     * call split again.
     **/
    public void add(IAction item) {
        getToolBarManager().add(item);
    }

    private ToolBarManager getToolBarManager() {
        isDirty = true;
        ToolBarManager tbm = null;
        if (tbms.isEmpty()) {
            tbm = createToolBarManager();
        } else {
            tbm = tbms.get(tbms.size() - 1);
        }
        return tbm;
    }

    public void update() {
        if (isDirty && numBars > 1) {
            splitToMultipleBars(numBars);
        }
        super.update();
        for (ToolBarManager tbm : tbms) {
            tbm.update(true);
            for (ToolItem item : tbm.getControl().getItems()) {
                for (SelectionListener listener : toolItemSelectionListeners) {
                    item.addSelectionListener(listener);
                }
            }
        }

    }

    /**
     * Delete all toolbars and toolItems from this widget but do not dispose the
     * widget, adding or populating will rebuild new toolbars.
     */
    public void disposeToolbars() {
        for (ToolBarManager tbm : tbms) {
            for (IContributionItem item : tbm.getItems()) {
                item.dispose();
            }
            tbm.dispose();
        }
        tbms.clear();
        numBars = 1;
    }

    /**
     * Use menu service to populate toolbar with items in the select location.
     * 
     * @param location
     */
    public void populate(String location) {
        isDirty = true;
        ToolBarManager tbm = null;
        if (tbms.isEmpty()) {
            tbm = createToolBarManager();
        } else {
            tbm = tbms.get(tbms.size() - 1);
        }
        IMenuService ms = (IMenuService) PlatformUI.getWorkbench().getService(
                IMenuService.class);
        ms.populateContributionManager(tbm, location);
    }

    /**
     * split into multiple bars, this is probably why you are using this class.
     * 
     * @param numBars
     */
    public void splitToMultipleBars(int numBars) {
        this.numBars = numBars;
        List<IContributionItem> items = new ArrayList<IContributionItem>();
        for (ToolBarManager tbm : tbms) {
            items.addAll(Arrays.asList(tbm.getItems()));
        }
        disposeToolbars();
        splitContruibutionItems(items, numBars);
        isDirty = false;
        update();
    }

    private void splitContruibutionItems(List<IContributionItem> items,
            int numBars) {
        // The reason we recalculate for every row is so that 13 in four rows is
        // 4,3,3,3 instead of 4,4,4,1
        int itemsPerBar = items.size() / numBars;
        if (items.size() % numBars != 0) {
            itemsPerBar += 1;
        }
        ToolBarManager tbm = createToolBarManager();
        for (int i = 0; i < itemsPerBar; i++) {
            tbm.add(items.remove(0));
        }
        if (numBars > 1) {
            splitContruibutionItems(items, numBars - 1);
        }
    }

    public void addToolItemSelectionListener(SelectionListener listener) {
        toolItemSelectionListeners.add(listener);
    }

}
