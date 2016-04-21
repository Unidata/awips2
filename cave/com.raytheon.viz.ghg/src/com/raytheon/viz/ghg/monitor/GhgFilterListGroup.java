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
package com.raytheon.viz.ghg.monitor;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;

import com.raytheon.viz.ghg.monitor.data.GhgConfigData;
import com.raytheon.viz.ghg.monitor.data.GhgDataFilter;
import com.raytheon.viz.ui.widgets.ToggleSelectList;

/**
 * This class contains a filter list and selection controls..
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 25 MAR 2008  N/A        lvenable    Initial creation 
 * 17Jun2008    1157       MW Fegan    Converted List&lt;String&gt; to String[] in 
 *                                     interactions with selection data.
 * 30 Jul 2010  6721       mpduff      Fixed an all/none selection problem.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class GhgFilterListGroup extends Composite {
    /**
     * Data list control.
     */
    private ToggleSelectList dataList;

    private String[] values = null;

    private GhgConfigData.AlertsFilterEnum type = null;

    /**
     * Title for the group container.
     */
    private String title;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public GhgFilterListGroup(Composite parent,
            GhgConfigData.AlertsFilterEnum type, String[] values) {
        super(parent, SWT.NONE);
        this.type = type;
        title = type.name;
        this.values = values;

        init();
    }

    /**
     * Initialize the composite.
     */
    private void init() {
        GridData gd = new GridData();
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        setLayout(gl);
        setLayoutData(gd);

        Group mainGroup = new Group(this, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        mainGroup.setLayout(gl);

        mainGroup.setText(title);

        initializeComponents(mainGroup);

        setInitialData();
    }

    /**
     * Initialize the controls on the composite.
     * 
     * @param groupComp
     *            Group container.
     */
    private void initializeComponents(Group groupComp) {
        GridData gd = new GridData(100, 300);
        gd.horizontalSpan = 2;

        dataList = new ToggleSelectList(groupComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL);
        dataList.setLayoutData(gd);
        dataList.addSelectionListener(new SelectionAdapter() {
            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateDisplay();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button allBtn = new Button(groupComp, SWT.PUSH);
        allBtn.setText("All");
        allBtn.setLayoutData(gd);
        allBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                dataList.selectAll();
                updateDisplay();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button noneBtn = new Button(groupComp, SWT.PUSH);
        noneBtn.setText("None");
        noneBtn.setLayoutData(gd);
        noneBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                dataList.deselectAll();
                updateDisplay();
            }
        });
    }

    private void setInitialData() {
        if (values == null) {
            return;
        }
        for (String value : values) {
            dataList.add(value);
        }
    }

    public GhgConfigData.AlertsFilterEnum getType() {
        return type;
    }

    /**
     * Set the items for the list box.
     * 
     * @param values
     */
    public void setListValues(String[] values) {
        dataList.removeAll();
        dataList.setItems(values);
    }

    /**
     * 
     * @param values
     */
    public void setSelValues(String[] values) {
        ArrayList<Integer> items = new ArrayList<Integer>();
        for (String value : values) {
            int index = dataList.indexOf(value);
            if (index != -1) {
                items.add(index);
            }
        }
        int[] ints = new int[items.size()];
        for (int i = 0; i < items.size(); i++) {
            ints[i] = items.get(i);
        }
        if (items.size() != 0) {
            dataList.select(ints);
        }
    }

    /**
     * Returns a list of values that are selected in the list box.
     * 
     * @return
     */
    public String[] getSelections() {
        String[] items = new String[0];
        if (dataList.getSelectionCount() > 0) {
            items = dataList.getSelection();
        }

        return items;
    }

    /**
     * Update the display based on the filter changes.
     */
    protected void updateDisplay() {
        GhgConfigData config = GhgConfigData.getInstance();
        GhgDataFilter filter = config.getCurrentFilter();
        filter.setFilterByType(type, getSelections());
        filter.name = GhgConfigData.CUSTOM_FILTER_NAME;
        config.setCurrentFilter(filter);
    }
}