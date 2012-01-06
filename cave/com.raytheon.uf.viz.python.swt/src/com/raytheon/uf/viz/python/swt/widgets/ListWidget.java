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
package com.raytheon.uf.viz.python.swt.widgets;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 4, 2011            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ListWidget extends Widget {
    private boolean isMultiSelect;

    public ListWidget() {
        this(null, false);
    }

    public ListWidget(String label, boolean isMultiSelect) {
        super(label);
        this.isMultiSelect = isMultiSelect;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.python.swt.widgets.Widget#buildComposite(org.eclipse
     * .swt.widgets.Composite, int)
     */
    @Override
    public Composite buildComposite(Composite parent, int style) {
        Group group = new Group(parent, style);
        group.setText(makeGuiLabel(getLabel()));
        GridLayout layout = new GridLayout(1, false);
        group.setLayout(layout);
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        group.setLayoutData(layoutData);

        // TODO: change to ToggleSelectList
        org.eclipse.swt.widgets.List list = new org.eclipse.swt.widgets.List(
                group, SWT.BORDER | SWT.V_SCROLL
                        | (this.isMultiSelect ? SWT.MULTI : SWT.SINGLE));
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.heightHint = list.getItemHeight() * 30;
        list.setLayoutData(layoutData);

        List<Object> values = getValues();
        if (getOptions() != null) {
            List<String> selections = new ArrayList<String>();
            for (Object option : getOptions()) {
                String text = option.toString() != null ? option.toString()
                        : "";
                list.setData(text, option);
                list.add(text);

                if (values.contains(option)) {
                    selections.add(text);
                }
            }
            list.setSelection(selections.toArray(new String[selections.size()]));
        }

        list.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                org.eclipse.swt.widgets.List list = (org.eclipse.swt.widgets.List) e.widget;
                List<Object> values = getValues();
                values.clear();
                for (String item : list.getSelection()) {
                    values.add(list.getData(item));
                }
                setValue(values);
            }
        });

        return null;
    }

    @SuppressWarnings("unchecked")
    protected List<Object> getValues() {
        Object value = super.getValue();
        if (value == null) {
            value = new ArrayList<Object>();
        } else if (value instanceof ArrayList) {
            // do nothing
        } else if (value instanceof List) {
            value = new ArrayList<Object>((List<?>) value);
        } else {
            value = new ArrayList<Object>(Arrays.asList(value));
        }
        return (List<Object>) value;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.python.swt.widgets.Widget#getValue()
     */
    @Override
    public Object getValue() {
        List<Object> values = getValues();
        Object value = null;
        if (isMultiSelect) {
            value = values;
        } else {
            if (values.size() > 0) {
                value = values.get(0);
            }
        }
        return value;
    }
}
