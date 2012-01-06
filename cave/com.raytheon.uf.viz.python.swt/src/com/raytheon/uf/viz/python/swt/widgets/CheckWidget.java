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
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Button;

import com.raytheon.uf.common.python.PyUtil;

/**
 * This class defines a check box widget.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 4, 2008	1164			jelkins	Initial creation
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public class CheckWidget extends ButtonWidget {

    /**
     * 
     */
    protected CheckWidget() {
        super();
    }

    /**
     * @param label
     */
    public CheckWidget(String label) {
        this();
        setLabel(label);
    }

    /**
     * @param string
     *            widget label
     * @param list
     *            of options
     */
    public CheckWidget(String string, List<Object> list) {
        this(string);
        setOptions(list);
    }

    /**
     * @return the values
     */
    @SuppressWarnings("unchecked")
    public List<Object> getValues() {
        return (List<Object>) value;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.ui.runtimeui.widgets.ButtonWidget#selectButton(org
     * .eclipse.swt.widgets.Button, java.lang.String)
     */
    @Override
    protected void selectButton(Button button, Object option) {

        List<Object> values = getValues();
        if (values != null && values.contains(option)) {
            button.setSelection(true);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.ui.runtimeui.widgets.ButtonWidget#setStyle()
     */
    @Override
    protected int setStyle() {
        return SWT.CHECK;
    }

    /**
     * @param values
     *            the values to set
     */
    public void setValues(List<Object> values) {
        this.value = values;
    }

    /**
     * Values which should be checked
     * 
     * @param values
     *            the values to set
     */
    public void setValues(Object[] values) {
        setValues(Arrays.asList(values));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.ui.runtimeui.widgets.Widget#toString()
     */
    @Override
    public String toString() {
        String[] stringTuple = super.toString().split(",");
        stringTuple[1] = PyUtil.objListToList(getValues());
        String output = "";
        for (String s : stringTuple) {
            output = output + s + ',';
        }
        return output.substring(0, output.length() - 1);
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.gfe.ui.runtimeui.widgets.ButtonWidget#
     * buttonSelectionListener()
     */
    @Override
    protected SelectionListener buttonSelectionListener() {
        return new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                List<Object> list = new ArrayList<Object>();

                for (Button b : getButtons()) {
                    if (b.getSelection()) {
                        list.add(b.getData());
                    }
                }
                setValues(list);

            }

        };
    }
}
