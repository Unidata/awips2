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

import java.util.List;

import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Button;

/**
 * This widget provides users with a list of selectable radio buttons.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 4, 2008	1164			jelkins	Initial creation
 * Sep 9  2010  4874            gzhou   Set default value for list
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public class RadioWidget extends ButtonWidget {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.ui.runtimeui.widgets.ButtonWidget#selectButton(org
     * .eclipse.swt.widgets.Button, java.lang.String)
     */
    @Override
    protected void selectButton(Button button, Object option) {

        if (option.equals(getValue())) {
            button.setSelection(true);
        }

    }

    /*
     * (non-Javadoc)
     * 
     * set default value as the first element of the option list
     */
    @Override
    public void setOptions(List<? extends Object> list) {
        super.setOptions(list);

        if (list.size() > 0) {
            Object obj = getValue();

            // if the value is not already set, then set default
            if (obj == null || obj.equals("")) {
                setValue(list.get(0));
            }
        }
    }

    /**
     * @param string
     */
    public RadioWidget(String label) {
        super();
        setLabel(label);
    }

    /**
     * @param label
     * @param list
     */
    public RadioWidget(String label, List<Object> list) {
        this(label);
        setOptions(list);
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
                Button widget = (Button) e.widget;
                setValue(widget.getData());
            }
        };
    }

}
