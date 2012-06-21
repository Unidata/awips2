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

import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.python.PyUtil;

/**
 * Runtime widget.
 * <p>
 * Runtime widgets are built by extending this class.
 * </p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 3, 2008	1164     	jelkins     Initial creation
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public abstract class Widget {

    private String label;

    protected Object value;

    private Object variable;

    private List<? extends Object> options;

    /**
     * Class constructor
     */
    protected Widget() {
        options = new ArrayList<Object>();
    }

    /**
     * Class constructor initializing this widget with the given label.
     * 
     * @param label
     *            this widget's label
     */
    public Widget(String label) {
        this();
        this.label = label;
    }

    /**
     * @return the label displayed by the widget
     */
    public String getLabel() {
        return label;
    }

    /**
     * Set the label
     * <p>
     * The label is use to visually identify the widget in the user interface
     * and identify the widget within the <code>values</code> Map.
     * </p>
     * 
     * @param label
     *            displayed by the widget
     */
    public void setLabel(String label) {
        this.label = label;
    }

    /**
     * @return the widget value
     */
    public Object getValue() {
        return value;
    }

    /**
     * @param defaultValue
     *            the initial value of the widget
     */
    public void setValue(Object defaultValue) {
        this.value = defaultValue;
    }

    /**
     * @return the options
     */
    public List<? extends Object> getOptions() {
        return options;
    }

    /**
     * @param elementList
     *            the options to set
     */
    public void setOptions(List<? extends Object> elementList) {
        this.options = elementList;
    }

    public void setOptions(Object[] options) {
        this.options = Arrays.asList(options);
    }

    /*
     * (non-Javadoc) Format the Widget according to: ("Variable Name", default
     * value, variable type, optional list)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        char Q = '\''; // quote
        char C = ','; // comma

        String valueString = "None";

        if (value != null) {
            valueString = Q + value.toString() + Q;
        }

        return "(" + Q + label + Q + C + valueString + C + Q
                + getClass().getSimpleName() + Q + C
                + PyUtil.objListToList(options) + ")";
    }

    /**
     * Builds a composite for the widget attached to the given parent.
     * 
     * @param parent
     * @param style
     * @return Composite capable of being placed into an SWT container.
     */
    public abstract Composite buildComposite(Composite parent, int style);

    /**
     * @param variable
     *            the variable to set
     */
    public void setVariable(Object variable) {
        this.variable = variable;
    }

    /**
     * @return the variable
     */
    public Object getVariable() {
        return variable;
    }

    protected String makeGuiLabel(String s) {
        return s.replace("&", "&&");
    }

}
