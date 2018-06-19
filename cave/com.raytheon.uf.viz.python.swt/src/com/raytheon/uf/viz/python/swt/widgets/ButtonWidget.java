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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;

/**
 * Button superclass to handle widgets such as checkslists and radiolists.
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

public abstract class ButtonWidget extends Widget {

    private ArrayList<Button> buttons;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.ui.dynamic.ISelectionInput#buildComposite(org.eclipse
     * .swt.widgets.Composite, int)
     */
    @Override
    public Composite buildComposite(Composite parent) {

        Group group = new Group(parent, SWT.NONE);
        group.setText(makeGuiLabel(getLabel()));
        GridData layoutData = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        group.setLayoutData(layoutData);

        GridLayout layout = new GridLayout(1, false);
        layout.marginHeight = 0;
        layout.verticalSpacing = 0;
        group.setLayout(layout);

        buttons = new ArrayList<Button>();

        if (getOptions() != null) {
            for (Object option : getOptions()) {
                Button button = new Button(group, setStyle());
                layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT, false,
                        false);
                button.setLayoutData(layoutData);
                button.setData(option);
                String text = option.toString() != null ? option.toString()
                        : "";
                button.setText(makeGuiLabel(text));
                button.addSelectionListener(buttonSelectionListener());

                selectButton(button, option);

                buttons.add(button);
            }
        }

        return group;
    }

    /**
     * Handle button selection events
     * <p>
     * This method handles select events appropriate to this widget.
     * </p>
     * 
     * @return a selection listener for handling button selection events
     */
    protected abstract SelectionListener buttonSelectionListener();

    /**
     * Determine if the button option should be checked
     * 
     * @param button
     * @param option
     */
    protected void selectButton(Button button, Object option) {
    }

    /**
     * Override this function to set the style of button for this widget.
     * <p>
     * The default style is <code>SWT.RADIO</code>. For check buttons return a
     * style of <code>SWT.CHECK</code>. See the SWT documentation for more
     * button styles.
     * </p>
     * 
     * @return the style of button
     */
    protected int setStyle() {
        return SWT.RADIO;
    }

    /**
     * @return the buttons
     */
    public ArrayList<Button> getButtons() {
        return buttons;
    }

    /**
     * Sets the buttons to be displayed
     * 
     * @param options
     *            an array of button labels
     */
    @Override
    public void setOptions(Object[] options) {
        super.setOptions(options);
    }

}
