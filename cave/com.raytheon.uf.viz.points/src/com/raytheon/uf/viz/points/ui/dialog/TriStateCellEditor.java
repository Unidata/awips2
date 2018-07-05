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
package com.raytheon.uf.viz.points.ui.dialog;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ColumnViewerEditorActivationEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * A cell editor that manages a tri-state checkbox. The cell editor's value is
 * one of the STATE enum values.
 * <p>
 * This class may be instantiated; it is not intended to be subclassed.
 * </p>
 * <p>
 * Note that this implementation simply fakes it and does does not create any
 * new controls. The mere activation of this editor means that the value of the
 * check box is being toggled by the end users; the listener method
 * <code>applyEditorValue</code> is immediately called to signal the change.
 * </p>
 * 
 * @noextend This class is not intended to be subclassed by clients.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 13, 2012            rferrel     Initial creation
 * Apr 18, 2017  6237      njensen     Added enum javadoc, constructor, and toString()
 * 
 * </pre>
 * 
 * @author rferrel
 */

public class TriStateCellEditor extends CellEditor {

    /**
     * The state of the checkbox. A tri-state checkbox has a selected,
     * unselected, and third state which often indicates a partial selection.
     * SWT refers to the third state as "grayed".
     */
    public static enum STATE {
        GRAYED("grayed"), SELECTED("checked"), UNSELECTED("unchecked");

        protected final String iconName;

        /**
         * Constructor
         * 
         * @param iconName
         *            the name of the icon to use for this state
         */
        STATE(String iconName) {
            this.iconName = iconName;
        }

        @Override
        public String toString() {
            return this.iconName;
        }
    }

    /**
     * Default CheckboxCellEditor style
     */
    private static final int defaultStyle = SWT.NONE;

    /**
     * The cell value.
     */
    private STATE value = STATE.GRAYED;

    /**
     * Creates a new cell editor with no control
     */
    public TriStateCellEditor() {
        setStyle(defaultStyle);
    }

    /**
     * Creates a new cell editor parented under the given control. The cell
     * editor value is a STATE value, which is initially <code>GRAYED</code>.
     * Initially, the cell editor has no cell validator.
     * 
     * @param parent
     *            the parent control
     */
    public TriStateCellEditor(Composite parent) {
        this(parent, defaultStyle);
    }

    /**
     * Creates a new cell editor parented under the given control. The cell
     * editor value is a STATE value, which is initially <code>GRAYED</code>.
     * Initially, the cell editor has no cell validator.
     * 
     * @param parent
     *            the parent control
     * @param style
     *            the style bits
     */
    public TriStateCellEditor(Composite parent, int style) {
        super(parent, style);
    }

    /**
     * The <code>TriStateCellEditor</code> implementation of this
     * <code>CellEditor</code> framework method simulates the toggling of the
     * checkbox control and notifies listeners with
     * <code>ICellEditorListener.applyEditorValue</code>.
     */
    @Override
    public void activate() {
        if (value == STATE.SELECTED) {
            value = STATE.UNSELECTED;
        } else {
            // Change either GRAYED or UNSELECTED
            value = STATE.SELECTED;
        }
        fireApplyEditorValue();
    }

    @Override
    protected Control createControl(Composite parent) {
        return null;
    }

    @Override
    protected Object doGetValue() {
        return value;
    }

    @Override
    protected void doSetFocus() {
        // Ignore
    }

    @Override
    protected void doSetValue(Object value) {
        Assert.isTrue(value instanceof STATE);
        this.value = (STATE) value;
    }

    @Override
    public void activate(ColumnViewerEditorActivationEvent activationEvent) {
        if (activationEvent.eventType != ColumnViewerEditorActivationEvent.TRAVERSAL) {
            super.activate(activationEvent);
        }
    }
}
