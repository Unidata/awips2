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
 *           <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 13, 2012            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class TriStateCellEditor extends CellEditor {
    public static enum STATE {
        GRAYED, SELECTED, UNSELECTED
    };

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
    public void activate() {
        if (value == STATE.SELECTED) {
            value = STATE.UNSELECTED;
        } else {
            // Change either GRAYED or UNSELECTED
            value = STATE.SELECTED;
        }
        fireApplyEditorValue();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.CellEditor#createControl(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    protected Control createControl(Composite parent) {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.CellEditor#doGetValue()
     */
    @Override
    protected Object doGetValue() {
        return value;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.CellEditor#doSetFocus()
     */
    @Override
    protected void doSetFocus() {
        // Ignore
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.CellEditor#doSetValue(java.lang.Object)
     */
    @Override
    protected void doSetValue(Object value) {
        Assert.isTrue(value instanceof STATE);
        this.value = (STATE) value;
    }

    public void activate(ColumnViewerEditorActivationEvent activationEvent) {
        if (activationEvent.eventType != ColumnViewerEditorActivationEvent.TRAVERSAL) {
            super.activate(activationEvent);
        }
    }
}
