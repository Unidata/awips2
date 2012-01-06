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
package com.raytheon.viz.ui.widgets;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.ListenerList;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.List;

/**
 * Provides a multi-selection list box similar to
 * {@link org.eclipse.swt.widgets.List} which allows an item's selection state
 * to be toggled by clicking without pressing the Shift or Control keys. <br>
 * <br>
 * NOTE: pressing the Shift and/or Control keys may result in unusual behavior.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 25, 2009      #2315 randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ToggleSelectList extends Composite {
    private List list;

    private Set<Integer> selectionIndices = new HashSet<Integer>();

    private ListenerList selectionListeners = new ListenerList(
            ListenerList.IDENTITY);

    /**
     * Constructs a new instance of this class given its parent and a style
     * value describing its behavior and appearance. <br>
     * <br>
     * The style value is either one of the style constants defined in class SWT
     * which is applicable to instances of this class, or must be built by
     * bitwise OR'ing together (that is, using the int "|" operator) two or more
     * of those SWT style constants. The class description lists the style
     * constants that are applicable to the class. Style bits are also inherited
     * from superclasses.
     * 
     * @see org.eclipse.swt.widgets.List
     * @param parent
     *            a composite control which will be the parent of the new
     *            instance (cannot be null)
     * @param style
     *            the style of control to construct (SWT.MULTI will be added)
     */
    public ToggleSelectList(Composite parent, int style) {
        super(parent, SWT.NONE);
        GridLayout layout = new GridLayout(1, true);
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        this.setLayout(layout);

        style = (style | SWT.MULTI) & ~SWT.SINGLE;
        list = new List(this, style);
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        list.setLayoutData(layoutData);
        list.addSelectionListener(new SelectionListener() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                for (int i : list.getSelectionIndices()) {
                    if (selectionIndices.contains(i)) {
                        selectionIndices.remove(i);
                    } else {
                        selectionIndices.add(i);
                    }
                }
                int[] indices = new int[selectionIndices.size()];
                int i = 0;
                for (Integer index : selectionIndices) {
                    indices[i++] = index;
                }
                list.deselectAll();
                list.select(indices);

                for (Object listener : selectionListeners.getListeners()) {
                    ((SelectionListener) listener).widgetSelected(e);
                }
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                for (Object listener : selectionListeners.getListeners()) {
                    ((SelectionListener) listener).widgetDefaultSelected(e);
                }
            }

        });
    }

    protected void updateIndices() {
        selectionIndices.clear();
        for (int i : list.getSelectionIndices()) {
            selectionIndices.add(i);
        }
    }

    public void add(String string) {
        list.add(string);
    }

    public void add(String string, int index) {
        list.add(string, index);
    }

    public void addSelectionListener(SelectionListener listener) {
        selectionListeners.add(listener);
    }

    public void deselect(int index) {
        list.deselect(index);
        updateIndices();
    }

    public void deselect(int[] indices) {
        list.deselect(indices);
        updateIndices();
    }

    public void deselect(int start, int end) {
        list.deselect(start, end);
        updateIndices();
    }

    public void deselectAll() {
        list.deselectAll();
        updateIndices();
    }

    public String getItem(int index) {
        return list.getItem(index);
    }

    public int getItemCount() {
        return list.getItemCount();
    }

    public int getItemHeight() {
        return list.getItemHeight();
    }

    public String[] getItems() {
        return list.getItems();
    }

    /**
     * Retrieve the internal org.eclipse.swt.widgets.List <br>
     * <br>
     * Do not use this method unless you need access to a method not wrapped by
     * ToggleSelectList.
     * 
     * @return the internal List widget
     */
    public List getlist() {
        return list;
    }

    public String[] getSelection() {
        return list.getSelection();
    }

    public int getSelectionCount() {
        return list.getSelectionCount();
    }

    public int getSelectionIndex() {
        return list.getSelectionIndex();
    }

    public int[] getSelectionIndices() {
        return list.getSelectionIndices();
    }

    public int getTopIndex() {
        return list.getTopIndex();
    }

    public int indexOf(String string) {
        return list.indexOf(string);
    }

    public int indexOf(String string, int start) {
        return list.indexOf(string, start);
    }

    public boolean isSelected(int index) {
        return list.isSelected(index);
    }

    public void remove(int index) {
        list.remove(index);
        updateIndices();
    }

    public void remove(int[] indices) {
        list.remove(indices);
        updateIndices();
    }

    public void remove(int start, int end) {
        list.remove(start, end);
        updateIndices();
    }

    public void remove(String string) {
        list.remove(string);
        updateIndices();
    }

    public void removeAll() {
        list.removeAll();
        updateIndices();
    }

    public void removeSelectionListener(SelectionListener listener) {
        selectionListeners.remove(listener);
    }

    public void select(int index) {
        list.select(index);
        updateIndices();
    }

    public void select(int[] indices) {
        list.select(indices);
        updateIndices();
    }

    public void select(int start, int end) {
        list.select(start, end);
        updateIndices();
    }

    public void selectAll() {
        list.selectAll();
        updateIndices();
    }

    @Override
    public void setFont(Font font) {
        super.setFont(font);
        list.setFont(font);
    }

    public void setItem(int index, String string) {
        list.setItem(index, string);
    }

    public void setItems(String[] items) {
        list.setItems(items);
    }

    public void setSelection(int index) {
        list.setSelection(index);
        updateIndices();
    }

    public void setSelection(int[] indices) {
        list.setSelection(indices);
        updateIndices();
    }

    public void setSelection(int start, int end) {
        list.setSelection(start, end);
        updateIndices();
    }

    public void setSelection(String[] items) {
        list.setSelection(items);
        updateIndices();
    }

    public void setTopIndex(int index) {
        list.setTopIndex(index);
    }

    public void showSelection() {
        list.showSelection();
    }
}
