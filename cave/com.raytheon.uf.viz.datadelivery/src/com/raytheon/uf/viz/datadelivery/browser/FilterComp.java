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
package com.raytheon.uf.viz.datadelivery.browser;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.datadelivery.filter.AbstractFilterComp;
import com.raytheon.uf.viz.datadelivery.filter.FilterImages.ExpandItemState;
import com.raytheon.uf.viz.datadelivery.filter.IFilterUpdate;
import com.raytheon.viz.ui.widgets.duallist.DualList;
import com.raytheon.viz.ui.widgets.duallist.DualListConfig;
import com.raytheon.viz.ui.widgets.duallist.IUpdate;

/**
 * Standard Filter Composite
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 21, 2012            mpduff      Initial creation
 * Aug 08, 2012    863     jpiatt      Added new interface method.
 * Jan 07, 2013   1432     mpduff      Fix case sensitive and exclude checkboxes.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class FilterComp extends AbstractFilterComp implements IUpdate {

    /** Search text field */
    private Text regExTxt;

    /** Match any button */
    private Button matchAnyBtn;

    /** Match all button */
    private Button matchAllBtn;

    /** Case sensitive check */
    private Button caseBtn;

    /** Exclusion check */
    private Button exclusionBtn;

    /** Match any flag */
    private boolean matchAnyFlag = true;

    /** Dual list widget */
    private DualList dualList;

    /** Dual list configuration */
    private final DualListConfig dualConfig;

    /** Filter Configuration */
    private final FilterConfig config;

    /** Flag */
    private boolean dirty = false;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent Control
     * @param style
     *            Control style
     * @param callback
     *            Callback object
     * @param config
     *            Control config object
     * @param idx
     *            Control index
     */
    public FilterComp(Composite parent, int style, IFilterUpdate callback,
            FilterConfig config, int idx) {
        super(parent, callback, idx);
        this.config = config;
        this.dualConfig = config.getDualConfig();

        init();
    }

    /**
     * Initialize the control
     */
    private void init() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, true);
        gl.verticalSpacing = 2;
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        this.setLayout(gl);
        this.setLayoutData(gd);

        if (config.isRegExVisible()) {
            createRegEx();
            if (config.isDualListVisible()) {
                addSeparator(this);
            }
        }

        if (config.isMatchControlVisible()) {
            createMatchControls();
        }

        if (config.isDualListVisible()) {
            createDualList();
        }
    }

    /**
     * Create the search widget
     */
    private void createRegEx() {
        Composite controlComp = new Composite(this, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        controlComp.setLayout(gl);
        controlComp.setLayoutData(gd);

        Label regExLbl = new Label(controlComp, SWT.NONE);
        regExLbl.setText("Search: ");

        regExTxt = new Text(controlComp, SWT.BORDER);
        gd = new GridData(225, SWT.DEFAULT);
        regExTxt.setLayoutData(gd);
        regExTxt.setToolTipText("Enter text for search");
        regExTxt.addFocusListener(new FocusAdapter() {
            @Override
            public void focusLost(FocusEvent e) {
                // handleFocusLost(e);
            }
        });
        regExTxt.addKeyListener(new KeyListener() {
            @Override
            public void keyReleased(KeyEvent e) {
                handleSearch();
            }

            @Override
            public void keyPressed(KeyEvent e) {
                // Not Called

            }
        });

        caseBtn = new Button(controlComp, SWT.CHECK);
        caseBtn.setText("Case Sensitive");
        caseBtn.setToolTipText("Match upper and lower case");
        caseBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                dualConfig.setCaseFlag(caseBtn.getSelection());
                handleSearch();
            }
        });

        exclusionBtn = new Button(controlComp, SWT.CHECK);
        exclusionBtn.setText("Exclude");
        exclusionBtn.setToolTipText("Does not contain search text");
        exclusionBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                dualConfig.setExcludeFlag(exclusionBtn.getSelection());
                handleSearch();
            }
        });

    }

    /**
     * Create the match radio buttons
     */
    private void createMatchControls() {
        Composite matchComp = new Composite(this, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 10;
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        matchComp.setLayout(gl);
        matchComp.setLayoutData(gd);

        matchAnyBtn = new Button(matchComp, SWT.RADIO);
        matchAnyBtn.setText("Match Any");
        matchAnyBtn.setSelection(true);
        matchAnyBtn.setToolTipText("Results match any in selected list");
        matchAnyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (matchAnyBtn.getSelection() == true) {
                    matchAnyFlag = true;
                }
            }
        });

        matchAllBtn = new Button(matchComp, SWT.RADIO);
        matchAllBtn.setText("Match All");
        matchAllBtn.setToolTipText("Results match all selected list");
        matchAllBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (matchAllBtn.getSelection() == true) {
                    matchAnyFlag = false;
                }
            }
        });
    }

    /**
     * Create the Dual List widget
     */
    private void createDualList() {
        dualList = new DualList(this, SWT.NONE, dualConfig, this);
    }

    /**
     * Add a separator
     * 
     * @param parentComp
     *            Composite that gets the separator
     */
    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout) parentComp.getLayout();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Reset the filter controls.
     */
    @Override
    public void resetControls() {
        if (regExTxt != null) {
            regExTxt.setText("");
        }

        dualList.clearSelection();
        setCurrentState(ExpandItemState.NoEntries);
    }

    /**
     * Handle the search action.
     */
    private void handleSearch() {
        boolean excludeSearch = !exclusionBtn.getSelection();

        String search = regExTxt.getText();
        ArrayList<String> tmpFilterList = new ArrayList<String>();
        if (search != null && search.length() > 0) {

            dualConfig.setSearchField(search);

            String[] parts;

            /* Iterate over the filtered list of items */
            String[] filteredList = dualConfig.getFullList().toArray(
                    new String[dualConfig.getFullList().size()]);

            // Search contains 1 or more *
            if (search.contains("*")) {
                parts = search.split("\\*");
                if (parts.length > 0) {
                    ITEM: for (String item : filteredList) {
                        for (String part : parts) {
                            if (part.length() > 0) {
                                if (caseBtn.getSelection()) {
                                    if (item.contains(part) == excludeSearch) {
                                        continue ITEM;
                                    }
                                } else {
                                    if (!item.toLowerCase().contains(
                                            part.toLowerCase()) == excludeSearch) {
                                        continue ITEM;
                                    }
                                }
                            }
                        }

                        // all parts are contained in the item, now figure
                        // out if they are in the right order
                        int idx = item.indexOf(parts[0]);
                        for (int i = 1; i < parts.length; i++) {
                            int curIdx = 0;
                            if (caseBtn.getSelection()) {
                                curIdx = item.indexOf(parts[i]);
                            } else {
                                curIdx = item.toLowerCase().indexOf(
                                        parts[i].toLowerCase());
                            }

                            if (curIdx > idx) {
                                idx = curIdx;
                            } else {
                                break ITEM;
                            }
                        }

                        // Made it this far so item is in list
                        tmpFilterList.add(item);
                    }
                    dualList.clearAvailableList(false);
                    dualList.setAvailableItems(tmpFilterList);
                }
                return;
            } else {
                // No * in search
                for (String item : filteredList) {
                    if (caseBtn.getSelection()) {
                        if (item.contains(search) == excludeSearch) {
                            tmpFilterList.add(item);
                        }
                    } else {
                        if (item.toLowerCase().contains(search.toLowerCase()) == excludeSearch) {
                            tmpFilterList.add(item);
                        }
                    }
                }
            }

            // Clear the list and add the newly filtered items
            dualList.clearAvailableList(false);
            dualList.setAvailableItems(tmpFilterList);
            return;
        }

        // Clear the list and repopulate with the full list
        dualList.clearAvailableList(false);
        dualList.setAvailableItems(dualConfig.getFullList());
    }

    /**
     * Set the selectedItems into the selected list.
     * 
     * @param selectedItems
     */
    public void setSelectedItems(String[] selectedItems) {
        dualList.setSelectedItems(selectedItems);
    }

    /**
     * Select the items passed in. This moves the items from the available list
     * into the selected list.
     * 
     * @param items
     */
    public void selectItems(String[] items) {
        dualList.selectItems(items);
    }

    /**
     * Get the rows in the Selected List.
     * 
     * @return array list of selected items
     */
    public String[] getSelectedListItems() {
        return dualList.getSelectedListItems();
    }

    /**
     * Get the Filter Configuration.
     * 
     * @return FilterConfig object
     */
    public FilterConfig getConfig() {
        return config;
    }

    /**
     * Get flag for matching.
     * 
     * @return boolean true if match any entry
     */
    public boolean matchAnyEntry() {
        return matchAnyFlag;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.datadelivery.common.ui.IUpdate#hasEntries(boolean)
     */
    @Override
    public void hasEntries(boolean entries) {
        if (entries) {
            setCurrentState(ExpandItemState.Entries);
        } else {
            setCurrentState(ExpandItemState.NoEntries);
        }
        this.dirty = true;
    }

    /**
     * Flag for dirty.
     * 
     * @return the dirty
     */
    public boolean isDirty() {
        return dirty;
    }

    /**
     * If any mods have been made to the composite selections, set dirty true.
     * 
     * @param dirty
     *            the dirty to set
     */
    public void setDirty(boolean dirty) {
        this.dirty = dirty;
    }

    @Override
    public void selectionChanged() {
        // unused

    }
}
