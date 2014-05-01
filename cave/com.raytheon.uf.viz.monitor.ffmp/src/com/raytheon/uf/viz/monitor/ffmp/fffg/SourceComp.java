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
package com.raytheon.uf.viz.monitor.ffmp.fffg;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.monitor.config.FFFGConfig.GuidSectType;
import com.raytheon.uf.common.monitor.config.FFFGConfig.UpdateType;
import com.raytheon.uf.common.monitor.config.SourceCompData;
import com.raytheon.uf.common.monitor.config.ValueNameIdData;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 15, 2010 #4517      lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class SourceComp extends Composite {
    /**
     * Display object.
     */
    private Display display;

    /**
     * Source combo control.
     */
    private Combo sourceCbo;

    /**
     * Duration list control.
     */
    private List durationList;

    /**
     * Remove source button - removes this source from the display.
     */
    private Button removeSourceBtn;

    /**
     * Select source button - selects this source on the display.
     */
    private Button selectBtn;

    /**
     * Add new source button - adds a new source to the display.
     */
    private Button addSourceBtn;

    /**
     * Text control displaying the Area FFG value.
     */
    private Text areaFFGValueTF;

    /**
     * County list control.
     */
    private List countyList;

    /**
     * Button to remove a county from the list control.
     */
    private Button removeCountyBtn;

    /**
     * Basin list control.
     */
    private List basinList;

    /**
     * Button to remove a basin from the list control.
     */
    private Button removeBasinBtn;

    /**
     * Flag indicating if this is the first source on the display.
     */
    private boolean firstSource = false;

    /**
     * List control width.
     */
    private int listWidth = 170;

    /**
     * List control height.
     */
    private int listHeight = 125;

    /**
     * Main control composite.
     */
    private Composite mainControlComp;

    /**
     * Callback called when a remove, select, or add button is clicked.
     */
    private ISourceCompAction actionCB;

    /**
     * Add/Remove font.
     */
    private Font addRemoveFont;

    /**
     * List control font.
     */
    private Font listFont;

    /**
     * Margin width.
     */
    private final int marginWidth = 15;

    /**
     * County data map.
     */
    private HashMap<Long, ValueNameIdData> countyDataMap;

    /**
     * County data array.
     */
    private ArrayList<ValueNameIdData> countyDataArray;

    /**
     * Basin data map.
     */
    private HashMap<Long, ValueNameIdData> basinDataMap;

    /**
     * Basin data array.
     */
    private ArrayList<ValueNameIdData> basinDataArray;

    /**
     * Area FFG Value.
     */
    private String areaFFGValue = null;

    /**
     * Margin value for the composite.
     */
    private final int marginVal = 3;

    /**
     * Font for the filler label. This will ensure correct spacing.
     */
    private Font fillerLabelFont;

    /**
     * A map of source names (key) and the display and duration data (value).
     * Example: Key: FFG0124hr Value: RFCFFG, 1 (contained in
     * SrcDisplayDurationData)
     */
    private LinkedHashMap<String, SrcDisplayDurationData> sourceDisplayMap;

    /**
     * Array of source names.
     */
    private ArrayList<String> sourceNamesArray;

    private SourceCompData sourceCompData = null;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param firstSource
     *            Flag indicating if this is the first source comp.
     * @param actionCB
     *            Callback.
     * @param fillerLabelFont
     *            Font for the filler label.
     */
    public SourceComp(Composite parent, boolean firstSource,
            ISourceCompAction actionCB, Font fillerLabelFont) {
        super(parent, 0);

        this.firstSource = firstSource;
        this.display = parent.getDisplay();
        this.actionCB = actionCB;
        this.fillerLabelFont = fillerLabelFont;

        initData();
        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, false, true);
        GridLayout gl = new GridLayout(1, false);
        gl.marginWidth = 0;
        gl.marginHeight = marginVal;
        gl.marginRight = marginVal;
        gl.marginLeft = marginVal;
        this.setLayout(gl);
        this.setLayoutData(gd);

        gl = new GridLayout(1, false);
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        mainControlComp = new Composite(this, SWT.NONE);
        mainControlComp.setLayout(gl);
        mainControlComp.setLayoutData(gd);

        createSourceControls();
        createAreaFFG();
        createCountyList();
        createBasinList();

        this.addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent arg0) {
                addRemoveFont.dispose();
                listFont.dispose();
            }
        });
    }

    /**
     * Initialize data method.
     */
    private void initData() {
        addRemoveFont = new Font(display, "Sans", 10, SWT.BOLD);
        listFont = new Font(display, "Monospace", 9, SWT.NORMAL);

        countyDataMap = new HashMap<Long, ValueNameIdData>();
        countyDataArray = new ArrayList<ValueNameIdData>();

        basinDataMap = new HashMap<Long, ValueNameIdData>();
        basinDataArray = new ArrayList<ValueNameIdData>();
    }

    /**
     * Create the source combo and list controls.
     */
    private void createSourceControls() {
        /*
         * Source controls
         */
        GridData gd = new GridData(SWT.DEFAULT, 170);
        Composite sourceComp = new Composite(mainControlComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.marginWidth = marginWidth;
        sourceComp.setLayout(gl);
        sourceComp.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.CENTER, true, true);
        Label sourceLbl = new Label(sourceComp, SWT.NONE);
        sourceLbl.setText("Source: ");
        sourceLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        sourceCbo = new Combo(sourceComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        sourceCbo.setLayoutData(gd);
        sourceCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                Combo combo = (Combo) event.getSource();
                loadDurationList(combo.getText());
                actionCB.setModified(true);
            }
        });

        Label durationLbl = new Label(sourceComp, SWT.NONE);
        durationLbl.setText("Duration:\n(hour)");

        gd = new GridData(50, 80);
        durationList = new List(sourceComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        durationList.setLayoutData(gd);
        durationList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                actionCB.setModified(true);
            }
        });

        /*
         * Select and add/remove controls
         */
        Composite selectCtrlComp = new Composite(sourceComp, SWT.NONE);
        gl = new GridLayout(3, true);
        gd = new GridData();
        gd.horizontalSpan = 2;
        selectCtrlComp.setLayout(gl);
        selectCtrlComp.setLayoutData(gd);

        if (firstSource == false) {
            gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
            gd.widthHint = 35;
            removeSourceBtn = new Button(selectCtrlComp, SWT.PUSH);
            removeSourceBtn.setText("-");
            removeSourceBtn.setFont(addRemoveFont);
            removeSourceBtn.setLayoutData(gd);
            removeSourceBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    removeSource();
                }
            });
        } else {
            new Label(selectCtrlComp, SWT.NONE);
        }

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        selectBtn = new Button(selectCtrlComp, SWT.PUSH);
        selectBtn.setText("Select");
        selectBtn.setLayoutData(gd);
        selectBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                selectSource();
            }
        });

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gd.widthHint = 35;
        addSourceBtn = new Button(selectCtrlComp, SWT.PUSH);
        addSourceBtn.setText("+");
        addSourceBtn.setFont(addRemoveFont);
        addSourceBtn.setLayoutData(gd);
        addSourceBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                addNewSource();
            }
        });
    }

    /**
     * Create the Area FFG control.
     */
    private void createAreaFFG() {
        /*
         * Create grey filler label.
         */
        createFillerColorLabel(display.getSystemColor(SWT.COLOR_DARK_GRAY));

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = 40;
        Composite areaFFGComp = new Composite(mainControlComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.marginWidth = marginWidth;
        areaFFGComp.setLayout(gl);
        areaFFGComp.setLayoutData(gd);
        areaFFGComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        areaFFGValueTF = new Text(areaFFGComp, SWT.BORDER);
        areaFFGValueTF.setEditable(false);
        areaFFGValueTF.setLayoutData(gd);
    }

    /**
     * Create the county list control.
     */
    private void createCountyList() {
        /*
         * Create grey filler label.
         */
        createFillerColorLabel(display.getSystemColor(SWT.COLOR_DARK_GRAY));

        /*
         * County list
         */
        Composite countyComp = new Composite(mainControlComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginWidth = marginWidth;
        countyComp.setLayout(gl);

        // Empty Filler label
        Button fillerRdo = new Button(countyComp, SWT.RADIO);
        fillerRdo.setVisible(false);

        GridData gd = new GridData(listWidth, listHeight);
        countyList = new List(countyComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        countyList.setLayoutData(gd);
        countyList.setFont(listFont);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        removeCountyBtn = new Button(countyComp, SWT.PUSH);
        removeCountyBtn.setText(" Remove County(ies) ");
        removeCountyBtn.setLayoutData(gd);
        removeCountyBtn.setEnabled(false);
        removeCountyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                removeSelectedListItems(countyList, countyDataArray,
                        countyDataMap);
                actionCB.setModified(true);
            }
        });

        addListSelectionListener(countyList, removeCountyBtn);

        // Empty Filler label
        Button fillerChk = new Button(countyComp, SWT.CHECK);
        fillerChk.setText("sdf");
        fillerChk.setVisible(false);

        createFillerColorLabel(display.getSystemColor(SWT.COLOR_GREEN));
    }

    /**
     * Create the basin list control.
     */
    private void createBasinList() {
        /*
         * Create grey filler label.
         */
        createFillerColorLabel(display.getSystemColor(SWT.COLOR_DARK_GRAY));

        /*
         * County list
         */
        Composite basinComp = new Composite(mainControlComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginWidth = marginWidth;
        basinComp.setLayout(gl);

        // Empty Filler label
        Button fillerRdo = new Button(basinComp, SWT.RADIO);
        fillerRdo.setVisible(false);

        GridData gd = new GridData(listWidth, listHeight);
        basinList = new List(basinComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        basinList.setLayoutData(gd);
        basinList.setFont(listFont);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        removeBasinBtn = new Button(basinComp, SWT.PUSH);
        removeBasinBtn.setText(" Remove Basin(s) ");
        removeBasinBtn.setLayoutData(gd);
        removeBasinBtn.setEnabled(false);
        removeBasinBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                removeSelectedListItems(basinList, basinDataArray, basinDataMap);
                actionCB.setModified(true);
            }
        });

        addListSelectionListener(basinList, removeBasinBtn);
    }

    /**
     * Create a filler label with a background color set to the color passed in.
     * 
     * @param bgColor
     *            Background color.
     */
    private void createFillerColorLabel(Color bgColor) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label sepLbl = new Label(mainControlComp, SWT.NONE);
        sepLbl.setBackground(bgColor);
        sepLbl.setText(" ");
        sepLbl.setFont(fillerLabelFont);
        sepLbl.setLayoutData(gd);
    }

    private void loadDurationList(String displayName) {
        durationList.removeAll();
        Set<String> srcNames = sourceDisplayMap.keySet();
        for (String name : srcNames) {
            if (sourceDisplayMap.get(name).getDisplayName().equals(displayName)) {
                durationList.add(sourceDisplayMap.get(name)
                        .getDurationHrsString());
            }
        }

        durationList.select(0);
    }

    /**
     * Populate the source combo list with the available sources.
     */
    private void populateSourceComboDurationList() {
        /*
         * Since the source name is the unique identifier it will be the key in the
         * the source display map. The value in the map is SrcDisplayDurationData which
         * contains the display name for the source combo box and the duration hour for
         * the duration hour list control.
         *
         * Example:
         *
         * Source Name: Display Name: Duration Hour:
         * --------------------------------------------------------------
         * FFG0124hr RFCFFG 1
         * FFG0324hr RFCFFG 3
         * FFG0624hr RFCFFG 6
         *
         * To figure out the source name you must use both the selection in the
         * source combo control and the duration list control.
         */

        Set<String> keys = sourceDisplayMap.keySet();

        sourceNamesArray = new ArrayList<String>();
        sourceNamesArray.addAll(keys);

        /*
         * Populate the source combo and duration list control.
         */
        for (String sourceName : sourceNamesArray) {
            // If the source combo doesn't contain the display name for the
            // specified source then add it.
            if (sourceCbo.indexOf(sourceDisplayMap.get(sourceName)
                    .getDisplayName()) < 0) {
                sourceCbo
                        .add(sourceDisplayMap.get(sourceName).getDisplayName());
            }
        }

        String selectedSource = null;
        if (this.sourceCompData == null) {
            // Select the first item in the source combo control.
            if (sourceCbo.getItemCount() > 0) {
                sourceCbo.select(0);
            }
        } else {
            for (String sourceName: sourceDisplayMap.keySet()) {
                if (sourceCompData.getSourceName().equals(sourceName)) {
                    selectedSource = sourceName;
                    String dispName = sourceDisplayMap.get(sourceName).getDisplayName();
                    String[] sources = sourceCbo.getItems();
                    for (int i = 0; i < sources.length; i++) {
                        if (sources[i].equals(dispName)) {
                            sourceCbo.select(i);
                            break;
                        }
                    }
                }
            }
        }
        
        loadDurationList(sourceCbo.getItem(sourceCbo.getSelectionIndex()));
        
        if (sourceDisplayMap.containsKey(selectedSource)) {
            double duration = sourceDisplayMap.get(selectedSource).getDurationHrs();
            for (int i = 0; i < durationList.getItems().length; i++) {
                if (Double.parseDouble(durationList.getItem(i)) == duration) {
                    durationList.select(i);
                    break;
                }
            }
        }
    }

    /**
     * Have this source removed from the display.
     */
    private void removeSource() {
        actionCB.removeSource(this);
        actionCB.setModified(true);
    }

    /**
     * Select this source as the main (selected) source.
     */
    private void selectSource() {
        actionCB.selectSource(this);
    }

    /**
     * Add a new source column.
     */
    private void addNewSource() {
        actionCB.addNewSource(null);
        actionCB.setModified(true);

    }

    /**
     * Add a selection listener to the List control passed in. When an item or
     * items are selected the remove button is enabled. If no items are selected
     * the remove button is disabled.
     * 
     * @param listControl
     *            List control.
     * @param removeBtn
     *            Remove button.
     */
    private void addListSelectionListener(final List listControl,
            final Button removeBtn) {
        listControl.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (listControl.getSelectionCount() == 0) {
                    removeBtn.setEnabled(false);
                    return;
                }

                removeBtn.setEnabled(true);
            }
        });
    }

    /**
     * Remove the selected items from the list control. This will also remove
     * the items from the data map and data array.
     * 
     * @param listControl
     *            List control.
     * @param array
     *            Data array.
     * @param map
     *            Data map.
     */
    private void removeSelectedListItems(List listControl,
            ArrayList<ValueNameIdData> array, HashMap<Long, ValueNameIdData> map) {
        int[] indexes = listControl.getSelectionIndices();

        if (indexes.length == 0) {
            return;
        }

        String msg;
        if (indexes.length > 1) {
            msg = "Removed selected items successfully";
        } else {
            String selection = listControl.getItem(listControl
                    .getSelectionIndex());
            msg = "Removed \"" + selection + "\" sucessfully";
        }

        // Remove the items from the data array and the data map
        for (int i = indexes.length - 1; i >= 0; --i) {
            // Remove the entry from the map
            map.remove(array.get(indexes[i]).getId());

            // Remove the entry from the array list
            array.remove(indexes[i]);
        }

        // Remove the entries from the list control
        listControl.remove(indexes);

        actionCB.setStatusMsg(0, msg);
    }

    /**
     * Add/Update data in the list control, data array, and data map. This can
     * be done for either basin or county.
     * 
     * @param newArray
     *            New array of data to be added.
     * @param listControl
     *            List control the data will be added to or updated.
     * @param array
     *            Data array the data will be added to or updated.
     * @param map
     *            Data map the data will be added to or updated.
     */
    private void addUpdateData(ArrayList<ValueNameIdData> newArray,
            List listControl, ArrayList<ValueNameIdData> array,
            HashMap<Long, ValueNameIdData> map) {
        ValueNameIdData valNameIdData;

        for (ValueNameIdData newData : newArray) {
            if (map.containsKey(newData.getId()) == true) {
                valNameIdData = map.get(newData.getId());
                int idx = array.indexOf(valNameIdData);

                map.put(newData.getId(), newData);
                array.set(idx, newData);

                listControl.setItem(idx, newData.getDisplayString());
            } else {
                map.put(newData.getId(), newData);
                array.add(newData);
                listControl.add(newData.getDisplayString());
            }
        }
    }

    /**
     * Add data in the list control, data array, and data map. This can be done
     * for either basin or county.
     * 
     * @param newArray
     *            New array of data to be added.
     * @param listControl
     *            List control the data will be added to.
     * @param array
     *            Data array the data will be added to.
     * @param map
     *            Data map the data will be added to.
     */
    private void addData(ArrayList<ValueNameIdData> newArray, List listControl,
            ArrayList<ValueNameIdData> array, HashMap<Long, ValueNameIdData> map) {
        for (ValueNameIdData newData : array) {
            if (map.containsKey(newData.getId()) == false) {
                map.put(newData.getId(), newData);
                array.add(newData);
                listControl.add(newData.getDisplayString());
            }
        }
    }

    /**
     * Select or Unselect this source. Selecting this source will put a blue
     * outline around the entrie source.
     * 
     * @param flag
     *            True to select, false to unselect.
     */
    public void setSelectedBackground(boolean flag) {
        if (flag == true) {
            ((GridLayout) this.getLayout()).marginRight = marginVal;
            ((GridLayout) this.getLayout()).marginLeft = marginVal;
            this.setBackground(display.getSystemColor(SWT.COLOR_BLUE));
            this.pack();
        } else {
            ((GridLayout) this.getLayout()).marginRight = 0;
            ((GridLayout) this.getLayout()).marginLeft = 0;
            this.setBackground(display
                    .getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));
            this.pack();
        }
    }

    /**
     * Enable/Disable the add button.
     * 
     * @param flag
     *            True to enable, false to disable.
     */
    public void enableAddButton(boolean flag) {
        addSourceBtn.setEnabled(flag);
    }

    /**
     * Check if the source contains an ID for the specified type.
     * 
     * @param id
     *            ID name.
     * @param type
     *            County or Basin.
     * @return True if the ID exists, false otherwise.
     */
    public boolean containsID(Long id, GuidSectType type) {
        if (type == GuidSectType.COUNTY) {
            return countyDataMap.containsKey(id);
        } else if (type == GuidSectType.BASIN) {
            return basinDataMap.containsKey(id);
        }
        return false;
    }

    /**
     * Set the data in the specified list.
     * 
     * @param array
     *            Array of data.
     * @param updateType
     *            Update existing values or no update.
     * @param type
     *            County or Basin.
     */
    public void addUpdateNewData(ArrayList<ValueNameIdData> newArray,
            UpdateType updateType, GuidSectType type) {
        if (updateType == UpdateType.UPDATE) {
            if (type == GuidSectType.COUNTY) {
                addUpdateData(newArray, countyList, countyDataArray,
                        countyDataMap);
            } else if (type == GuidSectType.BASIN) {
                addUpdateData(newArray, basinList, basinDataArray, basinDataMap);
            }
        } else if (updateType == UpdateType.NO_UPDATE) {
            if (type == GuidSectType.COUNTY) {
                addData(newArray, countyList, countyDataArray, countyDataMap);
            } else if (type == GuidSectType.BASIN) {
                addData(newArray, basinList, basinDataArray, basinDataMap);
            }
        }
    }

    /**
     * Add all of the data from the array to the proper controls. The data array
     * passed in contains both county and basin data. This method is intended to
     * be used when loading in a new file.
     * 
     * @param newArray
     *            Array of new data.
     */
    public void addCountyBasinData(ArrayList<ValueNameIdData> newArray) {
        for (ValueNameIdData newData : newArray) {
            if (newData.getType() == GuidSectType.COUNTY) {
                countyDataMap.put(newData.getId(), newData);
                countyDataArray.add(newData);
                countyList.add(newData.getDisplayString());
            } else if (newData.getType() == GuidSectType.BASIN) {
                basinDataMap.put(newData.getId(), newData);
                basinDataArray.add(newData);
                basinList.add(newData.getDisplayString());
            }
        }
    }

    /**
     * Set the Area FFG value. If the value is null or empty then nothing is
     * done.
     * 
     * @param value
     *            The area FFG value.
     */
    public void setAreaFFG(String value) {
        if (value == null) {
            return;
        }

        if (value.equalsIgnoreCase("clear")) {
            areaFFGValue = value;
            areaFFGValueTF.setText("");
            areaFFGValue = null;
            return;
        }

        if ((areaFFGValue != null) && (areaFFGValue.length() != 0)) {
            MessageBox mb = new MessageBox(getParent().getShell(), SWT.YES
                    | SWT.NO);
            mb.setText("areaFFG");
            mb.setMessage("An areaFFG has already been assigned.  Do "
                    + "you wish to update?");
            int rv = mb.open();

            if (rv == SWT.NO) {
                return;
            }
        }

        /*
         * If the areaFFG value has something in it then we can set the label.
         */
        if (value.length() != 0) {
            areaFFGValue = value;
            areaFFGValueTF.setText(areaFFGValue + ";areaFFG");
        }
    }

    /**
     * Clear all of the data in the maps, arrays, and controls.
     */
    public void clearAllData() {
        countyDataMap.clear();
        countyDataArray.clear();
        countyList.removeAll();

        basinDataMap.clear();
        basinDataArray.clear();
        basinList.removeAll();

        areaFFGValue = "";
        areaFFGValueTF.setText("");
    }

    /**
     * Get the source component data for the this source component.
     * 
     * @return The source component data class.
     */
    public SourceCompData getSourceData() {
        String sourceName = getSourceName();

        SourceCompData scd = new SourceCompData();

        scd.setSourceName(sourceName);
        scd.setAreaFFGValue(areaFFGValue);

        ArrayList<ValueNameIdData> allCntyBasinData = new ArrayList<ValueNameIdData>();

        if (countyDataArray != null) {
            allCntyBasinData.addAll(countyDataArray);
        }

        if (basinDataArray != null) {
            allCntyBasinData.addAll(basinDataArray);
        }

        scd.setCountyBasinData(allCntyBasinData);
        this.sourceCompData = scd;

        return sourceCompData;
    }

    /**
     * Check to see if the source name is valid.
     * 
     * @param name
     *            Source name.
     * @return True if the source name is valid.
     */
    public boolean isValidSourceName(String name) {
        return sourceDisplayMap.containsKey(name);
    }

    /**
     * If the guidances map contains the source name then grab the data from the
     * guidances map and select the correct entries in the source combo box and
     * the duration hour list box.
     */
    public void setSourceName(String srcName) {
        if (sourceDisplayMap.containsKey(srcName)) {
            SrcDisplayDurationData sddd = sourceDisplayMap.get(srcName);
            sourceCbo.select(sourceCbo.indexOf(sddd.getDisplayName()));
            durationList.select(durationList.indexOf(sddd
                    .getDurationHrsString()));
        }
    }

    /**
     * Get the selected source name from the combo control and the selected
     * duration hour from the list control and then find the source name that
     * contains both of those values.
     * 
     * @return The unique source name.
     */
    public String getSourceName() {
        /*
         * Get the selected source name from the combo control and the selected
         * duration hour from the list control and then find the source name
         * that contains both of those values.
         */
        String selectedSourceCboName = sourceCbo.getItem(sourceCbo
                .getSelectionIndex());
        String selectedDuration = durationList.getItem(durationList
                .getSelectionIndex());

        String sourceName = "";
        SrcDisplayDurationData sddd;

        for (String srcName : sourceNamesArray) {
            sddd = sourceDisplayMap.get(srcName);

            if (sddd.dataMatch(selectedSourceCboName, selectedDuration)) {
                sourceName = srcName;
                break;
            }
        }

        return sourceName;
    }

    /**
     * Check if there is data in the areaFFG, county list, or basin list.
     * 
     * @return True if there is data available, false if not data is available.
     */
    public boolean hasData() {
        if (areaFFGValueTF.getText().trim().length() > 0) {
            return true;
        }

        if (countyList.getItemCount() > 0) {
            return true;
        }

        if (basinList.getItemCount() > 0) {
            return true;
        }

        return false;
    }

    public void setGuidanceData(
            LinkedHashMap<String, SrcDisplayDurationData> sourceDisplayMap) {
        this.sourceDisplayMap = sourceDisplayMap;
    }

    public void setSourceCompData(SourceCompData sourceCompData) {
        this.sourceCompData = sourceCompData;
    }
    
    public void populateSourceComp() {
        populateSourceComboDurationList();
    }
}
