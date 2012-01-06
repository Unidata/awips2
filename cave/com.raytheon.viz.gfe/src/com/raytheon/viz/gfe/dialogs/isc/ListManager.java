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
package com.raytheon.viz.gfe.dialogs.isc;

import java.util.Arrays;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.List;

public class ListManager {
    private Composite parentComp;

    protected List dataList;

    private boolean[] indexArray = new boolean[0];

    private boolean singleSelect = false;

    public ListManager(Composite parentComp) {
        this.parentComp = parentComp;

        initialize();
    }

    public ListManager(Composite parentComp, boolean singleSelect) {
        this.parentComp = parentComp;
        this.singleSelect = true;

        initialize();
    }

    private void initialize() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 250;
        gd.minimumWidth = 170;
        gd.minimumHeight = 250;

        dataList = new List(parentComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        dataList.setLayoutData(gd);
        addListListener();
        
    }
    
    protected void addListListener(){
        dataList.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                updateListIndexes();
            }
        });
    }

    protected void updateListIndexes() {

        if (singleSelect) {
            Arrays.fill(indexArray, false);
        }

        if (indexArray[dataList.getSelectionIndex()] == false) {
            indexArray[dataList.getSelectionIndex()] = true;
        } else {
            indexArray[dataList.getSelectionIndex()] = false;
        }

        dataList.deselectAll();
        int indexes[] = new int[dataList.getItemCount()];
        Arrays.fill(indexes, -99);
        int counter = 0;

        for (int i = 0; i < indexArray.length; i++) {
            if (indexArray[i] == true) {
                indexes[counter] = i;
                ++counter;
            }
        }

        dataList.select(indexes);
    }

    public void addItemToList(String item) {
        dataList.add(item);
        dataList.select(dataList.getItemCount() - 1);

        boolean[] tmpIndexes = new boolean[dataList.getItemCount()];
        Arrays.fill(tmpIndexes, true);

        if (indexArray.length > 0) {
            System.arraycopy(indexArray, 0, tmpIndexes, 0, indexArray.length);

            indexArray = new boolean[tmpIndexes.length];
            System.arraycopy(tmpIndexes, 0, indexArray, 0, tmpIndexes.length);
        } else {
            indexArray = new boolean[tmpIndexes.length];
            System.arraycopy(tmpIndexes, 0, indexArray, 0, tmpIndexes.length);
        }
    }

    public void addItemToList(String[] items) {
        for (String item : items) {
            addItemToList(item);
        }
    }

    public void clearList() {
        dataList.removeAll();
        indexArray = new boolean[0];
    }

    public void deselectAll() {
        Arrays.fill(indexArray, false);
        dataList.deselectAll();
    }

    public void selectAll() {
        Arrays.fill(indexArray, true);
        dataList.selectAll();
    }

    public void setSelected(int index) {
        dataList.setSelection(index);
        updateListIndexes();
    }

    public void setSelected(int[] indexes) {
        for (int index : indexes) {
            setSelected(index);
        }
    }

    public void setSelected(String item) {
        int index = dataList.indexOf(item);
        if (index != -1) {
            setSelected(index);
        }
    }

    public void setSelected(String[] items) {
        for (String item : items) {
            setSelected(item);
        }
    }
    
    public java.util.List<String> getSelectedItems(){
        return Arrays.asList(dataList.getSelection());
    }
    
    public java.util.List<String> getItems(){
        return Arrays.asList(dataList.getItems());
    }
}
