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
package com.raytheon.uf.viz.gisdatastore.directory;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Label;
import org.geotools.data.DataStore;
import org.geotools.data.directory.DirectoryDataStoreFactory;

import com.raytheon.uf.viz.gisdatastore.IGisDataStorePlugin;
import com.raytheon.uf.viz.gisdatastore.directory.rsc.DirectoryDataStoreResourceData;
import com.raytheon.uf.viz.gisdatastore.rsc.DataStoreResourceData;

/**
 * Directory GisDataStorePlugin implementation
 * 
 * Will load data from any recognized GIS feature file types in a directory
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 6, 2012            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class DirectoryDataStorePlugin implements IGisDataStorePlugin {
    private static int DIR_LIST_SIZE = 10;

    private Map<String, Object> connectionParameters;

    private Combo dirCombo;

    private Button browseButton;

    private List<String> dirList;

    @Override
    public void createControls(final Composite comp) {
        GridLayout layout = (GridLayout) comp.getLayout();
        layout.numColumns = 2;
        layout.makeColumnsEqualWidth = false;

        Label label = new Label(comp, SWT.NONE);
        label.setText("Directory:");
        GridData layoutData = new GridData(SWT.FILL, SWT.CENTER, true, false);
        label.setLayoutData(layoutData);

        browseButton = new Button(comp, SWT.PUSH);
        layoutData = new GridData(SWT.END, SWT.CENTER, false, false);
        browseButton.setLayoutData(layoutData);
        browseButton.setText("Browse...");
        browseButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                DirectoryDialog dlg = new DirectoryDialog(comp.getShell());
                dlg.setFilterPath(dirCombo.getText());
                String dir = dlg.open();
                if (dir != null) {
                    dirCombo.setText(dir);
                }
            }
        });

        dirCombo = new Combo(comp, SWT.DROP_DOWN);
        layoutData = new GridData(SWT.FILL, SWT.CENTER, true, false);
        layoutData.widthHint = 300;
        layoutData.horizontalSpan = 2;
        dirCombo.setLayoutData(layoutData);
    }

    @Override
    public Map<String, Object> getConnectionParameters() {
        return this.connectionParameters;
    }

    @Override
    public void loadFromPreferences() {
        dirList = new ArrayList<String>(DIR_LIST_SIZE);
        IPreferenceStore prefs = Activator.getDefault().getPreferenceStore();
        for (int i = 0; i < DIR_LIST_SIZE; i++) {
            String url = prefs.getString("DirectoryImportPath" + i);
            dirList.add(url);
        }
        dirCombo.setItems(dirList.toArray(new String[dirList.size()]));
        dirCombo.setText(dirList.get(0));
    }

    @Override
    public void saveToPreferences() {
        IPersistentPreferenceStore prefs = Activator.getDefault()
                .getPreferenceStore();
        String dir = dirCombo.getText();
        if (dirList.contains(dir)) {
            // dir already in list remove it
            dirList.remove(dir);
        } else if (dirList.size() == DIR_LIST_SIZE) {
            // list at max size remove last entry
            dirList.remove(DIR_LIST_SIZE - 1);
        }
        // put dir at front of list
        dirList.add(0, dir);

        int i = 0;
        for (String s : dirList) {
            prefs.setValue("DirectoryImportPath" + i++, s);
        }

        try {
            prefs.save();
        } catch (IOException e) {
            Activator.statusHandler.error(
                    "Unable to save recently used directories", e);
        }
    }

    @Override
    public DataStore connectToDataStore() throws IOException {
        String dir = dirCombo.getText();
        URL url = new File(dir).toURI().toURL();

        Map<String, Object> params = new HashMap<String, Object>();
        params.put(DirectoryDataStoreFactory.URLP.key, url);

        DataStoreResourceData rd = constructResourceData(null, params);
        DataStore dataStore = rd.getDataStore();

        this.connectionParameters = params;

        return dataStore;
    }

    @Override
    public DataStoreResourceData constructResourceData(String typeName,
            Map<String, Object> connectionParameters) {
        DirectoryDataStoreResourceData rd = new DirectoryDataStoreResourceData(
                typeName, connectionParameters);
        return rd;
    }
}
