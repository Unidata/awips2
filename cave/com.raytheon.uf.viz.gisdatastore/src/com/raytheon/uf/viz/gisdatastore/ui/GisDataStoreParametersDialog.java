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
package com.raytheon.uf.viz.gisdatastore.ui;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.geotools.data.DataStore;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.gisdatastore.Activator;
import com.raytheon.uf.viz.gisdatastore.IGisDataStorePlugin;
import com.raytheon.uf.viz.gisdatastore.rsc.DataStoreResource;
import com.raytheon.uf.viz.gisdatastore.rsc.DataStoreResourceData;
import com.raytheon.viz.core.ColorUtil;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.widgets.TimeRangeEntry;

/**
 * Generic GIS Connection Parameters Dialog
 * 
 * Each GisDataStore implementation will need to provide an implementation of
 * IParametersComp containing the necessary widgets to enter its specific
 * connection parameters
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 5, 2012            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GisDataStoreParametersDialog extends CaveJFACEDialog {
    private static final String DATE_TIME_FORMAT = "yyyy-MM-dd HH:mm:ss'Z'";

    private static final String GIS_DATA_STORE_PLUGIN_PREF = "GisDataStorePlugin";

    private static final String EXTENSION_POINT_ID = "com.raytheon.uf.viz.gisdatastore.gisDataStore";

    private IDescriptor descriptor;

    private DataTime currentFrame;

    private boolean loadAsProduct;

    private Map<String, IGisDataStorePlugin> plugins;

    private String[] pluginNames;

    private String typeName;

    private IGisDataStorePlugin gdsPlugin;

    private Composite mainComp;

    private TimeRangeEntry timeRangeEntry;

    private Group connectionGroup;

    private Button connectButton;

    private Button disconnectButton;

    private List tablesList;

    private Button okButton;

    private DataStore dataStore;

    private String selectedPlugin;

    public GisDataStoreParametersDialog(Shell parentShell,
            IDescriptor descriptor) {
        super(parentShell);
        this.descriptor = descriptor;
        FramesInfo framesInfo = descriptor.getFramesInfo();

        currentFrame = framesInfo.getCurrentFrame();
        if (currentFrame == null) {
            currentFrame = new DataTime(SimulatedTime.getSystemTime().getTime());
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setText("GIS DataStore Parameters");
    }

    /*
     * (non-Javadoc) Method declared on Dialog.
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        mainComp = (Composite) super.createDialogArea(parent);
        Group dsTypeGroup = new Group(mainComp, SWT.NONE);
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        dsTypeGroup.setLayoutData(layoutData);
        GridLayout layout = new GridLayout();
        dsTypeGroup.setLayout(layout);
        dsTypeGroup.setText("DataStore Type:");

        Combo typeCombo = new Combo(dsTypeGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        typeCombo.setLayoutData(layoutData);

        for (String pluginName : getPluginNames()) {
            typeCombo.add(pluginName);
        }

        typeCombo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                String pluginName = ((Combo) e.widget).getText();
                selectPlugin(pluginName);
            }
        });

        connectionGroup = new Group(mainComp, SWT.NONE);
        layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        connectionGroup.setLayoutData(layoutData);
        layout = new GridLayout();
        connectionGroup.setLayout(layout);
        connectionGroup.setText("Connection Parameters:");

        Composite btnComp = new Composite(mainComp, SWT.NONE);
        layoutData = new GridData(SWT.END, SWT.FILL, true, true);
        btnComp.setLayoutData(layoutData);
        layout = new GridLayout(2, false);
        btnComp.setLayout(layout);

        connectButton = new Button(btnComp, SWT.PUSH);
        layoutData = new GridData(SWT.END, SWT.CENTER, false, false);
        connectButton.setLayoutData(layoutData);
        connectButton.setText("Connect");
        connectButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                connect();
            }
        });

        disconnectButton = new Button(btnComp, SWT.PUSH);
        layoutData = new GridData(SWT.END, SWT.CENTER, false, false);
        disconnectButton.setLayoutData(layoutData);
        disconnectButton.setText("Disconnect");
        disconnectButton.setEnabled(false);
        disconnectButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                disconnect();
            }
        });

        Group productGroup = new Group(mainComp, SWT.NONE);
        layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        productGroup.setLayoutData(layoutData);
        productGroup.setText("Load As:");
        layout = new GridLayout(2, true);
        productGroup.setLayout(layout);

        Button mapButton = new Button(productGroup, SWT.RADIO);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        mapButton.setLayoutData(layoutData);
        mapButton.setText("Map");
        mapButton.setSelection(!loadAsProduct);

        Button productButton = new Button(productGroup, SWT.RADIO);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        productButton.setLayoutData(layoutData);
        productButton.setText("Product");
        productButton.setSelection(loadAsProduct);

        timeRangeEntry = new TimeRangeEntry(productGroup, SWT.HORIZONTAL);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.horizontalSpan = 2;
        timeRangeEntry.setLayoutData(layoutData);
        timeRangeEntry.setDateFormat(DATE_TIME_FORMAT);
        timeRangeEntry.setTimeZone(TimeZone.getTimeZone("GMT"));
        timeRangeEntry.setTimeRange(currentFrame.getValidPeriod());
        timeRangeEntry.setEnabled(loadAsProduct);

        productButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                Button button = (Button) e.widget;
                loadAsProduct = button.getSelection();
                timeRangeEntry.setEnabled(loadAsProduct);
            }
        });

        Group tableGroup = new Group(mainComp, SWT.NONE);
        layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        tableGroup.setLayoutData(layoutData);
        layout = new GridLayout(1, false);
        tableGroup.setLayout(layout);
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        tableGroup.setText("Table:");

        tablesList = new List(tableGroup, SWT.SINGLE | SWT.BORDER
                | SWT.V_SCROLL | SWT.H_SCROLL);
        layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        layoutData.heightHint = tablesList.getItemHeight() * 12;
        tablesList.setLayoutData(layoutData);
        tablesList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                if (tablesList.getSelectionCount() > 0) {
                    typeName = tablesList.getSelection()[0];
                    GisDataStoreParametersDialog.this.okButton.setEnabled(true);
                    buttonPressed(IDialogConstants.OK_ID);
                }
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (tablesList.getSelectionCount() > 0) {
                    typeName = tablesList.getSelection()[0];
                    okButton.setEnabled(true);
                }
            }
        });

        applyDialogFont(mainComp);

        IPersistentPreferenceStore prefs = Activator.getDefault()
                .getPreferenceStore();
        String pluginName = prefs.getString(GIS_DATA_STORE_PLUGIN_PREF);
        if (!Arrays.asList(getPluginNames()).contains(pluginName)) {
            pluginName = getPluginNames()[0];
            prefs.setToDefault(GIS_DATA_STORE_PLUGIN_PREF);
            try {
                prefs.save();
            } catch (IOException e1) {
                Activator.statusHandler
                        .error("Unable to save most recently used GIS Plugin to prefrences",
                                e1);
            }
        }

        typeCombo.setText(pluginName);
        selectedPlugin = null;
        selectPlugin(pluginName);
        return mainComp;
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        super.createButtonsForButtonBar(parent);
        okButton = getButton(IDialogConstants.OK_ID);
        okButton.setEnabled(false);
    }

    /*
     * (non-Javadoc) Method declared on Dialog.
     */
    @Override
    protected void buttonPressed(int buttonId) {
        if (dataStore != null) {
            dataStore.dispose();
        }
        super.buttonPressed(buttonId);
    }

    @Override
    protected void okPressed() {
        try {
            DataStoreResourceData rd = gdsPlugin.constructResourceData(
                    typeName, gdsPlugin.getConnectionParameters());
            DataStoreResource rsc = rd.construct(new LoadProperties(),
                    descriptor);

            if (loadAsProduct) {
                rsc.setTimeRange(timeRangeEntry.getTimeRange());
            }

            RGB color = ColorUtil.getNewColor(descriptor);
            rsc.getCapability(ColorableCapability.class).setColor(color);

            ResourceProperties props = new ResourceProperties();
            props.setVisible(true);
            props.setMapLayer(!loadAsProduct);
            descriptor.getResourceList().add(rsc, props);

        } catch (Exception e) {
            Activator.statusHandler.error("Error importing GIS resource: ", e);
        }

        super.okPressed();
    }

    private void selectPlugin(String pluginName) {
        if (!pluginName.equals(selectedPlugin)) {
            gdsPlugin = getPlugin(pluginName);
            for (Control c : connectionGroup.getChildren()) {
                c.dispose();
            }
            disconnect();
            gdsPlugin.createControls(connectionGroup);
            gdsPlugin.loadFromPreferences();
            connectionGroup.layout();
            getShell().pack();
            selectedPlugin = pluginName;
        }
    }

    private void connect() {
        try {
            dataStore = gdsPlugin.connectToDataStore();
            disconnectButton.setEnabled(true);
            connectButton.setEnabled(false);

            IPersistentPreferenceStore prefs = Activator.getDefault()
                    .getPreferenceStore();
            prefs.setValue(GIS_DATA_STORE_PLUGIN_PREF, selectedPlugin);
            try {
                prefs.save();
            } catch (IOException e1) {
                Activator.statusHandler
                        .error("Unable to save most recently used GIS Plugin to prefrences",
                                e1);
            }

            gdsPlugin.saveToPreferences();

            String[] typeNames = dataStore.getTypeNames();
            Arrays.sort(typeNames);
            tablesList.setItems(typeNames);
            if (typeName != null) {
                int index = tablesList.indexOf(typeName);
                if (index >= 0) {
                    tablesList.select(index);
                }
            }
        } catch (Exception e) {
            Activator.statusHandler.error(e.getLocalizedMessage(), e);
        }
    }

    private void disconnect() {
        if (dataStore != null) {
            dataStore.dispose();
        }
        connectButton.setEnabled(true);
        disconnectButton.setEnabled(false);
        tablesList.removeAll();
    }

    public Map<String, Object> getConnectionParameters() {
        return gdsPlugin.getConnectionParameters();
    }

    private IGisDataStorePlugin getPlugin(String pluginName) {
        getPluginNames(); // ensure plugins are loaded
        return plugins.get(pluginName);
    }

    private String[] getPluginNames() {
        if (pluginNames == null) {
            IExtensionRegistry registry = Platform.getExtensionRegistry();
            IExtensionPoint point = registry
                    .getExtensionPoint(EXTENSION_POINT_ID);
            plugins = new HashMap<String, IGisDataStorePlugin>();
            if (point != null) {
                IExtension[] extensions = point.getExtensions();

                for (IExtension ext : extensions) {
                    IConfigurationElement[] config = ext
                            .getConfigurationElements();

                    for (IConfigurationElement cfg : config) {
                        try {
                            IGisDataStorePlugin plugin = (IGisDataStorePlugin) cfg
                                    .createExecutableExtension("class");
                            String pluginName = cfg.getAttribute("name");
                            plugins.put(pluginName, plugin);
                        } catch (CoreException e) {
                            Activator.statusHandler.error(
                                    "Error loading unit registrations for "
                                            + cfg.getAttribute("class"), e);
                        }
                    }
                }
            }
            pluginNames = plugins.keySet().toArray(new String[plugins.size()]);
            Arrays.sort(pluginNames);
        }
        return pluginNames;
    }

}
