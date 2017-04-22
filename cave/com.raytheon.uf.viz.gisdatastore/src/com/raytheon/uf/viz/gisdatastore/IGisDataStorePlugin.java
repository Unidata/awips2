package com.raytheon.uf.viz.gisdatastore;

import java.io.IOException;
import java.util.Map;

import org.eclipse.swt.widgets.Composite;
import org.geotools.data.DataStore;

import com.raytheon.uf.viz.gisdatastore.rsc.DataStoreResourceData;

public interface IGisDataStorePlugin {

    public void createControls(final Composite comp);

    public Map<String, Object> getConnectionParameters();

    public void loadFromPreferences();

    public void saveToPreferences();

    public DataStore connectToDataStore() throws IOException;

    public DataStoreResourceData constructResourceData(String typeName,
            Map<String, Object> connectionParameters);
}