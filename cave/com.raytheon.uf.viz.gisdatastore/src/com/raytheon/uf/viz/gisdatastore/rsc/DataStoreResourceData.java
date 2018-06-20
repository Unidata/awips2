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
package com.raytheon.uf.viz.gisdatastore.rsc;

import java.io.IOException;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.geotools.data.DataStore;

import com.raytheon.uf.common.serialization.XmlGenericMapAdapter;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Abstract base class for GeoTools DataStore base resource data classes
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 31, 2012      #1326 randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public abstract class DataStoreResourceData extends AbstractResourceData {
    private DataStore dataStore;

    @XmlElement
    @XmlJavaTypeAdapter(XmlGenericMapAdapter.class)
    private Map<String, Object> connectionParameters;

    @XmlElement
    private String typeName;

    @XmlElement
    private String mapName;

    /**
     * 
     */
    public DataStoreResourceData() {
    }

    public DataStoreResourceData(String typeName,
            Map<String, Object> connectionParameters) {
        this.typeName = typeName;
        this.mapName = typeName;
        this.connectionParameters = connectionParameters;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon
     * .uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public DataStoreResource construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {

        DataStoreResource rsc;
        try {
            rsc = new DataStoreResource(this, loadProperties);
        } catch (IOException e) {
            throw new VizException("Error constructing GIS resource", e);
        }
        return rsc;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#update(java.lang.Object
     * )
     */
    @Override
    public void update(Object updateData) {
        // Default is to do nothing.
    }

    public DataStore getDataStore() throws IOException {
        if (dataStore == null) {
            dataStore = constructDataStore();
        }
        return dataStore;
    }

    protected abstract DataStore constructDataStore() throws IOException;

    public void disposeDataStore() {
        if (dataStore != null) {
            dataStore.dispose();
            dataStore = null;
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (!(obj instanceof DataStoreResourceData)) {
            return false;
        }
        DataStoreResourceData other = (DataStoreResourceData) obj;
        if (connectionParameters == null) {
            if (other.connectionParameters != null) {
                return false;
            }
        } else if (!connectionParameters.equals(other.connectionParameters)) {
            return false;
        }
        if (mapName == null) {
            if (other.mapName != null) {
                return false;
            }
        } else if (!mapName.equals(other.mapName)) {
            return false;
        }
        if (typeName == null) {
            if (other.typeName != null) {
                return false;
            }
        } else if (!typeName.equals(other.typeName)) {
            return false;
        }
        return true;
    }

    public Map<String, Object> getConnectionParameters() {
        return connectionParameters;
    }

    public void setConnectionParameters(Map<String, Object> connectionParameters) {
        this.connectionParameters = connectionParameters;
    }

    public String getTypeName() {
        return typeName;
    }

    public void setTypeName(String typeName) {
        this.typeName = typeName;
        if (this.mapName == null) {
            this.mapName = typeName;
        }
    }

    String getMapName() {
        return mapName;
    }

    void setMapName(String mapName) {
        this.mapName = mapName;
    }
}
