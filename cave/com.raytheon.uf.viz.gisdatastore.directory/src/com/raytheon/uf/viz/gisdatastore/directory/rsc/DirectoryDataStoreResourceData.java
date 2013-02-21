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
package com.raytheon.uf.viz.gisdatastore.directory.rsc;

import java.io.IOException;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import org.geotools.data.DataStore;
import org.geotools.data.directory.DirectoryDataStoreFactory;

import com.raytheon.uf.viz.gisdatastore.rsc.DataStoreResourceData;

/**
 * DataStore resource data class for GIS feature files
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
public class DirectoryDataStoreResourceData extends DataStoreResourceData {
    public DirectoryDataStoreResourceData() {
    }

    public DirectoryDataStoreResourceData(String typeName,
            Map<String, Object> connectionParameters) {
        super(typeName, connectionParameters);
    }

    @Override
    protected DataStore constructDataStore() throws IOException {
        DirectoryDataStoreFactory factory = new DirectoryDataStoreFactory();
        Map<String, Serializable> params = new HashMap<String, Serializable>();
        for (Entry<String, Object> entry : getConnectionParameters().entrySet()) {
            if (entry.getValue() instanceof Serializable) {
                params.put(entry.getKey(), (Serializable) entry.getValue());
            } else {
                throw new IllegalArgumentException(entry.getKey() + "("
                        + entry.getValue().getClass().getName()
                        + ") not an instance of java.io.Serializable");
            }
        }
        DataStore dataStore = factory.createDataStore(params);
        return dataStore;
    }
}
