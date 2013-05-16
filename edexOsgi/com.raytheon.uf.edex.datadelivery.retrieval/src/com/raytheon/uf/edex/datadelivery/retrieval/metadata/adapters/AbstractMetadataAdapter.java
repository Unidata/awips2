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
package com.raytheon.uf.edex.datadelivery.retrieval.metadata.adapters;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.madis.MadisRecord;
import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * 
 * Abstract class for converting RetrievalAttribute to PluginDataObjects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 19, 2012            bsteffen     Initial javadoc
 * May 12, 2013  753       dhladky      Added support for Madis
 * 
 * </pre>
 * 
 * @author unknown
 * @version 1.0
 */
public abstract class AbstractMetadataAdapter<RecordKey> implements ISerializableObject {

    protected PluginDataObject[] pdos;
    
    protected RetrievalAttribute attXML;

    protected static Map<String, String[]> parameterMap = new HashMap<String, String[]>();

    public static AbstractMetadataAdapter<?> getMetadataAdapter(Class<?> clazz,
            RetrievalAttribute attXML) throws InstantiationException {

        AbstractMetadataAdapter<?> adapter = null;
        
        if (clazz == GridRecord.class) {
            adapter = new GridMetadataAdapter(attXML);
        } else if (clazz == MadisRecord.class) {
            adapter = new MadisMetadataAdapter(attXML);
        } else {
            throw new IllegalArgumentException("Did not receive a class recognzed for retreival.");
        }

        return adapter;
    }

    // setup an individual record from the direct plugin translation
    public abstract PluginDataObject getRecord(RecordKey o);
    
    // set the size of the PDO array to return
    public abstract void setPdos(int size);
    
    /**
     * get the PDO list
     * @return
     */
    public PluginDataObject[] getPdos() {
        return pdos;
    }
  
}
