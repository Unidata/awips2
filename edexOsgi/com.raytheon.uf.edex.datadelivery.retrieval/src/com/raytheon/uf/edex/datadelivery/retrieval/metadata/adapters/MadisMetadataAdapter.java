package com.raytheon.uf.edex.datadelivery.retrieval.metadata.adapters;

import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.madis.MadisRecord;
import com.raytheon.uf.edex.plugin.madis.ogc.feature.Madis;

/**
 * 
 * Convert RetrievalAttribute to MadisRecords.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2013  753          dhladky    Initial javadoc
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class MadisMetadataAdapter extends AbstractMetadataAdapter<Madis> {

    public MadisMetadataAdapter(RetrievalAttribute attXML)
            throws InstantiationException {

        this.attXML = attXML;
    }

    @Override
    public PluginDataObject getRecord(Madis madis) {
        return madis.getRecord();
    }

    @Override
    public void setPdos(int size) {
        pdos = new MadisRecord[size];
    }
    
}
