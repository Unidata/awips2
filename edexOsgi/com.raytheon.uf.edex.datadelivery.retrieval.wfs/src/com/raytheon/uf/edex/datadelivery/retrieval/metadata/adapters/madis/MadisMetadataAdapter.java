package com.raytheon.uf.edex.datadelivery.retrieval.metadata.adapters.madis;

import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.madis.MadisRecord;
import com.raytheon.uf.edex.datadelivery.retrieval.metadata.adapters.AbstractMetadataAdapter;
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
 * May 12, 2013  753       dhladky      Initial javadoc
 * May 31, 2013 2038       djohnson     Move to correct git repo.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class MadisMetadataAdapter extends AbstractMetadataAdapter<Madis> {

    public MadisMetadataAdapter() {
    }

    @Override
    public PluginDataObject getRecord(Madis madis) {
        return madis.getRecord();
    }

    @Override
    public void allocatePdoArray(int size) {
        pdos = new MadisRecord[size];
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void processAttributeXml(RetrievalAttribute attXML)
            throws InstantiationException {
        this.attXML = attXML;
    }
    
}
