package com.raytheon.uf.edex.plugin.madis.ogc;

import java.util.ArrayList;

import javax.xml.bind.JAXBElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.madis.MadisRecord;
import com.raytheon.uf.edex.plugin.madis.MadisPointDataTransform;
import com.raytheon.uf.edex.plugin.madis.ogc.feature.Madis;
import com.raytheon.uf.edex.plugin.madis.ogc.feature.MadisObjectFactory;
import com.raytheon.uf.edex.wfs.reg.IPdoGmlTranslator;

/**
 * 
 * Madis Layer Translator Impl
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/01/2013   1746       dhladky      Initial creation
 * 05/30/2013   753        dhladky      updated
 * June 03, 2013 1763      dhladky      Bulked the PointDataQuery
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public class MadisTranslator implements IPdoGmlTranslator {

    public static final String GML_VERSION = "3.1.1";

    public MadisTranslator() {

    }

    @Override
    public ArrayList<JAXBElement<?>> translate(PluginDataObject[] pdos) {
        ArrayList<JAXBElement<?>> rval = new ArrayList<JAXBElement<?>>(
                pdos.length);

        MadisPointDataTransform.populatePointDataFields(pdos);

        for (PluginDataObject pdo : pdos) {
            rval.add(translate(pdo));
        }
        return rval;
    }

    /**
     * @param pdo
     * @return
     */
    public JAXBElement<Madis> translate(PluginDataObject pdo) {

        return new MadisObjectFactory().create(new Madis((MadisRecord) pdo));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wfs.reg.PdoGmlTranslator#getVersion()
     */
    @Override
    public String getVersion() {
        return GML_VERSION;
    }

}
