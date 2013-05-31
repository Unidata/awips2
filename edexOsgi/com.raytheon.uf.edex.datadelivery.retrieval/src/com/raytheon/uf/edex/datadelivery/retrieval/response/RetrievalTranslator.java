package com.raytheon.uf.edex.datadelivery.retrieval.response;

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

import java.util.ArrayList;

import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalTranslator;
import com.raytheon.uf.edex.datadelivery.retrieval.metadata.adapters.AbstractMetadataAdapter;

/**
 * Abstract Translator
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2011    191     dhladky      Initial creation
 * Feb 07, 2013 1543       djohnson     Allow overriding of methods for mocking in tests.
 * Feb 12, 2013 1543       djohnson     Pass the exception as the cause for instantiation exceptions.
 * May 31, 2013 2038       djohnson     Protected access for constructor.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public abstract class RetrievalTranslator implements IRetrievalTranslator {

    protected Class<?> recordClass;

    protected RetrievalAttribute attXML;

    protected AbstractMetadataAdapter metadataAdapter;

    /**
     * Used by all translators
     * 
     * @param attXML
     */
    public RetrievalTranslator(RetrievalAttribute attXML)
            throws InstantiationException {
        this.attXML = attXML;

        try {
            PluginFactory factory = PluginFactory.getInstance();
            String clazz = factory.getPluginRecordClassName(getAttribute()
                    .getPlugin());
            configureFromPdoClassName(clazz);
        } catch (Exception e) {
            throw new InstantiationException(e.toString());
        }
    }

    protected RetrievalTranslator(RetrievalAttribute attXML, String className)
            throws InstantiationException {
        this.attXML = attXML;
        try {
            configureFromPdoClassName(className);
        } catch (Exception e) {
            InstantiationException ie = new InstantiationException();
            ie.initCause(e);
            throw ie;
        }
    }

    protected void configureFromPdoClassName(String className)
            throws InstantiationException, ClassNotFoundException {
        setPdoClass(className);
        metadataAdapter = AbstractMetadataAdapter.getMetadataAdapter(
                getPdoClass(), attXML);

    }

    @Override
    public void setAttribute(RetrievalAttribute attXML) {
        this.attXML = attXML;
    }

    @Override
    public RetrievalAttribute getAttribute() {
        return attXML;
    }

    @Override
    public Class<?> getPdoClass() {
        return recordClass;
    }

    @Override
    public void setPdoClass(String clazz) throws ClassNotFoundException {
        this.recordClass = Class.forName(clazz);
    }

    @Override
    public PluginDataObject getPdo(int index) throws InstantiationException,
            IllegalAccessException {
        PluginDataObject pdo = null;

        if (metadataAdapter != null) {
            pdo = metadataAdapter.getRecord(index);
        }

        return pdo;

    }

    /**
     * Get the number of subset times for your retrieval
     * 
     * @return
     */
    protected abstract int getSubsetNumTimes();

    /**
     * Get the levels for your retrieval
     * 
     * @return
     */
    protected abstract int getSubsetNumLevels();

    /**
     * Get a list of data times for your retrieval
     * 
     * @return
     */
    protected abstract ArrayList<DataTime> getTimes();

}
