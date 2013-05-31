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
package com.raytheon.uf.edex.datadelivery.retrieval.response;

import org.junit.Ignore;

import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.edex.datadelivery.retrieval.metadata.adapters.MockGridMetaDataAdapter;

/**
 * Overrides specific methods in {@link OpenDAPTranslator} that require external
 * system interaction.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 06, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore
public class MockOpenDAPTranslator extends OpenDAPTranslator {

    /**
     * @param attXML
     * @param className
     * @throws InstantiationException
     */
    public MockOpenDAPTranslator(RetrievalAttribute attXML)
            throws InstantiationException {
        super(attXML, GridRecord.class.getName());
        this.attXML = attXML;
        this.metadataAdapter = new MockGridMetaDataAdapter();
        metadataAdapter.processAttributeXml(attXML);
    }

    @Override
    protected void configureFromPdoClassName(String className)
            throws ClassNotFoundException, InstantiationException {
        setPdoClass(className);
    }

    @Override
    void constructDataUri(PluginDataObject record)
    throws PluginException {
        // Ignored, because you need all the MasterLevels and a bunch of other
        // stuff to do this
    }
}
