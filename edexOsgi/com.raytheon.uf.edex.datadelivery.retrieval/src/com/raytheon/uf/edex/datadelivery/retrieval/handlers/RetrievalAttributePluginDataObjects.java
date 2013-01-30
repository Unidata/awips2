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
package com.raytheon.uf.edex.datadelivery.retrieval.handlers;

import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Plugin data objects and the retrieval information they are associated with.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 01, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@DynamicSerialize
public class RetrievalAttributePluginDataObjects {

    @DynamicSerializeElement
    private RetrievalAttribute attributeXml;

    @DynamicSerializeElement
    private PluginDataObject[] pluginDataObjects;

    /**
     * Constructor.
     */
    public RetrievalAttributePluginDataObjects() {
    }

    /**
     * Constructor.
     * 
     * @param attributeXml
     * @param pluginDataObjects
     */
    public RetrievalAttributePluginDataObjects(RetrievalAttribute attributeXml,
            PluginDataObject[] pluginDataObjects) {
        this.attributeXml = attributeXml;
        this.pluginDataObjects = pluginDataObjects;
    }

    /**
     * @return the attributeXml
     */
    public RetrievalAttribute getAttributeXml() {
        return attributeXml;
    }

    /**
     * @param attributeXml
     *            the attributeXml to set
     */
    public void setAttributeXml(RetrievalAttribute attributeXml) {
        this.attributeXml = attributeXml;
    }

    /**
     * @return the pluginDataObjects
     */
    public PluginDataObject[] getPluginDataObjects() {
        return pluginDataObjects;
    }

    /**
     * @param pluginDataObjects
     *            the pluginDataObjects to set
     */
    public void setPluginDataObjects(PluginDataObject[] pluginDataObjects) {
        this.pluginDataObjects = pluginDataObjects;
    }

}
