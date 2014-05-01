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
package com.raytheon.uf.edex.decodertools.core.filterimpl;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2009            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public abstract class AbstractFilterElement {

    @XmlElement
    @DynamicSerializeElement
    private String filterElementName;

    @XmlElement
    @DynamicSerializeElement
    private String filterType;
    
    /**
     * Executes this filter element against the supplied report data. The
     * supplied report is returned if it matches the filter criteria. A null
     * report reference is returned if the report fails.
     * @param report
     * @return
     */
    public abstract PluginDataObject filter(PluginDataObject report);

    /**
     * @return the filterElementName
     */
    public String getFilterElementName() {
        return filterElementName;
    }

    /**
     * @param filterElementName the filterElementName to set
     */
    public void setFilterElementName(String filterElementName) {
        this.filterElementName = filterElementName;
    }

    /**
     * @return the filterType
     */
    public String getFilterType() {
        return filterType;
    }

    /**
     * @param filterType the filterType to set
     */
    public void setFilterType(String filterType) {
        this.filterType = filterType;
    }

}
