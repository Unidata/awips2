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
package com.raytheon.edex.transform.shef;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.edex.decodertools.core.filterimpl.AbstractFilterElement;

/**
 * Used to group {@link AbstractFilterElement}s with a single configuration
 * file. Multiple {@link SynopticToShefRun}s can be processed by the system.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 28, 2015 4783       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class SynopticToShefRun {
    @XmlElement
    @DynamicSerializeElement
    private String configFileName;

    @XmlElement
    @DynamicSerializeElement
    protected List<AbstractFilterElement> filterElements = new ArrayList<AbstractFilterElement>();

    @XmlElement
    @DynamicSerializeElement
    private String filterName;

    public SynopticToShefRun() {
    }

    /**
     * @return the configFileName
     */
    public String getConfigFileName() {
        return configFileName;
    }

    /**
     * @param configFileName
     *            the configFileName to set
     */
    public void setConfigFileName(String configFileName) {
        this.configFileName = configFileName;
    }

    /**
     * @return the filterElements
     */
    public List<AbstractFilterElement> getFilterElements() {
        return filterElements;
    }

    /**
     * @param filterElements
     *            the filterElements to set
     */
    public void setFilterElements(List<AbstractFilterElement> filterElements) {
        this.filterElements = filterElements;
    }

    /**
     * @return the filterName
     */
    public String getFilterName() {
        return filterName;
    }

    /**
     * @param filterName
     *            the filterName to set
     */
    public void setFilterName(String filterName) {
        this.filterName = filterName;
    }
}