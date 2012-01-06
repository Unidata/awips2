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
package com.raytheon.uf.common.dataplugin.gfe.request;

import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 22, 2009 3058       rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

@DynamicSerialize
public class IscGetRequestXmlRequest extends AbstractGfeRequest {

    /** The xml to parse through to get the correct server lists */
    @DynamicSerializeElement
    private String xml;

    /** The servers that have been selected */
    @DynamicSerializeElement
    private List<String> selectedServers;

    /** The weather elements that have been selected */
    @DynamicSerializeElement
    private List<String> selectedWEList;

    public List<String> getSelectedServers() {
        return selectedServers;
    }

    public void setSelectedServers(List<String> selectedServers) {
        this.selectedServers = selectedServers;
    }

    public List<String> getSelectedWEList() {
        return selectedWEList;
    }

    public void setSelectedWEList(List<String> selectedWEList) {
        this.selectedWEList = selectedWEList;
    }

    public String getXml() {
        return xml;
    }

    public void setXml(String xml) {
        this.xml = xml;
    }
}
