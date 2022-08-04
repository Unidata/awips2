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
package com.raytheon.uf.edex.plugin.text.dbsrv.security;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

/**
 * The white list paths associated with an environment name.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 03, 2015 4492       rferrel     Initial creation
 * Jan 18, 2016 4562       tjensen     Moved from edex.plugin.text to 
 *                                     edex.plugin.text.dbsrv
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class EnvironmentCfg {
    @XmlAttribute(required = true)
    private String name;

    @XmlElement(name = "path")
    private List<String> whitePaths;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<String> getWhitePaths() {
        return whitePaths;
    }

    public void setWhitePaths(List<String> whitePaths) {
        this.whitePaths = whitePaths;
    }
}
