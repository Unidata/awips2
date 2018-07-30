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
package com.raytheon.uf.edex.plugin.mpe.rocchecker;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * RocCheckerConstraints (optional) used to limit the checks that are performed
 * to specific lid(s) and/or pe(s).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 23, 2016 5590       bkowal      Initial creation
 * Jun 24, 2016 5699       bkowal      Added {@link #isEmptyConstraints()} and
 *                                     {@link #toString()}.
 * 
 * </pre>
 * 
 * @author bkowal
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "constraints")
public class RocCheckerConstraints {

    @XmlElement(name = "lid")
    private List<String> lids;

    @XmlElement(name = "pe")
    private List<String> pes;

    public RocCheckerConstraints() {
    }

    /**
     * Determines if this instance of {@link RocCheckerConstraints} does not
     * actually specify any specified lid or pe constraints.
     * 
     * @return {@code true}, if this is an empty constraint; {@code false},
     *         otherwise.
     */
    public boolean isEmptyConstraints() {
        return CollectionUtils.isEmpty(lids) && CollectionUtils.isEmpty(pes);
    }

    public List<String> getLids() {
        return lids;
    }

    public void setLids(List<String> lids) {
        this.lids = lids;
    }

    public List<String> getPes() {
        return pes;
    }

    public void setPes(List<String> pes) {
        this.pes = pes;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("RocCheckerConstraints [");
        boolean comma = false;
        if (CollectionUtils.isNotEmpty(lids)) {
            comma = true;
            sb.append("lids={")
                    .append(StringUtils.collectionToCommaDelimitedString(lids))
                    .append("}");
        }
        if (CollectionUtils.isNotEmpty(pes)) {
            if (comma) {
                sb.append(", ");
            }
            sb.append("pes={")
                    .append(StringUtils.collectionToCommaDelimitedString(pes))
                    .append("}");
        }
        sb.append("]");
        return sb.toString();
    }
}