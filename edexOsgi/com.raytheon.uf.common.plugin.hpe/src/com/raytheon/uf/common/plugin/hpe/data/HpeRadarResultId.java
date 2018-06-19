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
package com.raytheon.uf.common.plugin.hpe.data;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Embeddable;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Representative of the primary key for {@link HpeRadarResult}.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 11, 2016 5631       bkowal      Initial creation
 * Nov 18, 2016 5631       bkowal      Added {@link #hashCode()) and {@link #equals(Object)}.
 *
 * </pre>
 *
 * @author bkowal
 */
@Embeddable
@DynamicSerialize
public class HpeRadarResultId implements Serializable {

    private static final long serialVersionUID = -5829059578918147922L;

    @DynamicSerializeElement
    @Column(name = "hpe_productname", nullable = false, length = 30)
    private String hpeProductName;

    @DynamicSerializeElement
    @Column(nullable = false, length = 29)
    private Date productTime;

    public HpeRadarResultId() {
    }

    public HpeRadarResultId(String hpeProductName, Date productTime) {
        this.hpeProductName = hpeProductName;
        this.productTime = productTime;
    }

    public String getHpeProductName() {
        return hpeProductName;
    }

    public void setHpeProductName(String hpeProductName) {
        this.hpeProductName = hpeProductName;
    }

    public Date getProductTime() {
        return productTime;
    }

    public void setProductTime(Date productTime) {
        this.productTime = productTime;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((hpeProductName == null) ? 0 : hpeProductName.hashCode());
        result = prime * result
                + ((productTime == null) ? 0 : productTime.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        HpeRadarResultId other = (HpeRadarResultId) obj;
        if (hpeProductName == null) {
            if (other.hpeProductName != null)
                return false;
        } else if (!hpeProductName.equals(other.hpeProductName))
            return false;
        if (productTime == null) {
            if (other.productTime != null)
                return false;
        } else if (!productTime.equals(other.productTime))
            return false;
        return true;
    }
}