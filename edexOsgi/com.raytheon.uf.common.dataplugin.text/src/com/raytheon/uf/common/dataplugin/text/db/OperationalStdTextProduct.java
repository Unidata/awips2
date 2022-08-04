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
package com.raytheon.uf.common.dataplugin.text.db;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.wmo.AFOSProductId;
import com.raytheon.uf.common.wmo.WMOHeader;

/**
 * The Operational Standard Text Product record
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------------
 * May 12, 2010  2187     cjeanbap  Initial Implementation
 * Sep 21, 2010  2187     cjeanbap  Add constant variable
 * Apr 25, 2018  6966     randerso  Added indexes
 * Mar 13, 2020 21048 mgamazaychikov Added wmoid and cccid to constructor
 *
 * </pre>
 *
 * @author cjeanbap
 */
@Entity
@Table(name = "stdTextProducts")
@org.hibernate.annotations.Table(appliesTo = "stdTextProducts", indexes = {
        @Index(name = "textAfosIdRefTime_idx", columnNames = { "cccid", "nnnid",
                "xxxid", "refTime" }),
        @Index(name = "textWmoID_idx", columnNames = { "wmoid", "site" }) })
@XmlRootElement
@DynamicSerialize
public final class OperationalStdTextProduct extends StdTextProduct {

    private static final long serialVersionUID = 1L;

    public static final String OPERATIONAL_TABLE = "stdtextproducts";

    public OperationalStdTextProduct() {
        super();
    }

    public OperationalStdTextProduct(StdTextProduct productToCopy) {
        super(productToCopy);
    }

    public OperationalStdTextProduct(StdTextProductId prodIdToCopy,
            String bbbid, Long createtime, String product, String wmoid, String cccid) {
        super(prodIdToCopy, bbbid, createtime, product, wmoid, cccid);
    }

    public OperationalStdTextProduct(String wmoid, String site, String cccid,
            String nnnid, String xxxid, String hdrtime, String bbbid,
            Long createtime, String product) {
        super(wmoid, site, cccid, nnnid, xxxid, hdrtime, bbbid, createtime,
                product);
    }

    public OperationalStdTextProduct(WMOHeader wmoHeader, AFOSProductId afosId,
            String product) {
        super(wmoHeader, afosId, product);
    }
}
