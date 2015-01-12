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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.wmo.AFOSProductId;
import com.raytheon.uf.common.wmo.WMOHeader;

/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12May2010    2187        cjeanbap   Initial Implementation
 * 21Sep2010    2187        cjeanbap   Add constant variable
 * 
 * </pre>
 * 
 * @author cjeanbap
 * @version 1.0
 */
@Entity
@Table(name = "stdTextProducts")
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
            String bbbid, Long createtime, String product) {
        super(prodIdToCopy, bbbid, createtime, product);        
    }

    public OperationalStdTextProduct(String wmoid, String site, String cccid,
            String nnnid, String xxxid, String hdrtime, String bbbid,
            Long createtime, String product) {
        super(wmoid, site, cccid, nnnid, xxxid, hdrtime, bbbid, createtime, product);        
    }

    public OperationalStdTextProduct(WMOHeader wmoHeader, AFOSProductId afosId,
            String product) {
        super(wmoHeader, afosId, product);        
    }
}
