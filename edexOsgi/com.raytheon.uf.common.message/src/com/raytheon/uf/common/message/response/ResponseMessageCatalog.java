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

package com.raytheon.uf.common.message.response;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.message.CatalogItem;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Response message for the Catalog
 * 
 * A response can consist of two possibilities: <LI>A list of catalog items -
 * one per data item <LI>A distinct list of values for a parameter
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Nov 13, 2006             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ResponseMessageCatalog extends AbstractResponseMessage {

    @XmlElement
    @DynamicSerializeElement
    private CatalogItem[] items;

    @XmlElement
    @DynamicSerializeElement
    private String[] values;

    public ResponseMessageCatalog() {
        this.items = new CatalogItem[] {};
        this.values = new String[] {};
        this.fileType = "";
        this.dataURI = "";
        this.validTime = new Date();
    }

    /**
     * @return the items
     */
    public CatalogItem[] getItems() {
        return items;
    }

    /**
     * @param items
     *            the items to set
     */
    public void setItems(CatalogItem[] items) {
        this.items = items;
    }

    /**
     * @return the values
     */
    public String[] getValues() {
        return values;
    }

    /**
     * @param values
     *            the values to set
     */
    public void setValues(String[] values) {
        this.values = values;
    }

    public static List<ResponseMessageCatalog> wrap(ResponseMessageCatalog rmc) {
        List<ResponseMessageCatalog> retVal = new ArrayList<ResponseMessageCatalog>();
        for (CatalogItem item : rmc.getItems()) {
            ResponseMessageCatalog resp = new ResponseMessageCatalog();
            resp.setItems(new CatalogItem[] { item });
            retVal.add(resp);
        }
        return retVal;
    }

}
