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
package com.raytheon.uf.common.plugin.hpe.request;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * HPE bias source request object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2014   3026     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
public class HpeLabelDataRequest implements IServerRequest {

    /** HPE Product name */
    @DynamicSerializeElement
    private String productName;

    /** HPE list of dates */
    @DynamicSerializeElement
    private List<Date> dateList;

    /**
     * Constructor.
     */
    public HpeLabelDataRequest() {

    }

    /**
     * Constructor.
     * 
     * @param productName
     *            The Hpe product name
     * @param dateList
     *            List of times for the product
     */
    public HpeLabelDataRequest(String productName, List<Date> dateList) {
        this.productName = productName;
        this.dateList = dateList;
    }

    /**
     * Constructor.
     * 
     * @param productName
     *            The Hpe product name
     * @param date
     *            Date of the product
     */
    public HpeLabelDataRequest(String productName, Date date) {
        this.productName = productName;
        dateList = new ArrayList<Date>(1);
        dateList.add(date);
    }

    /**
     * @return the productName
     */
    public String getProductName() {
        return productName;
    }

    /**
     * @param productName
     *            the productName to set
     */
    public void setProductName(String productName) {
        this.productName = productName;
    }

    /**
     * @return the dateList
     */
    public List<Date> getDateList() {
        return dateList;
    }

    /**
     * @param dateList
     *            the dateList to set
     */
    public void setDateList(List<Date> dateList) {
        this.dateList = dateList;
    }
}
