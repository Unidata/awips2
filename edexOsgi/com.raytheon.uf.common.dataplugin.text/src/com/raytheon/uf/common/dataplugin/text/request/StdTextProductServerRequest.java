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

package com.raytheon.uf.common.dataplugin.text.request;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Standard Text Product Server Request class to be used in a thrift service.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 19, 2010            cjeanbap    Initial creation
 * Sep 30, 2015 4860       skorolev    Corrected misspelling.
 * 
 * </pre>
 * 
 * @author cjeanbap
 * @version 1.0
 */
@DynamicSerialize
public class StdTextProductServerRequest implements IServerRequest {

    // attribute of StdTExtProductId
    @DynamicSerializeElement
    private String wmoid;

    // attribute of StdTExtProductId
    @DynamicSerializeElement
    private String site;

    // attribute of StdTExtProductId
    @DynamicSerializeElement
    private String cccid;

    // attribute of StdTExtProductId
    @DynamicSerializeElement
    private String nnnid;

    // attribute of StdTExtProductId
    @DynamicSerializeElement
    private String xxxid;

    // attribute of StdTExtProductId
    @DynamicSerializeElement
    private String hdrtime;

    // attribute of StdTExtProductId
    @DynamicSerializeElement
    private Long dataCrc;

    @DynamicSerializeElement
    private String bbbid;

    @DynamicSerializeElement
    private Long createTime;

    @DynamicSerializeElement
    private String product;

    @DynamicSerializeElement
    private boolean operationalFlag;

    public StdTextProductServerRequest() {
    }

    /**
     * full constructor
     */
    public StdTextProductServerRequest(String wmoid, String site, String cccid,
            String nnnid, String xxxid, String hdrtime, String bbbid,
            Long dataCrc, Long createTime, String product,
            boolean operationalFlag) {
        this.wmoid = wmoid;
        this.site = site;
        this.cccid = cccid;
        this.nnnid = nnnid;
        this.xxxid = xxxid;
        this.hdrtime = hdrtime;
        this.bbbid = bbbid;
        this.dataCrc = dataCrc;
        this.createTime = createTime;
        this.product = product;
        this.operationalFlag = operationalFlag;
    }

    public String getBbbid() {
        return this.bbbid;
    }

    public void setBbbid(String bbbid) {
        this.bbbid = bbbid;
    }

    public Long getCreatetime() {
        return this.createTime;
    }

    public void setCreatetime(Long createtime) {
        this.createTime = createtime;
    }

    public String getProduct() {
        return this.product;
    }

    public void setProduct(String product) {
        this.product = product;
    }

    public String getWmoid() {
        return this.wmoid;
    }

    public void setWmoid(String wmoid) {
        this.wmoid = wmoid;
    }

    public String getSite() {
        return this.site;
    }

    public void setSite(String site) {
        this.site = site;
    }

    public String getCccid() {
        return this.cccid;
    }

    public void setCccid(String cccid) {
        this.cccid = cccid;
    }

    public String getNnnid() {
        return this.nnnid;
    }

    public void setNnnid(String nnnid) {
        this.nnnid = nnnid;
    }

    public String getXxxid() {
        return this.xxxid;
    }

    public void setXxxid(String xxxid) {
        this.xxxid = xxxid;
    }

    public String getHdrtime() {
        return this.hdrtime;
    }

    public void setHdrtime(String hdrtime) {
        this.hdrtime = hdrtime;
    }

    public Long getDataCrc() {
        return this.dataCrc;
    }

    public void setDataCrc(Long dataCrc) {
        this.dataCrc = dataCrc;
    }

    public boolean isOperationalFlag() {
        return operationalFlag;
    }

    public void setOperationalFlag(boolean operationalFlag) {
        this.operationalFlag = operationalFlag;
    }

}
