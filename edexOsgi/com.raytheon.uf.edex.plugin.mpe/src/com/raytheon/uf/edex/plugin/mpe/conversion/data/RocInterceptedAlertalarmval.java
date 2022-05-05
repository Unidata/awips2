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
package com.raytheon.uf.edex.plugin.mpe.conversion.data;

import java.util.Date;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.shef.tables.Alertalarmval;
import com.raytheon.uf.common.dataplugin.shef.tables.AlertalarmvalId;

/**
 * POJO for JAXB serialization and deserialization of an intercepted
 * {@link Alertalarmval}. TODO: the entire package that this class is a part of
 * should be removed when the decision has been made to use the converted mpe
 * applications.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 29, 2016 5699       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(name = "rocAlertalarmval")
public class RocInterceptedAlertalarmval {

    /*
     * Id fields.
     */
    private String lid;

    private String pe;

    private short dur;

    private String ts;

    private String extremum;

    private float probability;

    private Date validtime;

    private Date basistime;

    private String aaCateg;

    private String aaCheck;

    /*
     * Remaining fields.
     */
    private Double value;

    private Double supplValue;

    private String shefQualCode;

    private Integer qualityCode;

    private Short revision;

    private String productId;

    private Date producttime;

    private Date postingtime;

    private Date actionTime;

    public static Alertalarmval convertAlertAlarm(
            RocInterceptedAlertalarmval interceptedAlertalarmval) {
        AlertalarmvalId id = new AlertalarmvalId();
        id.setLid(interceptedAlertalarmval.lid);
        id.setPe(interceptedAlertalarmval.pe);
        id.setDur(interceptedAlertalarmval.dur);
        id.setTs(interceptedAlertalarmval.ts);
        id.setExtremum(interceptedAlertalarmval.extremum);
        id.setProbability(interceptedAlertalarmval.probability);
        id.setValidtime(interceptedAlertalarmval.validtime);
        id.setBasistime(interceptedAlertalarmval.basistime);
        id.setAaCateg(interceptedAlertalarmval.aaCateg);
        id.setAaCheck(interceptedAlertalarmval.aaCheck);

        Alertalarmval alertalarmval = new Alertalarmval(id);
        alertalarmval.setValue(interceptedAlertalarmval.value);
        alertalarmval.setSupplValue(interceptedAlertalarmval.supplValue);
        alertalarmval.setShefQualCode(interceptedAlertalarmval.shefQualCode);
        alertalarmval.setQualityCode(interceptedAlertalarmval.qualityCode);
        alertalarmval.setRevision(interceptedAlertalarmval.revision);
        alertalarmval.setProductId(interceptedAlertalarmval.productId);
        alertalarmval.setProducttime(interceptedAlertalarmval.producttime);
        alertalarmval.setPostingtime(interceptedAlertalarmval.postingtime);
        return alertalarmval;
    }

    public RocInterceptedAlertalarmval() {
    }

    public RocInterceptedAlertalarmval(final Alertalarmval alertalarmval) {
        final AlertalarmvalId id = alertalarmval.getId();
        lid = id.getLid();
        pe = id.getPe();
        dur = id.getDur();
        ts = id.getTs();
        extremum = id.getExtremum();
        probability = id.getProbability();
        validtime = id.getValidtime();
        basistime = id.getBasistime();
        aaCateg = id.getAaCateg();
        aaCheck = id.getAaCheck();
        value = alertalarmval.getValue();
        supplValue = alertalarmval.getSupplValue();
        shefQualCode = alertalarmval.getShefQualCode();
        qualityCode = alertalarmval.getQualityCode();
        revision = alertalarmval.getRevision();
        productId = alertalarmval.getProductId();
        producttime = alertalarmval.getProducttime();
        postingtime = alertalarmval.getPostingtime();
        actionTime = alertalarmval.getActionTime();
    }

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    public String getPe() {
        return pe;
    }

    public void setPe(String pe) {
        this.pe = pe;
    }

    public short getDur() {
        return dur;
    }

    public void setDur(short dur) {
        this.dur = dur;
    }

    public String getTs() {
        return ts;
    }

    public void setTs(String ts) {
        this.ts = ts;
    }

    public String getExtremum() {
        return extremum;
    }

    public void setExtremum(String extremum) {
        this.extremum = extremum;
    }

    public float getProbability() {
        return probability;
    }

    public void setProbability(float probability) {
        this.probability = probability;
    }

    public Date getValidtime() {
        return validtime;
    }

    public void setValidtime(Date validtime) {
        this.validtime = validtime;
    }

    public Date getBasistime() {
        return basistime;
    }

    public void setBasistime(Date basistime) {
        this.basistime = basistime;
    }

    public String getAaCateg() {
        return aaCateg;
    }

    public void setAaCateg(String aaCateg) {
        this.aaCateg = aaCateg;
    }

    public String getAaCheck() {
        return aaCheck;
    }

    public void setAaCheck(String aaCheck) {
        this.aaCheck = aaCheck;
    }

    public Double getValue() {
        return value;
    }

    public void setValue(Double value) {
        this.value = value;
    }

    public Double getSupplValue() {
        return supplValue;
    }

    public void setSupplValue(Double supplValue) {
        this.supplValue = supplValue;
    }

    public String getShefQualCode() {
        return shefQualCode;
    }

    public void setShefQualCode(String shefQualCode) {
        this.shefQualCode = shefQualCode;
    }

    public Integer getQualityCode() {
        return qualityCode;
    }

    public void setQualityCode(Integer qualityCode) {
        this.qualityCode = qualityCode;
    }

    public Short getRevision() {
        return revision;
    }

    public void setRevision(Short revision) {
        this.revision = revision;
    }

    public String getProductId() {
        return productId;
    }

    public void setProductId(String productId) {
        this.productId = productId;
    }

    public Date getProducttime() {
        return producttime;
    }

    public void setProducttime(Date producttime) {
        this.producttime = producttime;
    }

    public Date getPostingtime() {
        return postingtime;
    }

    public void setPostingtime(Date postingtime) {
        this.postingtime = postingtime;
    }

    public Date getActionTime() {
        return actionTime;
    }

    public void setActionTime(Date actionTime) {
        this.actionTime = actionTime;
    }
}