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
package com.raytheon.uf.common.dataplugin.shef.tables;

// Generated Oct 17, 2008 2:22:17 PM by Hibernate Tools 3.2.2.GA

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Entity for the ihfs rwbiasstat table.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 17, 2008                        Initial generation by hbm2java
 * Aug 19, 2011      10672     jkorman Move refactor to new project
 * Oct 07, 2013       2361     njensen Removed XML annotations
 * May 13, 2016       5576     bkowal  Cleanup.
 * May 20, 2016       5576     bkowal  Implemented {@link #toString()}.
 * 
 * </pre>
 * 
 * @author jkorman
 */
@Entity
@Table(name = "rwbiasstat")
@DynamicSerialize
public class Rwbiasstat extends PersistableDataObject<String> implements
        Serializable {

    private static final long serialVersionUID = 1L;

    @DynamicSerializeElement
    private String officeId;

    @DynamicSerializeElement
    private Float minGrValueBias;

    @DynamicSerializeElement
    private Integer npairBiasSelect;

    @DynamicSerializeElement
    private Integer npairSvarUpdate;

    @DynamicSerializeElement
    private Integer stdCut;

    @DynamicSerializeElement
    private Integer lagCut;

    @DynamicSerializeElement
    private Integer initSpan;

    @DynamicSerializeElement
    private Integer biasQcOpt;

    @DynamicSerializeElement
    private Integer numSpan;

    @DynamicSerializeElement
    private Float memSpan1;

    @DynamicSerializeElement
    private Float memSpan2;

    @DynamicSerializeElement
    private Float memSpan3;

    @DynamicSerializeElement
    private Float memSpan4;

    @DynamicSerializeElement
    private Float memSpan5;

    @DynamicSerializeElement
    private Float memSpan6;

    @DynamicSerializeElement
    private Float memSpan7;

    @DynamicSerializeElement
    private Float memSpan8;

    @DynamicSerializeElement
    private Float memSpan9;

    @DynamicSerializeElement
    private Float memSpan10;

    public Rwbiasstat() {
    }

    public Rwbiasstat(String officeId) {
        this.officeId = officeId;
    }

    public Rwbiasstat(String officeId, Float minGrValueBias,
            Integer npairBiasSelect, Integer npairSvarUpdate, Integer stdCut,
            Integer lagCut, Integer initSpan, Integer biasQcOpt,
            Integer numSpan, Float memSpan1, Float memSpan2, Float memSpan3,
            Float memSpan4, Float memSpan5, Float memSpan6, Float memSpan7,
            Float memSpan8, Float memSpan9, Float memSpan10) {
        this.officeId = officeId;
        this.minGrValueBias = minGrValueBias;
        this.npairBiasSelect = npairBiasSelect;
        this.npairSvarUpdate = npairSvarUpdate;
        this.stdCut = stdCut;
        this.lagCut = lagCut;
        this.initSpan = initSpan;
        this.biasQcOpt = biasQcOpt;
        this.numSpan = numSpan;
        this.memSpan1 = memSpan1;
        this.memSpan2 = memSpan2;
        this.memSpan3 = memSpan3;
        this.memSpan4 = memSpan4;
        this.memSpan5 = memSpan5;
        this.memSpan6 = memSpan6;
        this.memSpan7 = memSpan7;
        this.memSpan8 = memSpan8;
        this.memSpan9 = memSpan9;
        this.memSpan10 = memSpan10;
    }

    @Id
    @Column(name = "office_id", unique = true, nullable = false, length = 5)
    public String getOfficeId() {
        return this.officeId;
    }

    public void setOfficeId(String officeId) {
        this.officeId = officeId;
    }

    @Column(name = "min_gr_value_bias", precision = 8, scale = 8)
    public Float getMinGrValueBias() {
        return this.minGrValueBias;
    }

    public void setMinGrValueBias(Float minGrValueBias) {
        this.minGrValueBias = minGrValueBias;
    }

    @Column(name = "npair_bias_select")
    public Integer getNpairBiasSelect() {
        return this.npairBiasSelect;
    }

    public void setNpairBiasSelect(Integer npairBiasSelect) {
        this.npairBiasSelect = npairBiasSelect;
    }

    @Column(name = "npair_svar_update")
    public Integer getNpairSvarUpdate() {
        return this.npairSvarUpdate;
    }

    public void setNpairSvarUpdate(Integer npairSvarUpdate) {
        this.npairSvarUpdate = npairSvarUpdate;
    }

    @Column(name = "std_cut")
    public Integer getStdCut() {
        return this.stdCut;
    }

    public void setStdCut(Integer stdCut) {
        this.stdCut = stdCut;
    }

    @Column(name = "lag_cut")
    public Integer getLagCut() {
        return this.lagCut;
    }

    public void setLagCut(Integer lagCut) {
        this.lagCut = lagCut;
    }

    @Column(name = "init_span")
    public Integer getInitSpan() {
        return this.initSpan;
    }

    public void setInitSpan(Integer initSpan) {
        this.initSpan = initSpan;
    }

    @Column(name = "bias_qc_opt")
    public Integer getBiasQcOpt() {
        return this.biasQcOpt;
    }

    public void setBiasQcOpt(Integer biasQcOpt) {
        this.biasQcOpt = biasQcOpt;
    }

    @Column(name = "num_span")
    public Integer getNumSpan() {
        return this.numSpan;
    }

    public void setNumSpan(Integer numSpan) {
        this.numSpan = numSpan;
    }

    @Column(name = "mem_span1", precision = 8, scale = 8)
    public Float getMemSpan1() {
        return this.memSpan1;
    }

    public void setMemSpan1(Float memSpan1) {
        this.memSpan1 = memSpan1;
    }

    @Column(name = "mem_span2", precision = 8, scale = 8)
    public Float getMemSpan2() {
        return this.memSpan2;
    }

    public void setMemSpan2(Float memSpan2) {
        this.memSpan2 = memSpan2;
    }

    @Column(name = "mem_span3", precision = 8, scale = 8)
    public Float getMemSpan3() {
        return this.memSpan3;
    }

    public void setMemSpan3(Float memSpan3) {
        this.memSpan3 = memSpan3;
    }

    @Column(name = "mem_span4", precision = 8, scale = 8)
    public Float getMemSpan4() {
        return this.memSpan4;
    }

    public void setMemSpan4(Float memSpan4) {
        this.memSpan4 = memSpan4;
    }

    @Column(name = "mem_span5", precision = 8, scale = 8)
    public Float getMemSpan5() {
        return this.memSpan5;
    }

    public void setMemSpan5(Float memSpan5) {
        this.memSpan5 = memSpan5;
    }

    @Column(name = "mem_span6", precision = 8, scale = 8)
    public Float getMemSpan6() {
        return this.memSpan6;
    }

    public void setMemSpan6(Float memSpan6) {
        this.memSpan6 = memSpan6;
    }

    @Column(name = "mem_span7", precision = 8, scale = 8)
    public Float getMemSpan7() {
        return this.memSpan7;
    }

    public void setMemSpan7(Float memSpan7) {
        this.memSpan7 = memSpan7;
    }

    @Column(name = "mem_span8", precision = 8, scale = 8)
    public Float getMemSpan8() {
        return this.memSpan8;
    }

    public void setMemSpan8(Float memSpan8) {
        this.memSpan8 = memSpan8;
    }

    @Column(name = "mem_span9", precision = 8, scale = 8)
    public Float getMemSpan9() {
        return this.memSpan9;
    }

    public void setMemSpan9(Float memSpan9) {
        this.memSpan9 = memSpan9;
    }

    @Column(name = "mem_span10", precision = 8, scale = 8)
    public Float getMemSpan10() {
        return this.memSpan10;
    }

    public void setMemSpan10(Float memSpan10) {
        this.memSpan10 = memSpan10;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("Rwbiasstat [");
        sb.append("officeId=").append(officeId);
        sb.append(", minGrValueBias=").append(minGrValueBias);
        sb.append(", npairBiasSelect=").append(npairBiasSelect);
        sb.append(", npairSvarUpdate=").append(npairSvarUpdate);
        sb.append(", stdCut=").append(stdCut);
        sb.append(", lagCut=").append(lagCut);
        sb.append(", initSpan=").append(initSpan);
        sb.append(", biasQcOpt=").append(biasQcOpt);
        sb.append(", numSpan=").append(numSpan);
        sb.append(", memSpan1=").append(memSpan1);
        sb.append(", memSpan2=").append(memSpan2);
        sb.append(", memSpan3=").append(memSpan3);
        sb.append(", memSpan4=").append(memSpan4);
        sb.append(", memSpan5=").append(memSpan5);
        sb.append(", memSpan6=").append(memSpan6);
        sb.append(", memSpan7=").append(memSpan7);
        sb.append(", memSpan8=").append(memSpan8);
        sb.append(", memSpan9=").append(memSpan9);
        sb.append(", memSpan10=").append(memSpan10);
        sb.append("]");
        return sb.toString();
    }
}
