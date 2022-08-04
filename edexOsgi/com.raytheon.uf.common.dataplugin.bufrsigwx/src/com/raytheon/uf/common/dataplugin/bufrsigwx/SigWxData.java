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
package com.raytheon.uf.common.dataplugin.bufrsigwx;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.bufrsigwx.common.SigWxLayer;
import com.raytheon.uf.common.dataplugin.bufrsigwx.common.SigWxType;
import com.raytheon.uf.common.dataplugin.bufrsigwx.common.TropopauseLayerData;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Upper air pointdata for significant weather
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2009            jkorman     Initial creation
 * Apr 04, 2013 1846       bkowal      Added an index on refTime and
 *                                     forecastTime
 * Apr 12, 2013 1857       bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                     PluginDataObject.
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Oct 14, 2013 2361       njensen     Removed XML annotations and IDecoderGettable
 * Jul 23, 2015 2360       rferrel     Add name to unique constraint.
 * Feb 04, 2016 5309       tgurney     Drop dataURI column and update unique constraint.
 * Aug 04, 2016 5783       tgurney     Add forecasttime to unique constraint
 * 
 * </pre>
 * 
 * @author jkorman
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "bufrsigwxseq")
@Table(name = "bufrsigwx", uniqueConstraints = { @UniqueConstraint(name = "uk_bufrsigwx_datauri_fields", columnNames = {
        "refTime", "forecastTime", "wxLayer", "wxType", "key" }) })
@DynamicSerialize
public class SigWxData extends PersistablePluginDataObject implements
        IPointData, IPersistable {

    private static final long serialVersionUID = 1L;

    @Column(nullable = false)
    @DataURI(position = 1)
    @DynamicSerializeElement
    private SigWxLayer wxLayer;

    @Column(nullable = false)
    @DataURI(position = 2)
    @DynamicSerializeElement
    private SigWxType wxType;

    @Column(nullable = false)
    @DataURI(position = 3)
    @DynamicSerializeElement
    private Integer key;

    @Column
    @DynamicSerializeElement
    private Integer baseHeight;

    @Column
    @DynamicSerializeElement
    private Integer topHeight;

    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView;

    // Text of the WMO header
    @Column(length = 32)
    @DynamicSerializeElement
    private String wmoHeader;

    @Transient
    @DynamicSerializeElement
    private TropopauseLayerData tropData;

    /**
     * Empty constructor.
     */
    public SigWxData() {
    }

    /**
     * Constructor for DataURI construction through base class. This is used by
     * the notification service.
     * 
     * @param uri
     *            A data uri applicable to this class.
     * @param tableDef
     *            The table definitions for this class.
     */
    public SigWxData(String uri) {
        super(uri);
    }

    /**
     * @return the wxLayer
     */
    public SigWxLayer getWxLayer() {
        return wxLayer;
    }

    /**
     * @param wxLayer
     *            the wxLayer to set
     */
    public void setWxLayer(SigWxLayer wxLayer) {
        this.wxLayer = wxLayer;
    }

    /**
     * @return the wxType
     */
    public SigWxType getWxType() {
        return wxType;
    }

    /**
     * @param wxType
     *            the wxType to set
     */
    public void setWxType(SigWxType wxType) {
        this.wxType = wxType;
    }

    /**
     * @return the baseHeight
     */
    public Integer getBaseHeight() {
        return baseHeight;
    }

    /**
     * @param baseHeight
     *            the baseHeight to set
     */
    public void setBaseHeight(Integer baseHeight) {
        this.baseHeight = baseHeight;
    }

    /**
     * @return the topHeight
     */
    public Integer getTopHeight() {
        return topHeight;
    }

    /**
     * @param topHeight
     *            the topHeight to set
     */
    public void setTopHeight(Integer topHeight) {
        this.topHeight = topHeight;
    }

    /**
     * @return the wmoHeader
     */
    public String getWmoHeader() {
        return wmoHeader;
    }

    /**
     * @param wmoHeader
     *            the wmoHeader to set
     */
    public void setWmoHeader(String wmoHeader) {
        this.wmoHeader = wmoHeader;
    }

    /**
     * @return the tropData
     */
    public TropopauseLayerData getTropData() {
        return tropData;
    }

    /**
     * @param tropData
     *            the tropData to set
     */
    public void setTropData(TropopauseLayerData tropData) {
        this.tropData = tropData;
    }

    /**
     * @return the key
     */
    public Integer getKey() {
        return key;
    }

    /**
     * @param key
     *            the key to set
     */
    public void setKey(Integer key) {
        this.key = key;
    }

    /**
     * 
     */
    @Override
    public PointDataView getPointDataView() {
        return pointDataView;
    }

    /**
     * 
     */
    @Override
    public void setPointDataView(PointDataView pointDataView) {
        this.pointDataView = pointDataView;
    }

    /**
     * 
     * @return
     */
    public final SigWxData copyObs() {
        SigWxData obs = new SigWxData();

        obs.dataTime = dataTime.clone();

        obs.baseHeight = baseHeight;
        obs.topHeight = topHeight;
        obs.wxLayer = wxLayer;
        obs.wxType = wxType;
        obs.wmoHeader = wmoHeader;

        return obs;
    }

    @Override
    public String getPluginName() {
        return "bufrsigwx";
    }
}
