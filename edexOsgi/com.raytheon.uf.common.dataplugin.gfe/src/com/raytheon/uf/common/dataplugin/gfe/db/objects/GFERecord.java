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
/**
 * 
 */
package com.raytheon.uf.common.dataplugin.gfe.db.objects;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OrderBy;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.util.GfeUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Record implementation for GFE plugin.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * --------            ---  randerso    Initial creation    
 * 20070914            379  jkorman     Added populateDataStore() and
 *                                      getPersistenceTime() from new IPersistable
 * 20071129            472  jkorman     Added IDecoderGettable interface.  
 * 06/17/08    #940         bphillip    Implemented GFE Locking  
 * </pre>
 * 
 * @author randerso
 * @version 1
 */
/**
 * 
 */
@Entity
@Table(name = "gfe", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@Cache(usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class GFERecord extends PluginDataObject {

    private static final long serialVersionUID = 1L;

    /** Grid type enumeration */
    public enum GridType {
        NONE, SCALAR, VECTOR, WEATHER, DISCRETE
    };

    /** The name of the parm parameter */
    @Column(length = 100)
    @XmlAttribute
    @DynamicSerializeElement
    private String parmName;

    /** The name of the parm level */
    @Column(length = 8)
    @XmlAttribute
    @DynamicSerializeElement
    private String parmLevel;

    /**
     * The parmID of the associated parm.<br>
     * This field is constructed when the getter is called.<br>
     * It is constructed from the parmName, parmLevel, and the databaseID
     */
    @DataURI(position = 1)
    @Column
    @Type(type = "com.raytheon.uf.common.dataplugin.gfe.db.type.ParmIdType")
    @XmlElement
    @DynamicSerializeElement
    private ParmID parmId;

    /**
     * The database associated with this record
     */
    @DataURI(position = 2)
    @Column
    @Type(type = "com.raytheon.uf.common.dataplugin.gfe.db.type.DatabaseIdType")
    @XmlElement
    @DynamicSerializeElement
    private DatabaseID dbId;

    /** The grid parm information associated with this parameter */
    @Transient
    private GridParmInfo gridInfo;

    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER, orphanRemoval = true)
    @JoinColumn(name = "parent", nullable = false)
    @Index(name = "gfe_gridhistory_history_idx")
    @OrderBy("key")
    @XmlElement
    @DynamicSerializeElement
    private List<GridDataHistory> gridHistory = new ArrayList<GridDataHistory>(
            0);

    /**
     * Creates a new empty GFERecord. Must use setters to fill in private fields
     * or the object is invalid.
     */
    public GFERecord() {

    }

    /**
     * Creates a new GFERecord
     * 
     * @param parmId
     *            The parm ID
     * @param parmInfo
     *            The parm Info
     */
    public GFERecord(ParmID parmId, TimeRange timeRange) {
        this.pluginName = "gfe";
        Calendar cal = (Calendar) Calendar.getInstance(
                TimeZone.getTimeZone("GMT")).clone();
        cal.setTime(timeRange.getStart());
        this.dataTime = new DataTime(cal, timeRange);
        this.parmId = parmId;
        this.parmName = parmId.getParmName();
        this.parmLevel = parmId.getParmLevel();
        this.dbId = parmId.getDbId();
    }

    public void addHistoryEntry(GridDataHistory historyEntry) {
        gridHistory.add(historyEntry);
    }

    /**
     * @return the parmId
     */
    public ParmID getParmId() {
        return parmId;
    }

    /**
     * @param parmId
     *            the parmId to set
     */
    public void setParmId(ParmID parmId) {
        this.parmId = parmId;
    }

    /**
     * Creates a new GFERecord from a data URI and the tableDefinition
     * 
     * @param uri
     *            The dataURI if the GFERecord
     * @param tableDef
     *            The GFE table definition
     */
    public GFERecord(String uri) {
        super(uri);
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }

    public void setGridHistory(GridDataHistory[] history) {
        setGridHistory(Arrays.asList(history));
    }

    public DatabaseID getDbId() {
        return dbId;
    }

    public void setDbId(DatabaseID dbId) {
        this.dbId = dbId;
    }

    public String getParmName() {
        return parmName;
    }

    public void setParmName(String parmName) {
        this.parmName = parmName;
    }

    public String getParmLevel() {
        return parmLevel;
    }

    public void setParmLevel(String parmLevel) {
        this.parmLevel = parmLevel;
    }

    public GridParmInfo getGridInfo() {
        return gridInfo;
    }

    public void setGridInfo(GridParmInfo gridInfo) {
        this.gridInfo = gridInfo;
    }

    public TimeRange getTimeRange() {
        return this.dataTime.getValidPeriod();
    }

    public String getGridHistoryStrings() {
        return GfeUtil.getHistoryStrings(this.gridHistory);
    }

    /**
     * @return the gridHistory
     */
    public List<GridDataHistory> getGridHistory() {
        return gridHistory;
    }

    /**
     * @param gridHistory
     *            the gridHistory to set
     */
    public void setGridHistory(List<GridDataHistory> gridHistory) {
        this.gridHistory = gridHistory;
    }

}
