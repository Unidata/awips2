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
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OrderBy;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.Index;
import org.hibernate.annotations.OnDelete;
import org.hibernate.annotations.OnDeleteAction;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Record implementation for GFE plugin. Record is essentially read only and
 * should never need to be updated. GridDataHistory referenced by record may
 * update.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 *                          randerso    Initial creation
 * Sep 14, 2007 379         jkorman     Added populateDataStore() and
 *                                      getPersistenceTime() from new
 *                                      IPersistable
 * Nov 29, 2007 472         jkorman     Added IDecoderGettable interface.
 * Jun 17, 2008 940         bphillip    Implemented GFE Locking
 * Apr 04, 2013 1846        bkowal      Added an index on refTime and
 *                                      forecastTime
 * Apr 12, 2013 1857        bgonzale    Added SequenceGenerator annotation.
 * Apr 23, 2013 1949        rjpeter     Normalized database structure.
 * May 07, 2013 1869        bsteffen    Remove dataURI column from
 *                                      PluginDataObject.
 * May 13, 2013 1869        bsteffen    Remove DataURI column from GFE.
 * Jun 20, 2013 2127        rjpeter     Added OnDelete annotation.
 * Aug 30, 2013 2298        rjpeter     Make getPluginName abstract
 * Sep 20, 2013 2147        rferrel     Changes to archive hdf5 files.
 * Dec 03, 2013 2597        randerso    Cleared gridHistory id when adding new history
 *                                      records in consolidateHistory so dao will recognize
 *                                      it as a new record
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "gfeseq")
@Table(name = GFERecord.PLUGIN_NAME, uniqueConstraints = { @UniqueConstraint(columnNames = {
        "parmId_id", "rangestart", "rangeend", "refTime", "forecasttime" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = GFERecord.PLUGIN_NAME, indexes = { @Index(name = "gfe_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@DynamicSerialize
@BatchSize(size = 500)
public class GFERecord extends PluginDataObject implements IPersistable {

    private static final long serialVersionUID = 1L;

    /** Grid type enumeration */
    public enum GridType {
        NONE, SCALAR, VECTOR, WEATHER, DISCRETE
    };

    public static final String PLUGIN_NAME = "gfe";

    /**
     * The parmID of the associated parm.<br>
     */
    @DataURI(position = 1, embedded = true)
    @ManyToOne(fetch = FetchType.EAGER, optional = false)
    @OnDelete(action = OnDeleteAction.CASCADE)
    @PrimaryKeyJoinColumn
    @DynamicSerializeElement
    private ParmID parmId;

    /** The grid parm information associated with this parameter */
    @Transient
    private GridParmInfo gridInfo;

    /**
     * GridHistories for this record. Only cascade on remove. Insert/Update
     * managed independently.
     */
    @OneToMany(fetch = FetchType.EAGER, mappedBy = "parent")
    @BatchSize(size = 500)
    @OnDelete(action = OnDeleteAction.CASCADE)
    @OrderBy("id")
    @DynamicSerializeElement
    private List<GridDataHistory> gridHistory = null;

    /**
     * Histories to remove when updated
     */
    @Transient
    private List<GridDataHistory> oldHistory = null;

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
        Calendar cal = (Calendar) Calendar.getInstance(
                TimeZone.getTimeZone("GMT")).clone();
        cal.setTime(timeRange.getStart());
        this.dataTime = new DataTime(cal, timeRange);
        this.parmId = parmId;
    }

    public void addHistoryEntry(GridDataHistory historyEntry) {
        if (gridHistory == null) {
            gridHistory = new ArrayList<GridDataHistory>(1);
        }
        gridHistory.add(historyEntry);
        historyEntry.setParent(this);
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

    public void setGridHistory(GridDataHistory[] history) {
        setGridHistory(Arrays.asList(history));
    }

    public DatabaseID getDbId() {
        return parmId.getDbId();
    }

    public String getParmName() {
        return parmId.getParmName();
    }

    public String getParmLevel() {
        return parmId.getParmLevel();
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

    /**
     * @return the gridHistory
     */
    public List<GridDataHistory> getGridHistory() {
        if (gridHistory == null) {
            gridHistory = new ArrayList<GridDataHistory>(0);
        }

        return gridHistory;
    }

    /**
     * @param gridHistory
     *            the gridHistory to set
     */
    public void setGridHistory(List<GridDataHistory> gridHistory) {
        this.gridHistory = gridHistory;
        if (gridHistory != null) {
            for (GridDataHistory hist : gridHistory) {
                hist.setParent(this);
            }
        }
    }

    /**
     * @return the oldHistory
     */
    public List<GridDataHistory> getOldHistory() {
        return oldHistory;
    }

    public void consolidateHistory(List<GridDataHistory> newHistory) {
        for (int i = 0; i < newHistory.size(); i++) {
            if (i < gridHistory.size()) {
                gridHistory.get(i).replaceValues(newHistory.get(i));
            } else {
                GridDataHistory hist = newHistory.get(i);
                hist.setParent(this);
                hist.setId(0);
                gridHistory.add(hist);
            }
        }

        if (gridHistory.size() > newHistory.size()) {
            if (oldHistory == null) {
                oldHistory = new ArrayList<GridDataHistory>(
                        gridHistory.subList(newHistory.size(),
                                gridHistory.size()));
            }

            for (int i = newHistory.size(); i < gridHistory.size(); i++) {
                gridHistory.remove(i);
            }
        }
    }

    @Override
    public String getPluginName() {
        return PLUGIN_NAME;
    }

    @Override
    public Date getPersistenceTime() {
        return getInsertTime().getTime();
    }

    @Override
    public void setPersistenceTime(Date persistTime) {
        Calendar pTime = Calendar.getInstance();
        pTime.setTime(persistTime);
        setInsertTime(pTime);
    }
}
