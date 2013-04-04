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
package com.raytheon.uf.common.dataplugin.grid;

import java.util.HashMap;
import java.util.Map;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.persist.IHDFFilePathProvider;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Record for storing generic gridded data. Everything interesting is stored in
 * the info object since it allows us to reuse the info object for identical
 * records at different times which saves db and improves theoretical
 * performance.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 21, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@Entity
@Table(name = "grid", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@DynamicSerialize
public class GridRecord extends PersistablePluginDataObject implements
        ISpatialEnabled {

    private static final long serialVersionUID = 1L;

    @ManyToOne
    @PrimaryKeyJoinColumn
    @DataURI(position = 1, embedded = true)
    @DynamicSerializeElement
    private GridInfoRecord info;

    /**
     * Holds any extra attributes which may be specific to this data. These are
     * not stored to the database or to hdf5 but they can be useful for passing
     * additional parameters from a decoder to a post processor.
     */
    @Transient
    private Map<String, Object> extraAttributes;

    public GridRecord() {
        super();
    }

    public GridRecord(GridRecord record) {
        this.pluginName = record.getPluginName();
        this.dataTime = record.getDataTime();
        this.info = new GridInfoRecord(record.getInfoNotNull());
        if (record.getExtraAttributes() != null) {
            this.extraAttributes = new HashMap<String, Object>(
                    record.getExtraAttributes());
        }
    }

    public GridRecord(String uri) {
        super(uri);
    }

    public GridInfoRecord getInfo() {

        return info;
    }

    public void setInfo(GridInfoRecord info) {
        this.info = info;
    }

    protected GridInfoRecord getInfoNotNull() {
        if (info == null) {
            info = new GridInfoRecord();
        }
        return info;
    }

    public String getDatasetId() {
        return getInfoNotNull().getDatasetId();
    }

    public void setDatasetId(String datasetId) {
        getInfoNotNull().setDatasetId(datasetId);
    }

    public String getSecondaryId() {
        return getInfoNotNull().getSecondaryId();
    }

    public void setSecondaryId(String secondaryId) {
        getInfoNotNull().setSecondaryId(secondaryId);
    }

    public GridCoverage getLocation() {
        return getInfoNotNull().getLocation();
    }

    public void setLocation(GridCoverage location) {
        getInfoNotNull().setLocation(location);
    }

    public Parameter getParameter() {
        return getInfoNotNull().getParameter();
    }

    public void setParameter(Parameter parameter) {
        getInfoNotNull().setParameter(parameter);
    }

    public Level getLevel() {
        return getInfoNotNull().getLevel();
    }

    public void setLevel(Level level) {
        getInfoNotNull().setLevel(level);
    }

    public String getEnsembleId() {
        return getInfoNotNull().getEnsembleId();
    }

    public void setEnsembleId(String ensembleId) {
        getInfoNotNull().setEnsembleId(ensembleId);
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }

    @Override
    public ISpatialObject getSpatialObject() {
        return getLocation();
    }

    @Override
    public IHDFFilePathProvider getHDFPathProvider() {
        return GridPathProvider.getInstance();
    }

    public Map<String, Object> getExtraAttributes() {
        return extraAttributes;
    }

    public void setExtraAttributes(Map<String, Object> extraAttributes) {
        this.extraAttributes = extraAttributes;
    }

    /**
     * Add an attribute to the data, if there are no attributes then the
     * attribute map is created.
     * 
     * @param key
     * @param value
     * @return the previous value of the attribute or null if there was none.
     */
    public Object addExtraAttribute(String key, Object value) {
        if (extraAttributes == null) {
            extraAttributes = new HashMap<String, Object>();
        }
        return extraAttributes.put(key, value);
    }

    public Object getExtraAttribute(String key) {
        if (extraAttributes == null) {
            return null;
        }
        return extraAttributes.get(key);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((info == null) ? 0 : info.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        GridRecord other = (GridRecord) obj;
        if (info == null) {
            if (other.info != null)
                return false;
        } else if (!info.equals(other.info))
            return false;
        return true;
    }

}
