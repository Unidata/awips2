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
package com.raytheon.uf.edex.netcdf.decoder;

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.apache.commons.beanutils.BeanMap;

import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;

/**
 * The info about a single record from a Netcdf file.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 9, 2016  5584       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 */

public class NetcdfRecordInfo {
    private BeanMap beanMap;

    /** Map of dimension names and this record's indices. */
    private Map<String, Integer> dimensionIndexMap;

    /** Map of field descriptions that are deferred until post processing. */
    private Map<String, Object> deferredDescriptions;

    /** Set of data dimension indices that need to be flipped when read. */
    private Set<Integer> flipDimensionIndices;

    /** The file on disk this record was generated from. */
    private File file;

    /**
     * Constructor.
     */
    public NetcdfRecordInfo(PersistablePluginDataObject record) {
        super();
        this.beanMap = new BeanMap(record);
        this.dimensionIndexMap = new HashMap<>();
        this.deferredDescriptions = new HashMap<>();
        this.flipDimensionIndices = new HashSet<>();
    }

    /**
     * Constructor.
     */
    public NetcdfRecordInfo(NetcdfRecordInfo record, Set<String> excludedFields) {
        super();

        Object bean = record.getBeanMap().getBean();
        try {
            this.beanMap = new BeanMap(bean.getClass().newInstance());
        } catch (InstantiationException | IllegalAccessException e) {
            throw new IllegalArgumentException(
                    "Unable to instantiate a new record of type "
                            + bean.getClass().getCanonicalName(), e);
        }
        this.dimensionIndexMap = new HashMap<>(record.getDimensionIndexMap());
        this.deferredDescriptions = new HashMap<>(
                record.getDeferredDescriptions());
        this.flipDimensionIndices = new HashSet<>(
                record.getFlipDimensionIndices());
        this.file = record.file;

        copyFields(record, excludedFields);
    }

    /**
     * Copies non-null writable fields from the source beanmap to this beanmap.
     * Certain fields are excluded due to their problematic nature.
     *
     * @param src
     *            The source record info.
     */
    private void copyFields(NetcdfRecordInfo src, Set<String> excludedFields) {
        Iterator<String> keyIter = src.beanMap.keyIterator();
        Object val;
        while (keyIter.hasNext()) {
            String key = keyIter.next();
            if (this.beanMap.getWriteMethod(key) != null
                    && !excludedFields.contains(key)) {
                val = src.beanMap.get(key);
                if (val != null) {
                    this.beanMap.put(key, val);
                }
            }
        }
    }

    /**
     * Convenience method for addField(fieldName, fieldValue, false). This
     * should not be called when inserting a value from a deferred description.
     *
     * @see NetcdfRecordInfo#addField(String, Object, boolean)
     */
    public void addField(String fieldName, Object fieldValue) {
        addField(fieldName, fieldValue, false);
    }

    /**
     * Inserts the value into the bean map, unless the bean map or the deferred
     * descriptions already contains that field and forceAdd is false. This must
     * be called with forceAdd = true when inserting values from a deferred
     * description.
     * 
     * @param fieldName
     *            The name of the field.
     * @param fieldValue
     *            The value of the field.
     * @param forceAdd
     *            If true, the value will be inserted. Otherwise, the value will
     *            only be inserted if both the internal beanmap and deferred
     *            descriptions map do not contain an entry with that field name.
     */
    public void addField(String fieldName, Object fieldValue, boolean forceAdd) {
        if (forceAdd
                || (this.beanMap.containsKey(fieldName)
                        && this.beanMap.get(fieldName) == null && !this.deferredDescriptions
                            .containsKey(fieldName))) {
            this.beanMap.put(fieldName, fieldValue);
        }
    }

    public Object getField(String fieldName) {
        return this.beanMap.get(fieldName);
    }

    /**
     * @return the dimensionIndexMap
     */
    public Map<String, Integer> getDimensionIndexMap() {
        return dimensionIndexMap;
    }

    /**
     * @param dimensionIndexMap
     *            the dimensionIndexMap to set
     */
    public void setDimensionIndexMap(Map<String, Integer> dimensionIndexMap) {
        this.dimensionIndexMap = dimensionIndexMap;
    }

    public void addDimensionIndex(String dimName, Integer dimIndex) {
        this.dimensionIndexMap.put(dimName, dimIndex);
    }

    /**
     * @return the deferredDescriptions
     */
    public Map<String, Object> getDeferredDescriptions() {
        return deferredDescriptions;
    }

    /**
     * @param deferredDescriptions
     *            the deferredDescriptions to set
     */
    public void setDeferredDescriptions(Map<String, Object> deferredDescriptions) {
        this.deferredDescriptions = deferredDescriptions;
    }

    /**
     * Add the deferred description if this info does not already contain a
     * deferred description for the given key.
     *
     * @param key
     * @param description
     */
    public void addDeferredDescription(String key, Object description) {
        if (!this.deferredDescriptions.containsKey(key)) {
            this.deferredDescriptions.put(key, description);
        }
    }

    public Object getDeferredDescription(String key) {
        return this.deferredDescriptions.get(key);
    }

    /**
     * @return the flipDimensionIndices
     */
    public Set<Integer> getFlipDimensionIndices() {
        return flipDimensionIndices;
    }

    /**
     * @param flipDimensionIndices
     *            the flipDimensionIndices to set
     */
    public void setFlipDimensionIndices(Set<Integer> flipDimensionIndices) {
        this.flipDimensionIndices = flipDimensionIndices;
    }

    public void addFlipDimensionIndex(int index) {
        this.flipDimensionIndices.add(Integer.valueOf(index));
    }

    /**
     * @return the file
     */
    public File getFile() {
        return file;
    }

    /**
     * @param file
     *            the file to set
     */
    public void setFile(File file) {
        this.file = file;
    }

    /**
     * @return the beanMap
     */
    public BeanMap getBeanMap() {
        return beanMap;
    }

    /**
     * @param beanMap
     *            the beanMap to set
     */
    public void setBeanMap(BeanMap beanMap) {
        this.beanMap = beanMap;
    }
}
