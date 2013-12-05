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
package com.raytheon.uf.common.pointdata;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.LongDataRecord;
import com.raytheon.uf.common.datastorage.records.StringDataRecord;
import com.raytheon.uf.common.pointdata.PointDataDescription.Type;
import com.raytheon.uf.common.pointdata.PointDataView.Mode;
import com.raytheon.uf.common.pointdata.elements.AbstractPointDataObject;
import com.raytheon.uf.common.pointdata.elements.FloatPointDataObject;
import com.raytheon.uf.common.pointdata.elements.IntPointDataObject;
import com.raytheon.uf.common.pointdata.elements.LongPointDataObject;
import com.raytheon.uf.common.pointdata.elements.StringPointDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * The base PointData object, containing a set of parameters for a set of
 * observations/forecasts.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 08, 2009           chammack    Initial creation
 * Dec 02, 2013  2537     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@DynamicSerialize
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class PointDataContainer {

    protected static final int DEFAULT_SZ = 2048;

    @DynamicSerializeElement
    @XmlAttribute
    protected int currentSz;

    @DynamicSerializeElement
    @XmlAttribute
    protected int allocatedSz;

    @DynamicSerializeElement
    @XmlElement
    @XmlJavaTypeAdapter(value = PointDataMarshaller.class)
    protected HashMap<String, AbstractPointDataObject<?>> pointDataTypes;

    public PointDataContainer() {
        this(0);
    }

    PointDataContainer(int sz) {
        this.currentSz = 0;
        this.allocatedSz = sz;
    }

    public static PointDataContainer build(List<IDataRecord> recs) {
        return build(recs.toArray(new IDataRecord[recs.size()]));
    }

    public static PointDataContainer build(IDataRecord[] recs) {
        int sz = 0;
        if (recs.length > 0) {
            if (recs[0].getDimension() == 1) {
                sz = (int) recs[0].getSizes()[0];
            } else {
                sz = (int) recs[0].getSizes()[1];
            }
        }

        PointDataContainer container = new PointDataContainer(sz);
        container.pointDataTypes = new HashMap<String, AbstractPointDataObject<?>>(
                recs.length);

        for (IDataRecord rec : recs) {
            addPointData(rec, container);
        }
        return container;

    }

    private static void addPointData(IDataRecord rec,
            PointDataContainer container) {
        AbstractPointDataObject<?> pdo = null;
        // Done using this way rather than instanceof for speed
        if (rec.getClass().equals(FloatDataRecord.class)) {
            pdo = new FloatPointDataObject(container, (FloatDataRecord) rec);
            pdo.getDescription().setType(Type.FLOAT);
        } else if (rec.getClass().equals(IntegerDataRecord.class)) {
            pdo = new IntPointDataObject(container, (IntegerDataRecord) rec);
            pdo.getDescription().setType(Type.INT);
        } else if (rec.getClass().equals(LongDataRecord.class)) {
            pdo = new LongPointDataObject(container, (LongDataRecord) rec);
            pdo.getDescription().setType(Type.LONG);
        } else if (rec.getClass().equals(StringDataRecord.class)) {
            pdo = new StringPointDataObject(container, (StringDataRecord) rec);
            pdo.getDescription().setType(Type.STRING);
        }

        if (pdo != null) {
            container.pointDataTypes.put(rec.getName(), pdo);
        }
    }

    /**
     * 
     * @param description
     * @param size
     * @return
     */
    public static PointDataContainer build(PointDataDescription description,
            int size) {
        PointDataContainer container = new PointDataContainer(size);
        container.pointDataTypes = new HashMap<String, AbstractPointDataObject<?>>(
                description.parameters.length);
        for (ParameterDescription p : description.parameters) {

            AbstractPointDataObject<?> pdo = null;
            switch (p.getType()) {
            case INT:
                pdo = new IntPointDataObject(container, p, p.getNumDims());
                break;
            case FLOAT:
                pdo = new FloatPointDataObject(container, p, p.getNumDims());
                break;
            case LONG:
                pdo = new LongPointDataObject(container, p, p.getNumDims());
                break;
            case STRING:
                pdo = new StringPointDataObject(container, p, p.getNumDims());
                break;
            }
            if (pdo != null) {
                if (pdo.getDimensions() > 1) {
                    pdo.resize(size * pdo.getDescription().getDimensionAsInt());
                } else {
                    pdo.resize(size);
                }
                container.pointDataTypes.put(p.getParameterName(), pdo);
            }
        }
        return container;
    }

    /**
     * 
     * @param description
     * @return
     */
    public static PointDataContainer build(PointDataDescription description) {
        return build(description, DEFAULT_SZ);
    }

    /**
     * Builds a point data container from the provided records. The
     * PointDataDescription is pared down to include only the description
     * information for the provided records
     * 
     * @param description
     *            The PointDataDescription for the data
     * @param records
     *            The records to add to the container
     * @return The PointDataContainer
     */
    public static PointDataContainer build(PointDataDescription description,
            List<IDataRecord> records) {
        // Build the initial container
        int sz = 0;
        if (records.size() > 0) {
            if (records.get(0).getDimension() == 1) {
                sz = (int) records.get(0).getSizes()[0];
            } else {
                sz = (int) records.get(0).getSizes()[1];
            }
        }
        PointDataContainer container = build(description);
        container.currentSz = container.allocatedSz = sz; // TODO: seems wrong,
                                                          // but will fail on
                                                          // CAVE side
                                                          // ortherwise. init
                                                          // with size is just a
                                                          // useless allocation
        Set<String> paramNames = new HashSet<String>();

        // Add the records to the container and construct a list of the
        // requested parameters
        for (IDataRecord rec : records) {
            ParameterDescription origDesc = container.getParamSafe(
                    rec.getName()).getDescription();
            paramNames.add(rec.getName());
            addPointData(rec, container);
            container.getParamSafe(rec.getName()).getDescription()
                    .setUnit(origDesc.getUnit());
        }
        Set<String> keys = container.pointDataTypes.keySet();
        List<String> keysToRemove = new ArrayList<String>();

        // Iterate through the keys in the description and remove the unwanted
        // parameter descriptions
        for (String key : keys) {
            if (!paramNames.contains(key)) {
                keysToRemove.add(key);
            }
        }
        for (String key : keysToRemove) {
            container.pointDataTypes.remove(key);
        }
        return container;
    }

    /**
     * Adds a IDataRecord object to this container.
     * 
     * @param record
     *            The IDataRecord object to add
     * @param unit
     *            The units for that record
     */
    public void add(IDataRecord record, String unit) {
        addPointData(record, this);
        AbstractPointDataObject<?> pdo = this.pointDataTypes.get(record
                .getName());
        pdo.getDescription().setUnit(unit);
    }

    /**
     * Removes the specified AbstractPointDataObject from the container.
     * 
     * @param recordName
     *            The name of the point data object to remove
     */
    public void remove(String recordName) {
        this.pointDataTypes.remove(recordName);
    }

    /**
     * @return the currentSz
     */
    public int getCurrentSz() {
        return currentSz;
    }

    public Set<String> getParameters() {
        return this.pointDataTypes.keySet();
    }

    public PointDataView readRandom(int idx) {
        PointDataView pdv = new PointDataView();
        pdv.mode = Mode.READ;
        pdv.container = this;
        readRandom(idx, pdv);
        return pdv;
    }

    public IDataRecord getParameterRecord(String param) {
        AbstractPointDataObject<?> pdo = this.pointDataTypes.get(param);
        if (pdo == null) {
            return null;
        }

        if (pdo.getContainer() == null) {
            pdo.setContainer(this);
        }

        return pdo.getRecord();
    }

    protected void readRandom(int idx, PointDataView pdv) {
        if (pdv.mode != Mode.READ) {
            throw new IllegalStateException(
                    "Passed in a view that was not in read mode");
        }

        if (idx >= currentSz) {
            throw new IndexOutOfBoundsException("Current size is: " + currentSz);
        }

        pdv.curIdx = idx;
    }

    public PointDataView append() {
        PointDataView pdv = new PointDataView();
        pdv.mode = Mode.APPEND;
        pdv.container = this;
        append(pdv);
        return pdv;
    }

    protected void append(PointDataView pdv) {
        int newSz = (currentSz + 1);
        if (newSz > allocatedSz) {
            resizeAll(2.0);
        }

        pdv.curIdx = currentSz;
        currentSz++;

    }

    private void resizeAll(double ratio) {
        int newSize = (int) (allocatedSz * ratio);
        for (AbstractPointDataObject<?> apdo : this.pointDataTypes.values()) {
            if (apdo.getDimensions() == 2) {
                apdo.resize(newSize * apdo.getDescription().getDimensionAsInt());
            } else {
                apdo.resize(newSize);
            }
        }
        this.allocatedSz = newSize;
    }

    public Object getData(String parameter) {
        AbstractPointDataObject<?> o = this.pointDataTypes.get(parameter);
        if (o == null) {
            return null;
        }
        if (o.getContainer() == null) {
            o.setContainer(this);
        }

        return o.getData();
    }

    public void combine(PointDataContainer container2) {
        for (String key : this.pointDataTypes.keySet()) {
            AbstractPointDataObject<?> p = this.pointDataTypes.get(key);
            p.combine(container2.pointDataTypes.get(key));
        }
        this.allocatedSz += container2.allocatedSz;
    }

    /**
     * Increments the indices of views
     * 
     * Don't call this unless you know what you're doing!
     * 
     * @param by
     * @param views
     */
    public void incrementIds(int by, List<PointDataView> views) {
        for (PointDataView pdv : views) {
            pdv.curIdx += by;
        }
    }

    /**
     * @return the allocatedSz
     */
    public int getAllocatedSz() {
        return allocatedSz;
    }

    /**
     * @param allocatedSz
     *            the allocatedSz to set
     */
    public void setAllocatedSz(int allocatedSz) {
        this.allocatedSz = allocatedSz;
    }

    /**
     * @return the pointDataTypes
     */
    public HashMap<String, AbstractPointDataObject<?>> getPointDataTypes() {
        return pointDataTypes;
    }

    /**
     * @param pointDataTypes
     *            the pointDataTypes to set
     */
    public void setPointDataTypes(
            HashMap<String, AbstractPointDataObject<?>> pointDataTypes) {
        this.pointDataTypes = pointDataTypes;
        for (AbstractPointDataObject<?> obj : pointDataTypes.values()) {
            obj.setContainer(this);
        }
    }

    /**
     * @param currentSz
     *            the currentSz to set
     */
    public void setCurrentSz(int currentSz) {
        this.currentSz = currentSz;
    }

    public static class PointDataMarshaller
            extends
            XmlAdapter<PointDataSerializable, HashMap<String, AbstractPointDataObject<?>>> {

        /*
         * (non-Javadoc)
         * 
         * @see
         * javax.xml.bind.annotation.adapters.XmlAdapter#marshal(java.lang.Object
         * )
         */
        @Override
        public PointDataSerializable marshal(
                HashMap<String, AbstractPointDataObject<?>> v) throws Exception {
            PointDataSerializable serializable = new PointDataSerializable();
            PointDataSerializable.PointDataItemSerializable[] items = new PointDataSerializable.PointDataItemSerializable[v
                    .size()];
            int i = 0;
            for (Entry<String, AbstractPointDataObject<?>> entry : v.entrySet()) {
                items[i] = new PointDataSerializable.PointDataItemSerializable();
                items[i].key = entry.getKey();
                items[i].value = entry.getValue();
                i++;
            }
            serializable.items = items;
            return serializable;
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * javax.xml.bind.annotation.adapters.XmlAdapter#unmarshal(java.lang
         * .Object)
         */
        @Override
        public HashMap<String, AbstractPointDataObject<?>> unmarshal(
                PointDataSerializable v) throws Exception {
            HashMap<String, AbstractPointDataObject<?>> map = new HashMap<String, AbstractPointDataObject<?>>(
                    v.items.length);
            for (PointDataSerializable.PointDataItemSerializable item : v.items) {
                map.put(item.key, item.value);
            }

            return map;
        }

    }

    protected AbstractPointDataObject<?> getParamSafe(String parameter) {
        AbstractPointDataObject<?> p = pointDataTypes.get(parameter);
        if (p == null) {
            throw new IllegalArgumentException("Parameter not present: "
                    + parameter);
        }

        return p;
    }

    public int getDimensions(String parameter) {
        int dimensions = -1;
        // return getParamSafe(parameter).getDescription().getNumDims();
        AbstractPointDataObject<?> p = pointDataTypes.get(parameter);
        if (p != null) {
            dimensions = p.getDimensions();
        }
        return dimensions;
    }

    public Type getType(String parameter) {
        AbstractPointDataObject<?> p = getParamSafe(parameter);
        if (p instanceof StringPointDataObject) {
            return Type.STRING;
        } else if (p instanceof IntPointDataObject) {
            return Type.INT;
        } else if (p instanceof LongPointDataObject) {
            return Type.LONG;
        } else if (p instanceof FloatPointDataObject) {
            return Type.FLOAT;
        }

        return null;
    }

    public ParameterDescription getDescription(String parameter) {
        return getParamSafe(parameter).getDescription();
    }
}
