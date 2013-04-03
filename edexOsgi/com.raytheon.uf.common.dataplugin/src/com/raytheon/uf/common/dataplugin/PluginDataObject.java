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

package com.raytheon.uf.common.dataplugin;

import java.lang.reflect.Field;
import java.util.Calendar;
import java.util.Map;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.Transient;
import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import org.apache.commons.beanutils.PropertyUtils;
import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.annotations.DataURIUtil;
import com.raytheon.uf.common.dataplugin.persist.DefaultPathProvider;
import com.raytheon.uf.common.dataplugin.persist.IHDFFilePathProvider;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.ConvertUtil;

/**
 * Abstract class from which all plugin specific data types inherit. A plugin
 * specific data type is a class found in each plugin with the naming convention
 * of <PluginType>Record.
 * <p>
 * For example, for a plugin that handled satellite images, the associated
 * plugin specific data type would be called SatelliteRecord.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/24/07      353         bphillip    Initial creation    
 * 20071129     472         jkorman     Added getDecoderGettable().
 * 2/6/09       1990        bphillip    Added database index on dataURI
 * 3/18/09      2105        jsanchez    Added getter for id.
 *                                       Removed unused getIdentfier().
 * 
 * </pre>
 * 
 */
@Entity
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public abstract class PluginDataObject extends PersistableDataObject implements
        ISerializableObject {

    private static final long serialVersionUID = 1L;

    // @GenericGenerator(name = "generator", strategy = "hilo", parameters = {
    // @Parameter(name = "max_lo", value = "1000") })
    // @GeneratedValue(generator = "generator")
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Id
    protected int id;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    @Index(name = "dataURI_idx")
    protected String dataURI;

    /** The name of the plugin this object is associated with */
    @Transient
    @XmlAttribute
    @DynamicSerializeElement
    protected String pluginName;

    /** The data time for this record */
    @Embedded
    @XmlElement
    @DynamicSerializeElement
    @DataURI(position = 0)
    protected DataTime dataTime;

    /** The timestamp denoting when this record was inserted into the database */
    @Column(columnDefinition = "timestamp without time zone")
    @Index(name = "insertTimeIndex")
    @XmlAttribute
    @DynamicSerializeElement
    protected Calendar insertTime;

    /** The raw data from the message */
    @Transient
    @DynamicSerializeElement
    protected Object messageData;

    /** Internal variable used for creating an object from a dataURI */
    @Transient
    private transient int uriIndex = 2;

    @Transient
    private Object record = null;

    /**
     * Default Constructor
     */
    public PluginDataObject() {
    }

    public PluginDataObject(String uri) {
        String[] uriTokens = uri.split(DataURI.SEPARATOR);
        pluginName = uriTokens[1];
        populateObject(this, uriTokens);
        this.dataURI = uri;
    }

    /**
     * Constructs the dataURI for this object
     * 
     * @param tableDef
     *            The tabledefinition corresponding to this data type
     * @throws PluginException
     *             If errors occur during dataURI creation
     */
    public void constructDataURI() throws PluginException {
        if (dataURI == null) {
            StringBuffer uriBuffer = new StringBuffer();
            uriBuffer.append(DataURI.SEPARATOR).append(pluginName);
            uriBuffer = generateURI(this, uriBuffer);
            this.dataURI = uriBuffer.toString().replaceAll(" ", "_");
        }
    }

    /**
     * Recursive method for generating a dataURI
     * 
     * @param obj
     *            An object containing fields annotated with the DataURI
     *            annotation
     * @param uriBuffer
     *            The dataURI StringBuffer
     * @return The updated dataURI
     */
    private StringBuffer generateURI(Object obj, StringBuffer uriBuffer) {

        // Get the fields with @DataURI annotation
        Field[] dataURIFields = DataURIUtil.getInstance().getDataURIFields(
                obj.getClass());

        /*
         * Iterate through each field and assemble the dataURI
         */
        for (Field field : dataURIFields) {
            Object property = null;
            try {
                property = PropertyUtils.getProperty(obj, field.getName());
            } catch (Exception e) {
                e.printStackTrace();
            }
            if (field.getAnnotation(DataURI.class).embedded()) {
                // Recursive call to get dataURI elements from embedded object
                uriBuffer = generateURI(property, uriBuffer);
            } else {
                // Append to the dataURI buffer
                uriBuffer.append(DataURI.SEPARATOR);
                if (property == null) {
                    uriBuffer.append("null");
                } else if (property instanceof Calendar) {
                    uriBuffer.append(TimeUtil
                            .formatCalendar((Calendar) property));
                } else {
                    uriBuffer.append(String.valueOf(property).replaceAll(
                            DataURI.SEPARATOR, "_"));
                }
            }
        }
        return uriBuffer;
    }

    /**
     * Recursive method to populate an object from the elements in a dataURI
     * string
     * 
     * @param obj
     *            The object for which to populate fields from the dataURI
     *            string
     * @param uriTokens
     *            The elements of the dataURI string
     * @return An object populated from the elements of the dataURI string
     */
    @SuppressWarnings("unchecked")
    private Object populateObject(Object obj, String[] uriTokens) {

        // Get the fields annotated with the @DataURI annotation
        Field[] dataURIFields = DataURIUtil.getInstance().getDataURIFields(
                obj.getClass());

        Field currentField = null;
        String currentUriToken = null;
        for (int i = 0; i < dataURIFields.length; i++) {
            currentUriToken = uriTokens[uriIndex];
            currentField = dataURIFields[i];

            if (currentField.getAnnotation(DataURI.class).embedded()) {
                // The current dataURI token refers to a field in an embedded
                // object. Execute recursive call to populate embedded object
                try {
                    if (obj instanceof Map) {
                        populateObject(obj, uriTokens);
                    } else {
                        PropertyUtils.setProperty(
                                obj,
                                currentField.getName(),
                                populateObject(currentField.getType()
                                        .newInstance(), uriTokens));
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            } else {
                // The current dataURI token refers to field in the obj class.
                // Assign the field and increment the uri index variable
                uriIndex++;
                try {
                    Object property = ConvertUtil.convertObject(
                            currentUriToken, currentField.getType());
                    if (obj instanceof Map) {
                        ((Map<String, Object>) obj).put(currentField.getName(),
                                property);
                    } else {
                        try {
                            PropertyUtils.setProperty(obj,
                                    currentField.getName(), property);
                        } catch (Throwable e) {
                            // TODO Auto-generated catch block
                            e.printStackTrace();
                        }
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
        return obj;
    }

    /**
     * Retrieves the name of the field represented by the specified index in the
     * dataURI. The index starts at the dataTime in the dataURI and does not
     * include the plugin name.
     * 
     * @param clazz
     *            The class of the object to examine
     * @param targetIndex
     *            The index in the dataURI
     * @return The name of the field at the specified dataURI index
     */
    public static String getDataURIFieldName(Class<?> clazz, int targetIndex) {
        return getDataURIFieldName(targetIndex, new int[] { 0 }, clazz, "");
    }

    /**
     * Recursive method to get the dataURI field name
     * 
     * @param targetIndex
     *            The index in the DataURI to get the field name for
     * @param index
     *            The current index being examined. This field is necessary for
     *            recursive calls
     * @param clazz
     *            The class being examined
     * @param fieldName
     *            The current field name. This field is necessary for recursive
     *            calls. If the dataURI is specified in an embedded object, this
     *            field will take the form class1.class2.field i.e. for a grib
     *            record this could be modelInfo.modelName
     * @return The fieldName
     */
    private static String getDataURIFieldName(int targetIndex, int[] index,
            Class<?> clazz, String fieldName) {
        // Get the fields annotated with the @DataURI annotation
        Field[] dataURIFields = DataURIUtil.getInstance().getDataURIFields(
                clazz);

        for (Field field : dataURIFields) {

            DataURI uriAnnotation = field.getAnnotation(DataURI.class);
            if (uriAnnotation != null) {
                if (uriAnnotation.embedded()) {
                    String tmp = getDataURIFieldName(targetIndex, index,
                            field.getType(), fieldName + field.getName() + ".");
                    if (tmp != null) {
                        return tmp;
                    }
                } else {
                    if (index[0] == targetIndex) {
                        fieldName += field.getName();
                        return fieldName;
                    } else {
                        index[0]++;
                    }
                }
            }
        }
        return null;
    }

    /**
     * Gets the value in the dataURI at the specified index
     * 
     * @param index
     *            The index in the dataURI for which to get the value
     * @param fieldName
     *            The name of the field
     * @return The value in the dataURI at the specified index
     * @throws Exception
     */
    public Object getDataURIFieldValue(int index, String fieldName)
            throws Exception {
        return getDataURIFieldValue(index, new int[] { 0 }, this.getClass(),
                fieldName);
    }

    /**
     * Recursive helper method to get a dataURI field value
     * 
     * @param targetIndex
     *            The index in the dataURI for which to get the value
     * @param index
     *            The current index in the dataURI being examined
     * @param clazz
     *            The class to be examined
     * @param fieldName
     *            The name of the field
     * @return The value in the dataURI at the specified index
     * @throws Exception
     */
    private Object getDataURIFieldValue(int targetIndex, int[] index,
            Class<?> clazz, String fieldName) throws Exception {
        // Get the fields annotated with the @DataURI annotation
        Field[] dataURIFields = DataURIUtil.getInstance().getDataURIFields(
                clazz);

        for (Field field : dataURIFields) {
            DataURI uriAnnotation = field.getAnnotation(DataURI.class);
            if (uriAnnotation != null) {
                if (uriAnnotation.embedded()) {
                    Object tmp = getDataURIFieldValue(targetIndex, index,
                            field.getType(), fieldName);
                    if (tmp != null) {
                        return tmp;
                    }
                } else {
                    if (index[0] == targetIndex) {
                        return ConvertUtil.convertObject(fieldName,
                                field.getType());
                    } else {
                        index[0]++;
                    }
                }
            }
        }
        return null;
    }

    public DataTime getDataTime() {
        return dataTime;
    }

    public String getDataURI() {
        return this.dataURI;
    }

    public String toXML() throws JAXBException {
        return SerializationUtil.marshalToXml(this);
    }

    public Calendar getInsertTime() {
        return insertTime;
    }

    public void setId(int id) {
        this.id = id;
    }

    public int getId() {
        return id;
    }

    public void setInsertTime(Calendar insertTime) {
        this.insertTime = insertTime;
    }

    public Object getMessageData() {
        return messageData;
    }

    public void setMessageData(Object messageData) {
        this.messageData = messageData;
    }

    public String getPluginName() {
        return pluginName;
    }

    public void setPluginName(String pluginName) {
        this.pluginName = pluginName;
    }

    public void setDataTime(DataTime dataTime) {
        this.dataTime = dataTime;
    }

    @Override
    public String toString() {
        return this.getDataURI();
    }

    @Override
    public String getIdentifier() {
        return dataURI;
    }

    @Override
    public void setIdentifier(Object obj) {
        if (obj instanceof String) {
            this.dataURI = (String) obj;
        }
    }

    public Object getRecord() {
        return record;
    }

    public void setRecord(Object record) {
        this.record = record;
    }

    /**
     * Used to determine if a given subclass exposes the IDecoderGettable
     * interface. Normally if the class does implement the interface then a
     * reference to "this" is returned. Otherwise a null reference indicates
     * that the interface is not implemented.
     * 
     * @return The IDecoderGettable interface implementation. Null reference if
     *         not implemented.
     */
    public abstract IDecoderGettable getDecoderGettable();

    public void setDataURI(String dataURI) {
        this.dataURI = dataURI;
    }

    public IHDFFilePathProvider getHDFPathProvider() {
        return DefaultPathProvider.getInstance();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (this == obj) {
            return true;
        }
        if (!this.getClass().equals(obj.getClass())) {
            return false;
        }
        PluginDataObject rhs = (PluginDataObject) obj;
        if (dataTime == null) {
            if (rhs.dataTime != null) {
                return false;
            }
        } else if (!dataTime.getRefTime()
                .equals(rhs.getDataTime().getRefTime())) {
            return false;
        } else if (dataTime.getFcstTime() != rhs.getDataTime().getFcstTime()) {
            return false;
        }

        if (dataURI == null) {
            if (rhs.dataURI != null) {
                return false;
            }
        } else if (!dataURI.equals(rhs.dataURI)) {
            return false;
        }

        if (insertTime == null) {
            if (rhs.insertTime != null) {
                return false;
            }
        } else if (!insertTime.equals(rhs.insertTime)) {
            return false;
        }

        if (pluginName == null) {
            if (rhs.pluginName != null) {
                return false;
            }
        } else if (!pluginName.equals(rhs.pluginName)) {
            return false;
        }

        return true;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((dataTime == null) ? 0 : dataTime.hashCode());
        result = prime * result + ((dataURI == null) ? 0 : dataURI.hashCode());
        result = prime * result + id;
        result = prime * result
                + ((insertTime == null) ? 0 : insertTime.hashCode());
        result = prime * result
                + ((pluginName == null) ? 0 : pluginName.hashCode());
        return result;
    }

}