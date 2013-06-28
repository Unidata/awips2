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

import java.util.Calendar;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.MappedSuperclass;
import javax.persistence.Transient;
import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

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
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;

/**
 * Abstract class from which all plugin specific data types inherit. A plugin
 * specific data type is a class found in each plugin with the naming convention
 * of <PluginType>Record.
 * <p>
 * For example, for a plugin that handled satellite images, the associated
 * plugin specific data type would be called SatelliteRecord.
 * 
 * <pre>
 * Hibernate Annotation Requirements for "@Entity" annotated classes that are subclasses
 * of PluginDataObject
 * 
 * 1) If it is not abstract and not a super class for "@Entity" annotated
 * subclasses, then add a SequenceGenerator annotation:
 * "@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "
 * <tablename>seq")"
 * 
 * 2) If it is abstract and a super class for @Entity annotated subclasses:
 * 
 * - if there are "@ManyToOne" or "@OneToMany" relationships to the class, then
 * an "@Entity" annotation has to be used otherwise use a "@MappedSuperClass"
 * annotation
 * 
 * - Add an "@Inheritance" annotation
 * "@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)"
 * 
 * - Add an "@Sequence" annotation
 * "@SequenceGenerator(name = PluginDataObject.ID_GEN)"
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jul 24, 2007 353         bphillip    Initial creation
 * Nov 29, 2007 472         jkorman     Added getDecoderGettable().
 * Feb 06, 2009 1990        bphillip    Added database index on dataURI
 * Mar 18, 2009 2105        jsanchez    Added getter for id.  Removed unused
 *                                      getIdentfier().
 * Mar 29, 2013 1638        mschenke    Added methods for loading from data map
 *                                      and creating data map from  dataURI
 *                                      fields
 * Apr 12, 2013 1857        bgonzale    Changed to MappedSuperclass, named
 *                                      generator,  GenerationType SEQUENCE,
 *                                      moved Indexes to getter  methods.
 * Apr 15, 2013 1868        bsteffen    Improved performance of createDataURIMap
 * May 02, 2013 1970        bgonzale    Moved Index annotation from getters to
 *                                      attributes.
 * May 07, 2013 1869        bsteffen    Remove dataURI column from
 *                                      PluginDataObject.
 * May 16, 2013 1869        bsteffen    Rewrite dataURI property mappings.
 * </pre>
 * 
 */
@MappedSuperclass
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public abstract class PluginDataObject extends PersistableDataObject implements
        ISerializableObject {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PluginDataObject.class);

    private static final long serialVersionUID = 1L;

    public static final String ID_GEN = "idgen";

    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = ID_GEN)
    @Id
    protected int id;

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
    @Index(name = "%TABLE%_insertTimeIndex")
    @XmlAttribute
    @DynamicSerializeElement
    protected Calendar insertTime;

    /** The raw data from the message */
    @Transient
    @DynamicSerializeElement
    protected Object messageData;

    @Transient
    protected transient String dataURI;

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
        try {
            DataURIUtil.populatePluginDataObject(this, uri);
        } catch (PluginException e) {
            // this should never happen operationally
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        this.dataURI = uri;
    }

    /**
     * Deprecated: getDataURI will generate the datauri on demand, no need to
     * construct it.
     */
    @Deprecated
    public void constructDataURI() throws PluginException {
        this.dataURI = null;
        getDataURI();
    }

    public DataTime getDataTime() {
        return dataTime;
    }

    public String getDataURI() {
        if (dataURI == null && pluginName != null) {
            try {
                this.dataURI = DataURIUtil.createDataURI(this);
            } catch (PluginException e) {
                // this should never happen operationally
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
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
        return getDataURI();
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
     * TODO: Rework non-PointDataContainer plots and remove
     * 
     * @return
     */
    @Deprecated
    public IDecoderGettable getDecoderGettable() {
        return null;
    }

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
        String dataURI = getDataURI();
        String rhsDataURI = rhs.getDataURI();
        if (dataURI == null) {
            if (rhsDataURI != null) {
                return false;
            }
        } else if (!dataURI.equals(rhsDataURI)) {
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
        result = prime * result
                + ((getDataURI() == null) ? 0 : dataURI.hashCode());
        result = prime * result + id;
        result = prime * result
                + ((insertTime == null) ? 0 : insertTime.hashCode());
        result = prime * result
                + ((pluginName == null) ? 0 : pluginName.hashCode());
        return result;
    }

}
