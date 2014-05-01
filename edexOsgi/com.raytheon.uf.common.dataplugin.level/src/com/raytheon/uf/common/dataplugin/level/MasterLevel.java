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
package com.raytheon.uf.common.dataplugin.level;

import java.text.ParseException;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.lang.builder.ToStringBuilder;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * MasterLevel - once a field is set it cannot be changed.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 03, 2009            rjpeter     Initial creation.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@DynamicSerialize
@Entity
@Table(name = "level_master")
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class MasterLevel extends PersistableDataObject implements
        ISerializableObject {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MasterLevel.class);

    private static final long serialVersionUID = 1L;

    @Id
    @DynamicSerializeElement
    @XmlAttribute
    @DataURI(position = 0)
    private String name;

    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private String description;

    @Column(name = "unit")
    @DynamicSerializeElement
    @XmlAttribute
    private String unitString;

    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private String type;

    private transient Unit<?> unit;

    private transient Progression progression;

    private transient int hashCode;

    private transient boolean dirtyFlag = true;

    private transient boolean processType = true;

    public MasterLevel() {
    }

    public MasterLevel(String name) {
        setName(name);
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
        dirtyFlag = true;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getUnitString() {
        return unitString;
    }

    /**
     * May only be set once.
     * 
     * @param unitString
     */
    public void setUnitString(String unitString) {
        this.unitString = (unitString != null ? unitString.trim() : null);
        dirtyFlag = true;
    }

    public Unit<?> getUnit() {
        if (dirtyFlag) {
            generateDependentFields();
        }

        return unit;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
        processType = true;
    }

    public Progression getProgression() {
        if (processType) {
            try {
                processTypeField();
            } catch (CommunicationException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

        return progression;
    }

    public void setProgression(Progression progression) {
        this.progression = progression;
    }

    @Override
    public int hashCode() {
        if (dirtyFlag) {
            generateDependentFields();
        }

        return hashCode;
    }

    private void generateDependentFields() {
        final int prime = 31;
        hashCode = prime + ((name == null) ? 0 : name.hashCode());

        if (unitString != null && unitString.length() > 0) {
            try {
                unit = (Unit<?>) UnitFormat.getUCUMInstance().parseObject(
                        unitString);
            } catch (ParseException e) {
                e.printStackTrace();
            }
        } else {
            unit = null;
        }

        dirtyFlag = false;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        MasterLevel other = (MasterLevel) obj;
        if (name == null) {
            if (other.name != null)
                return false;
        } else if (!name.equals(other.name))
            return false;
        return true;
    }

    public boolean isCompatible(MasterLevel that) {
        boolean rval = false;

        if (that != null) {
            rval = equals(that) || name.equals(that.type)
                    || that.name.equals(type);
        }

        return rval;
    }

    private void processTypeField() throws CommunicationException {
        if (type != null && type.trim().length() > 0) {
            try {
                setProgression(Progression.valueOf(type));
            } catch (IllegalArgumentException e) {
                // check for wrongly pointing to self, otherwise would
                // infinite loop recursively
                if (!name.equals(type)) {
                    MasterLevel ml = LevelFactory.getInstance().getMasterLevel(
                            type);

                    // set to sub type
                    if (ml != null) {
                        setProgression(ml.getProgression());
                        setUnitString(ml.getUnitString());
                    }
                }
            }
        }

        processType = false;
    }

    @Override
    public String toString() {
        ToStringBuilder tmp = new ToStringBuilder(this);
        tmp.append("name", getName());
        tmp.append("type", getType());
        tmp.append("unit", getUnitString());
        tmp.append("description", getDescription());
        return tmp.toString();
    }
}