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
package com.raytheon.uf.edex.plugin.goesr.decoder.lookup;

import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import ucar.nc2.Attribute;
import ucar.nc2.NetcdfFile;

import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;

/**
 * Object which can be used to map attributes from a {@link NetcdfFile} to a
 * {@link SatelliteRecord}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 17, 2015  4336     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class AttributeValue {

    @XmlAttribute
    private String value;

    @XmlAttribute
    private String attribute;

    @XmlAttribute
    private String format;

    @XmlAttribute
    private String[] formatAttributes;

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public String getAttribute() {
        return attribute;
    }

    public void setAttribute(String attribute) {
        this.attribute = attribute;
    }

    public String getFormat() {
        return format;
    }

    public void setFormat(String format) {
        this.format = format;
    }

    public String[] getFormatAttributes() {
        return formatAttributes;
    }

    public void setFormatAttributes(String[] formatAttributes) {
        this.formatAttributes = formatAttributes;
    }

    /**
     * Pull the attributes from the netCDF that this object is describing.
     * 
     * @param cdfFile
     * @return
     */
    public String getValue(NetcdfFile cdfFile, SatelliteRecord record) {
        if (value != null) {
            return value;
        } else if (attribute != null) {
            Attribute attr = cdfFile.findGlobalAttribute(attribute);
            if (attr != null) {
                return attr.getStringValue();
            } else {
                Object value = getAttributeFromRecord(record, attribute);
                if (value != null) {
                    return value.toString();
                } else {
                    return null;
                }
            }
        } else if (format != null && formatAttributes != null) {
            Object[] formatValues = new Object[formatAttributes.length];
            for (int i = 0; i < formatAttributes.length; i += 1) {
                Attribute attr = cdfFile
                        .findGlobalAttribute(formatAttributes[i]);
                if (attr == null) {
                    formatValues[i] = getAttributeFromRecord(record,
                            formatAttributes[i]);
                    if (formatValues[i] == null) {
                        return null;
                    }
                } else if (attr.getDataType().isNumeric()) {
                    formatValues[i] = attr.getNumericValue();
                } else {
                    formatValues[i] = attr.getStringValue();
                }
            }
            return String.format(format, formatValues);
        } else {
            return null;
        }
    }

    /**
     * When in the Course of decoding records, it becomes necessary for one
     * value to be sent from the {@link DataDescription} to an attribute of a
     * data record, it can be added to the
     * {@link IDataRecord#getDataAttributes()} and be used as though it were an
     * attribute on the netcdf file.
     */
    private Object getAttributeFromRecord(SatelliteRecord record, String name) {
        if (record == null) {
            return null;
        }
        IDataRecord dataRecord = (IDataRecord) record.getMessageData();
        if (dataRecord == null) {
            return null;
        }
        Map<String, Object> attributes = dataRecord.getDataAttributes();
        if (attributes == null) {
            return null;
        }
        return attributes.get(name);
    }
}
