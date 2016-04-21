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

import java.io.IOException;
import java.text.ParseException;
import java.util.Collections;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.plugin.goesr.exception.GoesrDecoderException;
import com.raytheon.uf.edex.plugin.goesr.exception.GoesrProjectionException;
import com.raytheon.uf.edex.plugin.goesr.geospatial.GoesrProjectionFactory;

/**
 * 
 * Contains the information necessary to match a {@link NetcdfFile} and its
 * global attributes to a {@link SatelliteRecord}. Logically this class is
 * copmposed of three parts, the {@link AttributeMatcher}s, the
 * {@link DataDescription}, and the {@link AttributeValue}s.
 * 
 * <ul>
 * <li>The {@link AttributeMatcher}s are evaluated to decide if this description
 * can be applied to the specified file.
 * <li>The {@link DataDescription} is optional, when it is present it describes
 * how the data variables with the file are mapped into the message data of the
 * SatelliteRecord.
 * <li>The {@link AttributeValue}s describe how to map netcdf attributes to the
 * attributes on the satellite record.
 * </ul>
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
public class ProductDescription {

    @XmlElement(name = "match")
    private List<AttributeMatcher> matches;

    @XmlElement
    private DataDescription data;

    @XmlElement
    private AttributeValue physicalElement;

    @XmlElement
    private AttributeValue creatingEntity;

    @XmlElement
    private AttributeValue source;

    @XmlElement
    private AttributeValue sectorID;

    @XmlElement
    private AttributeValue satHeight;

    @XmlElement
    private AttributeValue units;

    @XmlElement
    private DateAttributeValue dataTime;

    public List<AttributeMatcher> getMatches() {
        return matches;
    }

    public void setMatches(List<AttributeMatcher> matches) {
        this.matches = matches;
    }

    public DataDescription getData() {
        return data;
    }

    public void setData(DataDescription data) {
        this.data = data;
    }

    public AttributeValue getPhysicalElement() {
        return physicalElement;
    }

    public void setPhysicalElement(AttributeValue physicalElement) {
        this.physicalElement = physicalElement;
    }

    public AttributeValue getCreatingEntity() {
        return creatingEntity;
    }

    public void setCreatingEntity(AttributeValue creatingEntity) {
        this.creatingEntity = creatingEntity;
    }

    public AttributeValue getSource() {
        return source;
    }

    public void setSource(AttributeValue source) {
        this.source = source;
    }

    public AttributeValue getSectorID() {
        return sectorID;
    }

    public void setSectorID(AttributeValue sectorID) {
        this.sectorID = sectorID;
    }

    public AttributeValue getUnits() {
        return units;
    }

    public void setUnits(AttributeValue units) {
        this.units = units;
    }

    public DateAttributeValue getDataTime() {
        return dataTime;
    }

    public void setDataTime(DateAttributeValue dataTime) {
        this.dataTime = dataTime;
    }

    public AttributeValue getSatHeight() {
        return satHeight;
    }

    public void setSatHeight(AttributeValue satHeight) {
        this.satHeight = satHeight;
    }

    /**
     * Check if this description contains a {@link DataDescription}. If it does
     * then it should not be used to describe other records, only those that are
     * extracted from its own data description.
     * 
     * @return true if this dewscription contains a {@link DataDescription}.
     */
    public boolean hasData() {
        return data != null;
    }

    /**
     * If this description contains a {@link DataDescription} then use the data
     * description to create {@link SatelliteRecord}s with message data set
     * according to the description. This method will also apply any
     * {@link AttributeValue}s in this description.
     * 
     * @param cdfFile
     * @param projectionFactory
     * @return
     * @throws GoesrProjectionException
     * @throws ParseException
     * @throws IOException
     */
    public List<SatelliteRecord> getData(NetcdfFile cdfFile,
            GoesrProjectionFactory projectionFactory)
            throws GoesrDecoderException {
        if (data != null) {
            List<SatelliteRecord> records = data.getData(cdfFile,
                    projectionFactory);
            if (records != null) {
                for (SatelliteRecord record : records) {
                    describe(record, cdfFile, true);
                }
                return records;
            }
        }
        return Collections.emptyList();
    }

    /**
     * Apply any {@link AttributeValue}s in this description to the supploed
     * record.
     */
    public void describe(SatelliteRecord record, NetcdfFile cdfFile)
            throws GoesrDecoderException {
        describe(record, cdfFile, false);
    }

    protected void describe(SatelliteRecord record, NetcdfFile cdfFile,
            boolean override) throws GoesrDecoderException {
        if (physicalElement != null
                && (override || record.getPhysicalElement() == null)) {
            record.setPhysicalElement(physicalElement.getValue(cdfFile, record));
        }
        if (creatingEntity != null
                && (override || record.getCreatingEntity() == null)) {
            record.setCreatingEntity(creatingEntity.getValue(cdfFile, record));
        }
        if (source != null && (override || record.getSource() == null)) {
            record.setSource(source.getValue(cdfFile, record));
        }
        if (sectorID != null && (override || record.getSectorID() == null)) {
            record.setSectorID(sectorID.getValue(cdfFile, record));
        }
        if (dataTime != null && (override || record.getDataTime() == null)) {
            record.setDataTime(new DataTime(dataTime.getDate(cdfFile)));
        }
        if (units != null && (override || record.getUnits() == null)) {
            record.setUnits(units.getValue(cdfFile, record));
        }
    }

    /**
     * Test all {@link AttributeMatcher}s for this description to see if this
     * description can be applied to a file.
     * 
     * @return true if it matches.
     */
    public boolean match(NetcdfFile cdfFile) throws GoesrDecoderException {
        for (AttributeMatcher matcher : matches) {
            if (!matcher.matches(cdfFile)) {
                return false;
            }
        }
        return true;
    }

}
