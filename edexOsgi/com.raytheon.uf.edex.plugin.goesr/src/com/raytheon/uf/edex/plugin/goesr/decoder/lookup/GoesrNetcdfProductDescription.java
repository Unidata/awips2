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

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSeeAlso;

import org.slf4j.Logger;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.decoder.NetcdfRecordInfo;
import com.raytheon.uf.edex.netcdf.decoder.util.NetcdfDecoderUtils;
import com.raytheon.uf.edex.netcdf.description.data.DataDescription;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.indirect.DelegateFieldDescription;
import com.raytheon.uf.edex.netcdf.description.product.NetcdfProductDescription;
import com.raytheon.uf.edex.plugin.goesr.description.data.GoesrDataDescription;

/**
 * Product Description of Netcdf Goes-R data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 17, 2016 5584       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 */
@XmlSeeAlso({ NetcdfProductDescription.class })
@XmlAccessorType(XmlAccessType.NONE)
public class GoesrNetcdfProductDescription extends NetcdfProductDescription {

    @XmlElement
    private DelegateFieldDescription physicalElement;

    @XmlElement
    private DelegateFieldDescription units;

    @XmlElement
    private DelegateFieldDescription creatingEntity;

    @XmlElement
    private DelegateFieldDescription source;

    @XmlElement
    private DelegateFieldDescription sectorID;

    @XmlElement
    private DelegateFieldDescription satHeight;

    /**
     *
     */
    public GoesrNetcdfProductDescription() {
        super();
    }

    /**
     * @return the data
     */
    @Override
    @XmlElement(type = GoesrDataDescription.class)
    public DataDescription getData() {
        return super.getData();
    }

    /**
     * @param data
     *            the data to set
     */
    @Override
    public void setData(DataDescription data) {
        super.setData(data);
    }

    /**
     * @return the physicalElement
     */
    public DelegateFieldDescription getPhysicalElement() {
        return physicalElement;
    }

    /**
     * @param physicalElement
     *            the physicalElement to set
     */
    public void setPhysicalElement(DelegateFieldDescription physicalElement) {
        this.physicalElement = physicalElement;
    }

    /**
     * @return the units
     */
    public DelegateFieldDescription getUnits() {
        return units;
    }

    /**
     * @param units
     *            the units to set
     */
    public void setUnits(DelegateFieldDescription units) {
        this.units = units;
    }

    /**
     * @return the creatingEntity
     */
    public DelegateFieldDescription getCreatingEntity() {
        return creatingEntity;
    }

    /**
     * @param creatingEntity
     *            the creatingEntity to set
     */
    public void setCreatingEntity(DelegateFieldDescription creatingEntity) {
        this.creatingEntity = creatingEntity;
    }

    /**
     * @return the source
     */
    public DelegateFieldDescription getSource() {
        return source;
    }

    /**
     * @param source
     *            the source to set
     */
    public void setSource(DelegateFieldDescription source) {
        this.source = source;
    }

    /**
     * @return the sectorId
     */
    public DelegateFieldDescription getSectorID() {
        return sectorID;
    }

    /**
     * @param sectorId
     *            the sectorId to set
     */
    public void setSectorID(DelegateFieldDescription sectorId) {
        this.sectorID = sectorId;
    }

    /**
     * @return the satHeight
     */
    public DelegateFieldDescription getSatHeight() {
        return satHeight;
    }

    /**
     * @param satHeight
     *            the satHeight to set
     */
    public void setSatHeight(DelegateFieldDescription satHeight) {
        this.satHeight = satHeight;
    }

    @Override
    public void updateFields(NetcdfFile netcdfFile,
            List<NetcdfRecordInfo> records) throws InvalidDescriptionException {
        super.updateFields(netcdfFile, records);

        NetcdfDecoderUtils.updateField(netcdfFile, this.creatingEntity, "creatingEntity", records);
        NetcdfDecoderUtils.updateField(netcdfFile, this.physicalElement, "physicalElement",
                records);
        NetcdfDecoderUtils.updateField(netcdfFile, this.satHeight, "satHeight", records);
        NetcdfDecoderUtils.updateField(netcdfFile, this.sectorID, "sectorID",
                records);
        NetcdfDecoderUtils.updateField(netcdfFile, this.source, "source", records);
        NetcdfDecoderUtils.updateField(netcdfFile, this.units, "units", records);
    }

    @Override
    public boolean isAllPresent(NetcdfFile file, Logger logger)
            throws InvalidDescriptionException {
        if (!super.isAllPresent(file, logger)) {
            return false;
        }

        if (this.physicalElement != null) {
            if (!this.physicalElement.isPresent(file)) {
                logDebugMessage(logger, "physicalElement");
                return false;
            }
        }
        if (this.creatingEntity != null) {
            if (!this.creatingEntity.isPresent(file)) {
                logDebugMessage(logger, "creatingEntity");
                return false;
            }
        }
        if (this.satHeight != null) {
            if (!this.satHeight.isPresent(file)) {
                logDebugMessage(logger, "satHeight");
                return false;
            }
        }
        if (this.sectorID != null) {
            if (!this.sectorID.isPresent(file)) {
                logDebugMessage(logger, "sectorID");
                return false;
            }
        }
        if (this.source != null) {
            if (!this.source.isPresent(file)) {
                logDebugMessage(logger, "source");
                return false;
            }
        }
        if (this.units != null) {
            if (!this.units.isPresent(file)) {
                logDebugMessage(logger, "units");
                return false;
            }
        }

        return true;
    }

    @Override
    public void validate() throws InvalidDescriptionException {
        super.validate();

        if (this.physicalElement != null) {
            this.physicalElement.validate();
        }
        if (this.creatingEntity != null) {
            this.creatingEntity.validate();
        }
        if (this.satHeight != null) {
            this.satHeight.validate();
        }
        if (this.sectorID != null) {
            this.sectorID.validate();
        }
        if (this.source != null) {
            this.source.validate();
        }
        if (this.units != null) {
            this.units.validate();
        }
    }
}
