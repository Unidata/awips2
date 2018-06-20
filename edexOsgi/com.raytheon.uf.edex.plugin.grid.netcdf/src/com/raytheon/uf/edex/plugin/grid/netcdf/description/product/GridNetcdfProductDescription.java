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
package com.raytheon.uf.edex.plugin.grid.netcdf.description.product;

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
import com.raytheon.uf.edex.plugin.grid.netcdf.description.coverage.GridCoverageDescription;

/**
 * Contains the data variable, dataset id, and parameter information for the
 * data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 25, 2015 4696       nabowle     Initial creation
 * May 17, 2016 5584       nabowle     Refactor and rename for consolidation.
 *
 * </pre>
 *
 * @author nabowle
 */
@XmlSeeAlso({ NetcdfProductDescription.class })
@XmlAccessorType(XmlAccessType.NONE)
public class GridNetcdfProductDescription extends NetcdfProductDescription {

    @XmlElement
    private GridCoverageDescription coverage;

    @XmlElement
    private DelegateFieldDescription datasetId;

    @XmlElement
    private DelegateFieldDescription secondaryId;

    @XmlElement
    private DelegateFieldDescription ensembleId;

    /**
     *
     */
    public GridNetcdfProductDescription() {
        super();
    }

    /**
     * @return the data
     */
    @Override
    @XmlElement
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
     * @return the coverage
     */
    public GridCoverageDescription getCoverage() {
        return coverage;
    }

    /**
     * @param coverage
     *            the coverage to set
     */
    public void setCoverage(GridCoverageDescription coverage) {
        this.coverage = coverage;
    }

    /**
     * Validate this description and the fields it contains.
     */
    @Override
    public void validate() throws InvalidDescriptionException {
        super.validate();

        if (this.coverage != null) {
            this.coverage.validate();
        }

        if (this.datasetId != null) {
            this.datasetId.validate();
        }

        if (this.secondaryId != null) {
            this.secondaryId.validate();
        }

        if (this.ensembleId != null) {
            this.ensembleId.validate();
        }
    }

    /**
     * @return the datasetId
     */
    public DelegateFieldDescription getDatasetId() {
        return datasetId;
    }

    /**
     * @param datasetId
     *            the datasetId to set
     */
    public void setDatasetId(DelegateFieldDescription datasetId) {
        this.datasetId = datasetId;
    }

    /**
     * @return the secondaryId
     */
    public DelegateFieldDescription getSecondaryId() {
        return secondaryId;
    }

    /**
     * @param secondaryId
     *            the secondaryId to set
     */
    public void setSecondaryId(DelegateFieldDescription secondaryId) {
        this.secondaryId = secondaryId;
    }

    /**
     * @return the ensembleId
     */
    public DelegateFieldDescription getEnsembleId() {
        return ensembleId;
    }

    /**
     * @param ensembleId
     *            the ensembleId to set
     */
    public void setEnsembleId(DelegateFieldDescription ensembleId) {
        this.ensembleId = ensembleId;
    }

    @Override
    public void updateFields(NetcdfFile netcdfFile,
            List<NetcdfRecordInfo> records) throws InvalidDescriptionException {
        super.updateFields(netcdfFile, records);

        NetcdfDecoderUtils.updateField(netcdfFile, this.datasetId, "datasetId",
                records);
        NetcdfDecoderUtils.updateField(netcdfFile, this.secondaryId,
                "secondaryId", records);
        NetcdfDecoderUtils.updateField(netcdfFile, this.ensembleId,
                "ensembleId", records);

        if (this.coverage != null) {
            for (NetcdfRecordInfo info : records) {
                info.addDeferredDescription(
                        GridCoverageDescription.COVERAGE_KEY, this.coverage);
            }
        }
    }

    @Override
    public boolean isAllPresent(NetcdfFile file, Logger logger)
            throws InvalidDescriptionException {
        if (!super.isAllPresent(file, logger)) {
            return false;
        }

        if (this.datasetId != null) {
            if (!this.datasetId.isPresent(file)) {
                logDebugMessage(logger, "datasetId");
                return false;
            }
        }
        if (this.secondaryId != null) {
            if (!this.secondaryId.isPresent(file)) {
                logDebugMessage(logger, "secondaryId");
                return false;
            }
        }
        if (this.ensembleId != null) {
            if (!this.ensembleId.isPresent(file)) {
                logDebugMessage(logger, "ensembleId");
                return false;
            }
        }
        if (this.coverage != null) {
            if (!this.coverage.isPresent(file)) {
                logDebugMessage(logger, "coverage");
                return false;
            }
        }

        return true;
    }
}
