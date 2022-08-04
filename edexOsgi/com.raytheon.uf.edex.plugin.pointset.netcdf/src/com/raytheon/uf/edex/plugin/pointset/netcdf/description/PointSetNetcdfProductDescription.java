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
package com.raytheon.uf.edex.plugin.pointset.netcdf.description;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSeeAlso;

import org.slf4j.Logger;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.common.dataplugin.pointset.PointSetRecord;
import com.raytheon.uf.edex.netcdf.decoder.NetcdfRecordInfo;
import com.raytheon.uf.edex.netcdf.decoder.util.NetcdfDecoderUtils;
import com.raytheon.uf.edex.netcdf.description.data.DataDescription;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.direct.VariableDescription;
import com.raytheon.uf.edex.netcdf.description.field.indirect.DelegateFieldDescription;
import com.raytheon.uf.edex.netcdf.description.product.NetcdfProductDescription;
import com.raytheon.uf.edex.plugin.pointset.netcdf.PointSetNetcdfDecoder;

/**
 * Describe a single product in a {@link NetcdfFile}. The fields on this object
 * provide the information necessary for the {@link PointSetNetcdfDecoder} to
 * convert the data in a netCDF file into {@link PointSetRecord}s.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 11, 2015 4709       bsteffen    Initial creation
 * Jan 21, 2016 5208       bsteffen    Move parameter persistence to dao and
 *                                     validate
 * Mar 21, 2016 5450       nabowle     Add multi-level/multi-record support.
 * Apr 07, 2016 5450       nabowle     Add data masking.
 * Apr 19, 2016 5450       nabowle     Add multi-date support. Description patterns.
 * May 18, 2016 5452       bsteffen    Validate triangulation description.
 * May 17, 2016 5584       nabowle     Refactor and rename for consolidation.
 *
 * </pre>
 *
 * @author nabowle
 */
@XmlSeeAlso({ NetcdfProductDescription.class })
@XmlAccessorType(XmlAccessType.NONE)
public class PointSetNetcdfProductDescription extends NetcdfProductDescription {

    public static final String LONGITUDE_KEY = "__longitude";

    public static final String LATITUDE_KEY = "__latitude";

    /**
     * The variable name of the longitude data, this is only necessary if the
     * data variable does not have a coordinates attribute that references the
     * longitude variable.
     */
    @XmlElement
    private VariableDescription longitude;

    /**
     * The variable name of the latitude data, this is only necessary if the
     * data variable does not have a coordinates attribute that references the
     * latitude variable.
     */
    @XmlElement
    private VariableDescription latitude;

    @XmlElement
    private TriangulationDescription triangulation;

    @XmlElement
    private DelegateFieldDescription datasetId;

    /**
     *
     */
    public PointSetNetcdfProductDescription() {
        super();
    }

    /**
     * @return the longitude
     */
    public VariableDescription getLongitude() {
        return longitude;
    }

    /**
     * @param longitude
     *            the longitude to set
     */
    public void setLongitude(VariableDescription longitude) {
        this.longitude = longitude;
    }

    /**
     * @return the latitude
     */
    public VariableDescription getLatitude() {
        return latitude;
    }

    /**
     * @param latitude
     *            the latitude to set
     */
    public void setLatitude(VariableDescription latitude) {
        this.latitude = latitude;
    }

    /**
     * @return the triangulation
     */
    public TriangulationDescription getTriangulation() {
        return triangulation;
    }

    /**
     * @param triangulation
     *            the triangulation to set
     */
    public void setTriangulation(TriangulationDescription triangulation) {
        this.triangulation = triangulation;
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
     * Validate this description and the fields it contains.
     */
    @Override
    public void validate() throws InvalidDescriptionException {
        super.validate();

        if (this.longitude != null) {
            this.longitude.validate();
        }

        if (this.latitude != null) {
            this.latitude.validate();
        }

        if (this.triangulation != null) {
            this.triangulation.validate();
        }

        if (this.datasetId != null) {
            this.datasetId.validate();
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

    @Override
    public void updateFields(NetcdfFile netcdfFile,
            List<NetcdfRecordInfo> records) throws InvalidDescriptionException {
        super.updateFields(netcdfFile, records);

        NetcdfDecoderUtils.updateField(netcdfFile, this.datasetId, "datasetId",
                records);

        if (this.triangulation != null) {
            for (NetcdfRecordInfo record : records) {
                record.addDeferredDescription(
                        TriangulationDescription.TRIANGULATION_KEY,
                        this.triangulation);
            }
        }

        if (this.latitude != null) {
            for (NetcdfRecordInfo record : records) {
                record.addDeferredDescription(LATITUDE_KEY, this.latitude);
            }
        }

        if (this.longitude != null) {
            for (NetcdfRecordInfo record : records) {
                record.addDeferredDescription(LONGITUDE_KEY, this.longitude);
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
        if (this.latitude != null) {
            if (!this.latitude.isPresent(file)) {
                logDebugMessage(logger, "latitude");
                return false;
            }
        }
        if (this.longitude != null) {
            if (!this.longitude.isPresent(file)) {
                logDebugMessage(logger, "longitude");
                return false;
            }
        }

        return true;
    }
}
