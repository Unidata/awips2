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
package com.raytheon.uf.edex.plugin.grid.netcdf.description.coverage;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlSeeAlso;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.LambertConformalGridCoverage;
import com.raytheon.uf.common.gridcoverage.LatLonGridCoverage;
import com.raytheon.uf.common.gridcoverage.MercatorGridCoverage;
import com.raytheon.uf.common.gridcoverage.PolarStereoGridCoverage;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.direct.DimensionDescription;

/**
 * Describes a Grid Coverage.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 09, 2015  4469      nabowle     Initial creation
 * Jun 09, 2016  5548      nabowle     Updates for consolidation.
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlSeeAlso(CoordinateListsDescription.class)
public class GridCoverageDescription {

    public static final String COVERAGE_KEY = "location";

    /**
     * Unique id for this description to enable the decoder to cache the
     * coverage decoded from this description for a netcdf file.
     */
    private final String id = UUID.randomUUID().toString();

    @XmlAttribute
    private String coverageFile;

    @XmlElements({
            @XmlElement(name = "latLonGridCoverage", type = LatLonGridCoverage.class),
            @XmlElement(name = "mercatorGridCoverage", type = MercatorGridCoverage.class),
            @XmlElement(name = "lambertConformalGridCoverage", type = LambertConformalGridCoverage.class),
            @XmlElement(name = "polarStereoGridCoverage", type = PolarStereoGridCoverage.class)/*,
            @XmlElement(name = "stereographicGridCoverage", type = StereographicGridCoverage.class)*/ })
    private GridCoverage coverage;

    @XmlElements({ @XmlElement(name = "coordinateLists", type = CoordinateListsDescription.class) })
    private CoverageCoordinatesDescription coordinatesDescription;

    @XmlElement
    private DimensionDescription latitudeDimension;

    @XmlElement
    private DimensionDescription longitudeDimension;

    /*
     * TODO: Eventually this could have a list of CoverageFieldDescriptions that
     * specify how to extract various parts of the coverage that are
     * indeterminate from the coordinate description.
     */

    public GridCoverageDescription() {
        super();
    }

    /**
     * Returns a copy of the coverage, which may not be fully initialized, but
     * is safe to modify. If a coverage is not specified, either by
     * {@link #coverage} by {@link #coverageFile}, null is returned.
     *
     * @return the coverage
     * @throws InvalidDescriptionException
     *             If the described coverage is non-null and invalid.
     */
    public synchronized GridCoverage getCoverage()
            throws InvalidDescriptionException {
        if (this.coverage == null) {
            loadFromLocalizationFile();
            if (this.coverage == null) {
                return null;
            }
        }

        GridCoverage retCoverage;
        try {
            retCoverage = this.coverage.clone();
        } catch (CloneNotSupportedException e) {
            throw new InvalidDescriptionException(
                    "Unsupported GridCoverage type "
                            + this.coverage.getClass().getName(), e);
        }
        return retCoverage;
    }

    /**
     * @param coverage
     *            the coverage to set
     */
    public synchronized void setCoverage(GridCoverage coverage) {
        this.coverage = coverage;
    }

    /**
     * @return the coverageFile
     */
    public String getCoverageFile() {
        return coverageFile;
    }

    /**
     * @param coverageFile
     *            the coverageFile to set
     */
    public void setCoverageFile(String coverageFile) {
        this.coverageFile = coverageFile;
    }

    /**
     * @return the coordinatesDescription
     */
    public CoverageCoordinatesDescription getCoordinatesDescription() {
        return coordinatesDescription;
    }

    /**
     * @param coordinatesDescription
     *            the coordinatesDescription to set
     */
    public void setCoordinatesDescription(
            CoverageCoordinatesDescription coordinatesDescription) {
        this.coordinatesDescription = coordinatesDescription;
    }


    /**
     * @return the id
     */
    public String getId() {
        return id;
    }

    /**
     * @return the latitudeDimension
     */
    public DimensionDescription getLatitudeDimension() {
        return latitudeDimension;
    }

    /**
     * @param latitudeDimension
     *            the latitudeDimension to set
     */
    public void setLatitudeDimension(DimensionDescription latitudeDimension) {
        this.latitudeDimension = latitudeDimension;
    }

    /**
     * @return the longitudeDimension
     */
    public DimensionDescription getLongitudeDimension() {
        return longitudeDimension;
    }

    /**
     * @param longitudeDimension
     *            the longitudeDimension to set
     */
    public void setLongitudeDimension(DimensionDescription longitudeDimension) {
        this.longitudeDimension = longitudeDimension;
    }

    /**
     *
     * @param file
     * @return
     * @throws InvalidDescriptionException
     */
    public synchronized GridCoverage getInitializedCoverage(NetcdfFile file)
                throws InvalidDescriptionException {
        GridCoverage retCoverage = getCoverage();
        initializeFromFields(retCoverage, file);
        if (retCoverage == null) {
            throw new InvalidDescriptionException(
                    "Could not initialize the coverage.");
        }
        try {
            retCoverage.initialize();
        } catch (GridCoverageException e) {
            throw new InvalidDescriptionException(
                    "Could not initialize the coverage.", e);
        }
        return retCoverage;
    }

    private void initializeFromFields(GridCoverage coverage, NetcdfFile file)
            throws InvalidDescriptionException {
        if (this.coordinatesDescription != null) {
            this.coordinatesDescription.updateCoverage(coverage, file);
        }
    }

    /**
     * Loads the grid coverage from the specified localization file.
     *
     * @throws InvalidDescriptionException
     */
    private void loadFromLocalizationFile() throws InvalidDescriptionException {
        if (this.coverageFile == null) {
            return;
        }

        LocalizationFile locFile = PathManagerFactory.getPathManager()
                .getStaticLocalizationFile(LocalizationType.EDEX_STATIC,
                        this.coverageFile);

        if (locFile == null) {
            throw new InvalidDescriptionException(
                    "Cannot find grid coverage file " + this.coverageFile);
        }

        try (InputStream inputStream = locFile.openInputStream()) {
            // JAXBManager unmarshal works fine, JAXB.unmarshal does not.
            JAXBManager jaxb = new JAXBManager(GridCoverage.class);
            this.coverage = (GridCoverage) jaxb
                    .unmarshalFromInputStream(inputStream);
        } catch (LocalizationException | IOException | SerializationException
                | JAXBException e) {
            throw new InvalidDescriptionException(
                    "Unable to load grid coverage from " + this.coverageFile);
        }
    }

    /**
     * @param netcdfFile
     * @return
     */
    public boolean isPresent(NetcdfFile netcdfFile) {
        return this.coverage != null;
    }

    /**
     * @return
     */
    public List<String> getLatitudeNames() {
        List<String> names = new ArrayList<>();
        if (this.latitudeDimension != null) {
            names.add(this.latitudeDimension.getName());
        }
        if (this.coordinatesDescription != null
                && this.coordinatesDescription.getLatitude() != null) {
            names.add(this.coordinatesDescription.getLatitude().getName());
        }
        return names;
    }

    /**
     * @return
     */
    public List<String> getLongitudeNames() {
        List<String> names = new ArrayList<>();
        if (this.longitudeDimension != null) {
            names.add(this.longitudeDimension.getName());
        }
        if (this.coordinatesDescription != null
                && this.coordinatesDescription.getLongitude() != null) {
            names.add(this.coordinatesDescription.getLongitude().getName());
        }
        return names;
    }

    /**
     * Validate this coverage description.
     */
    public void validate() throws InvalidDescriptionException {
        if (this.latitudeDimension != null) {
            this.latitudeDimension.validate();
        }

        if (this.longitudeDimension != null) {
            this.longitudeDimension.validate();
        }

        if (this.coverageFile != null) {
            loadFromLocalizationFile();

        }

        if (this.coordinatesDescription != null) {
            this.coordinatesDescription.validate();
        }
    }
}
