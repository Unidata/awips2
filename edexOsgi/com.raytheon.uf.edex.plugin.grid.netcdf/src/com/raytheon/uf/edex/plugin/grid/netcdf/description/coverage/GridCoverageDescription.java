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

/**
 * Describes a Grid Coverage.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 9, 2015  4469       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlSeeAlso(CoordinateListsDescription.class)
public class GridCoverageDescription {

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
            retCoverage = (GridCoverage) this.coverage.clone();
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
}
