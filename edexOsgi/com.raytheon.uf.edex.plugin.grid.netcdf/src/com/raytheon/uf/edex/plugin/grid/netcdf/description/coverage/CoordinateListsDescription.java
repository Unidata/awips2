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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.common.gridcoverage.Corner;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.edex.netcdf.decoder.exception.NetcdfDecoderException;
import com.raytheon.uf.edex.netcdf.description.AbstractFieldDescription;
import com.raytheon.uf.edex.netcdf.description.ValueDescription;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;

/**
 * Describes the coordinates for a Grid Coverage where the latitude and
 * longitude coordinates are stored in separate fields as lists.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 11, 2015 4698       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class CoordinateListsDescription extends
        CoverageCoordinatesDescription {

    /**
     * Constructor.
     */
    public CoordinateListsDescription() {
        super();
    }

    @Override
    public void updateCoverage(GridCoverage coverage, NetcdfFile file)
            throws InvalidDescriptionException {
        validateRequirements(coverage);

        double[] latRange = getCoordRange(file, this.latitude);
        double dy = Math.abs(latRange[1] - latRange[0]);
        int ny = (int) this.latitude.getLength(file);
        dy /= ny - 1;

        double[] lonRange = getCoordRange(file, this.longitude);
        double dx = Math.abs(lonRange[1] - lonRange[0]);
        int nx = (int) this.longitude.getLength(file);
        dx /= nx - 1;

        Corner corner;
        double laCenterOffset = dy * .5;
        double loCenterOffset = dx * .5;
        if (latRange[0] >= latRange[1]) {
            laCenterOffset = -laCenterOffset;
            if (lonRange[0] <= lonRange[1]) {
                corner = Corner.UpperLeft;
            } else {
                corner = Corner.UpperRight;
                loCenterOffset = -loCenterOffset;
            }
        } else {
            if (lonRange[0] <= lonRange[1]) {
                corner = Corner.LowerLeft;
            } else {
                corner = Corner.LowerRight;
                loCenterOffset = -loCenterOffset;
            }
        }

        double lo1 = lonRange[0];
        if (lo1 > 180) {
            lo1 -= 360;
        } else if (lo1 < -180) {
            lo1 += 360;
        }
        coverage.setDx(dx);
        coverage.setDy(dy);
        coverage.setNx(nx);
        coverage.setNy(ny);
        coverage.setLa1(latRange[0] + laCenterOffset);
        coverage.setLo1(lo1 + loCenterOffset);
        coverage.setFirstGridPointCorner(corner);
        coverage.setSpacingUnit("degree");
    }

    /**
     * @param coverage
     * @throws InvalidDescriptionException
     */
    private void validateRequirements(GridCoverage coverage)
            throws InvalidDescriptionException {
        if (coverage == null) {
            throw new InvalidDescriptionException(
                    "A base coverage specifying the projection is required.");
        }

        if (this.latitude == null) {
            throw new InvalidDescriptionException(
                "Latitude field is not configured.");
        }

        if (this.longitude == null) {
            throw new InvalidDescriptionException(
                    "Longitude field is not configured.");
        }

        // Cannot support value descriptions because they're scalar
        if (this.latitude instanceof ValueDescription
                || this.longitude instanceof ValueDescription) {
            throw new InvalidDescriptionException(
                    "Value descriptions are unsupported for latitude and longitude of "
                            + getClass().getName());
        }
    }

    /**
     * Returns the first and last values for a coordinate.
     *
     * @param coordVar
     *            The coordinate variable.
     * @return The first and last values for a coordinate.
     * @throws NetcdfDecoderException
     */
    private double[] getCoordRange(NetcdfFile file,
            AbstractFieldDescription field) throws InvalidDescriptionException {
        int len = (int) field.getLength(file);
        if (len < 2) {
            throw new InvalidDescriptionException(field.getName()
                    + " does not have at least two coordinates.");
        }

        return new double[] { field.getNumber(file, 0).doubleValue(),
                field.getNumber(file, len - 1).doubleValue() };
    }
}
