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
package com.raytheon.uf.edex.plugin.nos.pgblended.hdf4;

import java.io.IOException;

import ucar.nc2.Attribute;
import ucar.nc2.NetcdfFile;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.gridcoverage.Corner;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.LatLonGridCoverage;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.plugin.nos.netcdf.AbstractNosNetcdfDecoder;
import com.raytheon.uf.edex.plugin.nos.netcdf.NosRecordInfo;

/**
 * Decoder for POES/GOES Blended data in HDF4 files.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 19, 2015 4699       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
public class PGBlendedHdf4Decoder extends AbstractNosNetcdfDecoder {

    private static final String COLS_ATTR = "cols";
    private static final String ROWS_ATTR = "rows";

    /**
     * Constructor.
     */
    public PGBlendedHdf4Decoder(String descriptionFile) {
        super(descriptionFile);
    }

    /**
     * Extracts the necessary values from the file and creates a GridCoverage.
     *
     * @param file
     *            The NetcdfFile.
     * @throws GridCoverageException
     */
    @Override
    protected GridCoverage getCoverage(NetcdfFile file) throws IOException,
            GridCoverageException {
        Attribute rowsAttr = file.findGlobalAttribute(ROWS_ATTR);
        Attribute colsAttr = file.findGlobalAttribute(COLS_ATTR);
        if (rowsAttr == null || colsAttr == null) {
            return null;
        }

        int ny = (Integer) rowsAttr.getNumericValue();
        int nx = (Integer) colsAttr.getNumericValue();

        double dx = 360.0 / nx;
        double dy = 180.0 / ny;

        double la1 = 90 - .5 * dy;
        double lo1 = -180 + .5 * dx;

        LatLonGridCoverage location = new LatLonGridCoverage();
        location.setDx(dx);
        location.setDy(dy);
        location.setNx(nx);
        location.setNy(ny);
        location.setLa1(la1);
        location.setLo1(lo1);
        location.setFirstGridPointCorner(Corner.UpperLeft);
        location.setSpacingUnit("degree");
        location.initialize();

        return location;
    }

    @Override
    protected Level getLevel(NosRecordInfo recordInfo, NetcdfFile file)
            throws IOException, InvalidDescriptionException {
        return getDescriptions().getLevel().getLevel(file,
                LevelFactory.getInstance());
    }

    @Override
    protected int getNumLevels(NetcdfFile file)
            throws InvalidDescriptionException {
        return 1;
    }
}
