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
package com.raytheon.uf.edex.plugin.nos.ncom.netcdf;

import java.io.IOException;

import ucar.ma2.Array;
import ucar.nc2.Attribute;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.gridcoverage.Corner;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.LatLonGridCoverage;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;
import com.raytheon.uf.edex.netcdf.description.LevelDescription;
import com.raytheon.uf.edex.netcdf.description.VariableDescription;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.plugin.nos.netcdf.AbstractNosNetcdfDecoder;
import com.raytheon.uf.edex.plugin.nos.netcdf.NosRecordInfo;

/**
 * Decoder for Navy Coastal Ocean Model (NCOM) data in netcdf files.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 27, 2015 4698       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
public class NcomNetcdfDecoder extends AbstractNosNetcdfDecoder {

    private static final String VALID_RANGE_ATTR = "valid_range";

    /**
     * Constructor.
     */
    public NcomNetcdfDecoder(String descriptionFile) {
        super(descriptionFile);
    }

    /**
     * @param file
     * @param descriptions
     * @return
     * @throws InvalidDescriptionException
     */
    @Override
    protected int getNumLevels(NetcdfFile file)
            throws InvalidDescriptionException {
        LevelDescription levelDesc = getDescriptions().getLevel();
        VariableDescription levelOneDesc = (VariableDescription) levelDesc
                .getLevelOneValue();
        Variable levelVar = file.findVariable(levelOneDesc.getName());
        if (levelVar == null) {
            throw new InvalidDescriptionException(
                    "Cannot find required level variable "
                            + levelOneDesc.getName());
        }
        int numLevels = levelVar.getDimension(0).getLength();
        return numLevels;
    }


    /**
     * @param recordInfo
     * @param file
     * @return
     * @throws IOException
     * @throws InvalidDescriptionException
     */
    @Override
    protected Level getLevel(NosRecordInfo recordInfo, NetcdfFile file)
            throws IOException, InvalidDescriptionException {
        LevelDescription levelDesc = getDescriptions().getLevel();
        String levelOneName = ((VariableDescription) levelDesc
                .getLevelOneValue()).getName();
        Variable levelOneVar = file.findVariable(levelOneName);
        if (levelOneVar == null) {
            return null;
        }

        Array arr = levelOneVar.read();
        double levelOneVal = arr.getDouble(recordInfo.getLevelIdx());
        Level level = LevelFactory.getInstance().getLevel(
                levelDesc.getMasterLevel().getString(file), levelOneVal);

        return level;
    }



    /**
     * Extracts the necessary values from the file and creates a GridCoverage.
     *
     * @param file
     *            The NetcdfFile.
     * @throws GridCoverageException
     * @throws IOException
     */
    @Override
    protected GridCoverage getCoverage(NetcdfFile file)
            throws GridCoverageException, IOException {

        Variable latVar = file.findVariable(LAT_VAR);
        Variable lonVar = file.findVariable(LON_VAR);

        if (latVar == null || lonVar == null) {
            return null;
        }

        double[] latRange = getCoordRange(latVar);
        double dy = Math.abs(latRange[1] - latRange[0]);
        int ny = latVar.getDimension(0).getLength();
        dy /= ny - 1;

        double[] lonRange = getCoordRange(lonVar);
        double dx = Math.abs(lonRange[1] - lonRange[0]);
        int nx = lonVar.getDimension(0).getLength();
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
        double lo2 = lonRange[1];
        if (lo2 > 180) {
            lo2 -= 360;
        } else if (lo2 < -180) {
            lo2 += 360;
        }

        LatLonGridCoverage location = new LatLonGridCoverage();
        location.setDx(dx);
        location.setDy(dy);
        location.setNx(nx);
        location.setNy(ny);
        location.setLa1(latRange[0] + laCenterOffset);
        location.setLo1(lo1 + loCenterOffset);
        location.setLa2(latRange[1] + laCenterOffset);
        location.setLo2(lo2 + loCenterOffset);
        location.setFirstGridPointCorner(corner);
        location.setSpacingUnit("degree");
        location.initialize();

        return location;
    }

    /**
     * Returns the first and last values for a coordinate.
     *
     * @param coordVar
     *            The coordinate variable.
     * @return The first and last values for a coordinate.
     * @throws IOException
     */
    private double[] getCoordRange(Variable coordVar) throws IOException {
        Attribute validRangeAttr = coordVar.findAttribute(VALID_RANGE_ATTR);
        double[] coordRange;
        if (validRangeAttr == null) {
            Array latArr = coordVar.read();
            coordRange = new double[] { latArr.getDouble(0),
                    latArr.getDouble((int) (latArr.getSize() - 1)) };
        } else {
            coordRange = (double[]) validRangeAttr.getValues()
                    .copyTo1DJavaArray();
        }
        return coordRange;
    }

}
