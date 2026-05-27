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
package com.raytheon.uf.edex.plugin.mpe.fieldgen;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.file.Files;
import java.nio.file.Path;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.mpe.constants.DPAConstants;
import com.raytheon.uf.edex.plugin.mpe.HrapGridFactor;
import com.raytheon.uf.edex.plugin.mpe.CPlusPlusSizeConstants;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.HPEFieldgenConstants;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.HPEFieldgenUtils;

/**
 * Reads a MPE Radar Beam Height File. Based on:
 * hpe_fieldgen/TEXT/load_radar_beam_height.c.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 16, 2016 5631       bkowal      Initial creation
 * Sep 27, 2016 5631       bkowal      Utilize {@link CPlusPlusSizeConstants}.
 *
 * </pre>
 *
 * @author bkowal
 */

public class RadarBeamHeightFile {

    private static final Logger logger = LoggerFactory
            .getLogger(RadarBeamHeightFile.class);

    public static final String FILE_NAME = "mpe_radar_beam_height";

    public RadarBeamHeightFile() {
    }

    public static double[][] readFileAtLocation(final Path containingDirectory,
            final HrapGridFactor hrapGridFactor) throws Exception {
        final Path mpeRadarBeamHeightPath = containingDirectory
                .resolve(FILE_NAME);
        if (!Files.exists(mpeRadarBeamHeightPath)) {
            throw new IOException(
                    "Unable to find the radar beam height file at the specified location: "
                            + containingDirectory.toString() + ".");
        }

        /*
         * Prepare the data arrays that the file contents will be read into.
         */
        double[] origBeam = new double[DPAConstants.NUM_DPA_ELEMENTS];
        double[][] sourceBeam = new double[DPAConstants.NUM_DPA_ROWS][DPAConstants.NUM_DPA_COLS];

        logger.info("Reading radar beam height file: {} ...",
                mpeRadarBeamHeightPath.toString());

        final byte[] radarBeamBytes = Files
                .readAllBytes(mpeRadarBeamHeightPath);
        /*
         * The file should include sufficient data to fully populate the
         * destination structure.
         */
        if ((radarBeamBytes.length
                / CPlusPlusSizeConstants.SIZE_OF_DOUBLE) != origBeam.length) {
            throw new IOException(
                    "The radar beam height file at the specified location: "
                            + containingDirectory.toString()
                            + " is not >= the expected size: "
                            + (origBeam.length * CPlusPlusSizeConstants.SIZE_OF_DOUBLE)
                            + ".");
        }
        ByteBuffer radarBeamBuffer = ByteBuffer.wrap(radarBeamBytes);
        radarBeamBuffer.asDoubleBuffer().get(origBeam);

        /*
         * Convert radar beam height array to current hrap grid.
         */
        for (int i = 0; i < DPAConstants.NUM_DPA_ROWS; i++) {
            for (int j = 0; j < DPAConstants.NUM_DPA_COLS; j++) {
                int index = i * DPAConstants.NUM_DPA_COLS + j;
                sourceBeam[i][j] = origBeam[index];
            }
        }

        /*
         * Prepare the destination array.
         */
        final int factor = (hrapGridFactor == null) ? 1
                : hrapGridFactor.getNum();
        final int radarRows = DPAConstants.NUM_DPA_ROWS * factor;
        final int radarColumns = DPAConstants.NUM_DPA_COLS * factor;
        final double[][] radarBeamHeight = new double[radarRows][radarColumns];

        HPEFieldgenUtils.convertDoubleArray(DPAConstants.NUM_DPA_ROWS,
                DPAConstants.NUM_DPA_ROWS, sourceBeam,
                HPEFieldgenConstants.HEIGHT_DEFAULT, radarBeamHeight.length,
                radarBeamHeight[0].length, radarBeamHeight);
        return radarBeamHeight;
    }
}