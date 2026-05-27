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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe;

import java.nio.file.Path;

/**
 * Interface defining common methods for determining which file formats a xmrg
 * file associated with a particular mosaic should be converted to.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 7, 2016  5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public interface IXmrgAlternateFormatFlags {

    /**
     * Returns a boolean flag indicating whether or not a gif should be
     * generated for a certain mosaic.
     * 
     * @return {@code true}, if the gif should be generated; {@code false},
     *         otherwise.
     */
    public Boolean getSaveGIF();

    /**
     * Returns a boolean flag indicating whether or not a jpeg should be
     * generated for a certain mosaic.
     * 
     * @return {@code true}, if the jpeg should be generated; {@code false},
     *         otherwise.
     */
    public Boolean getSaveJPEG();

    /**
     * Returns a boolean flag indicating whether or not a grib file should be
     * generated for a certain mosaic.
     * 
     * @return {@code true}, if the grib file should be generated; {@code false}
     *         , otherwise.
     */
    public Boolean getSaveGRIB();

    /**
     * Returns a boolean flag indicating whether or not a netcdf file should be
     * generated for a certain mosaic.
     * 
     * @return {@code true}, if the netcdf file should be generated;
     *         {@code false}, otherwise.
     */
    public Boolean getSaveNetCDF();

    /**
     * Returns the location that the generated gif should be saved to.
     * 
     * @return the location that the generated gif should be saved to as a
     *         {@link Path}.
     */
    public Path getGifPath();

    /**
     * Returns an identifier value that should be included in the name of the
     * generated gif file.
     * 
     * @return an identifier to use in the name of the generated gif file.
     */
    public String getGifId();

    /**
     * Returns the location that the generated netcdf file should be saved to.
     * 
     * @return the location that the generated netcdf file should be saved to as
     *         a {@link Path}.
     */
    public Path getNetCDFPath();

    /**
     * Returns an identifier value that should be included in the name of the
     * generated netcdf file.
     * 
     * @return an identifier to use in the name of the generated netcdf file.
     */
    public String getNetCDFId();
}