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
package com.raytheon.viz.mpe.util;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

import ucar.ma2.ArrayDouble;
import ucar.ma2.DataType;
import ucar.ma2.InvalidRangeException;
import ucar.nc2.Attribute;
import ucar.nc2.Dimension;
import ucar.nc2.NetcdfFileWriteable;
import ucar.nc2.Variable;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Grid;

/**
 * Creates netCDF output files for MPE/DQC QPE data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 9, 2011            snaples     Initial creation
 * Mar 5, 2013  15884      wkwock      gridPointLL and gridPointUR should be integer
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class WriteDQCNetCDFGrids {
    static int gcount = 1;

    static int vt_count = 0;

    static int k = 0;

    static long[] start = { 0, 0, 0 };

    static long[] count = new long[3];

    static int mxx, mxy;

    static int[] valid_times = new int[12];

    static Dimension dimp_id;

    static Dimension dimx_id;

    static Dimension dimy_id;

    // static double[] cdf_grid;
    static ArrayDouble.D3 cdf_grid;

    ArrayList<Dimension> grid_dims = new ArrayList<Dimension>();

    static Variable cdf_grid_id;

    AppsDefaults apps_defaults = AppsDefaults.getInstance();

    static NetcdfFileWriteable ncfile;

    CommonGridAttributes ga;

    /**
     * Called by AutoDailyQC or SaveLevel2Data to create a netCDF file for 6/24
     * hour QPE data.
     * 
     * @param fname_nc
     *            File name of netCDF file.
     * @param pnum
     *            Period number.
     * @param num_period_qc
     *            Number of periods with grids (number of periods qc done for).
     * @param dtype
     *            Data type code. 1 = QPE, 2 = QTE, 3 = QZE.
     * @param grid
     *            Grid definitions.
     * @param dataArray
     *            int Array of data values in [x][y] format.
     * 
     * @return Output: netCDF file.
     * 
     *         If period number = 0, Then execute ncopen and write header.
     * 
     *         If period number = number of periods qc done for, Then execute
     *         ncclose after writing grid.
     */
    public void write_dqc_netcdf_grids(String fname_nc, int pnum,
            int num_period_qc, int dtype, CommonGridAttributes grid,
            float[][] dataArray) {

        Hrap_Grid hrap_grid = DailyQcUtils.getHrap_grid();
        // Pcp pcp = DailyQcUtils.pcp;
        String databaseid = "";
        String grid_filename = "";
        String data_level;
        ga = grid;
        double[] lonOrigin = { 0 };
        int data_precision = 0;
        int data_ratedep = 0;
        int[] time_constraints = new int[3];
        double fill_value = 0;
        double[] minmax_allowable = new double[2];

        /*----------------------------------------------*/
        /* first time in this routine for new Hydro Day */
        /*----------------------------------------------*/

        if (gcount == 1) {

            mxx = hrap_grid.maxi;
            mxy = hrap_grid.maxj;

            count[0] = num_period_qc;
            count[1] = mxy;
            count[2] = mxx;

            cdf_grid = new ArrayDouble.D3((int) count[0], (int) count[1],
                    (int) count[2]);

            /* initialize */
            int mm = 0;
            for (int lo = 0; lo < num_period_qc; lo++) {
                for (int j = 0; j < mxy; j++) {
                    for (int i = 0; i < mxx; i++) {
                        cdf_grid.setDouble(mm, -9999.);
                        mm++;
                    }
                }
            }
            Arrays.fill(valid_times, 0);
            String dqc_lonorigin = apps_defaults.getToken("mpe_dqc_lonorigin");
            if (dqc_lonorigin.length() != 0) {
                try {
                    lonOrigin[0] = Float.parseFloat(dqc_lonorigin);
                } catch (NumberFormatException e) {
                    System.out
                            .println("Error parsing token for mpe_dqc_lonorigin. "
                                    + e);
                }
            } else {
                lonOrigin[0] = 0;
            }

            databaseid = String.format("%s_GRID_Fcst_0000000_0000", ga.siteID);
            grid_filename = fname_nc;

            /*-------------------------------------------*/
            /* read appropraite data array */
            /* define parameters depending on data type */
            /*-------------------------------------------*/

            switch (dtype) {
            case 1:
                /* QPE (precip) */
                data_level = "SFC";
                data_precision = 2;
                data_ratedep = 1;
                fill_value = -30000.;

                minmax_allowable[0] = 0.;
                minmax_allowable[1] = 64.;

                time_constraints[0] = 0;
                time_constraints[1] = 21600;
                time_constraints[2] = 21600;
                break;

            case 2:
                /* QTE (temperature) */
                data_level = "SFC";
                data_precision = 1;
                data_ratedep = 1;
                fill_value = -30000.;

                minmax_allowable[0] = -300.;
                minmax_allowable[1] = 400.;

                time_constraints[0] = 0;
                time_constraints[1] = 21600;
                time_constraints[2] = 21600;
                break;

            case 3:
                /* QZE (freezing level) */
                data_level = "SFC";
                data_precision = 2;
                data_ratedep = 1;
                fill_value = -30000.;

                minmax_allowable[0] = 0.;
                minmax_allowable[1] = 30000.;

                time_constraints[0] = 0;
                time_constraints[1] = 21600;
                time_constraints[2] = 21600;
                break;

            default:
                // default to type 1
                data_level = "SFC";
                data_precision = 2;
                data_ratedep = 1;
                fill_value = -30000.;

                minmax_allowable[0] = 0.;
                minmax_allowable[1] = 64.;

                time_constraints[0] = 0;
                time_constraints[1] = 21600;
                time_constraints[2] = 21600;
            }

            try {
                ncfile = NetcdfFileWriteable.createNew(grid_filename, false);
            } catch (IOException e) {
                System.out
                        .println("Error in write_dqc_netcdf_grids: Error creating netCDF file \n"
                                + e);
                return;
            } catch (Throwable e) {
                System.out.println("Error creating new netCDF file. " + e);
            }
            /* define dimensions */
            dimp_id = ncfile.addDimension("dimp", num_period_qc);
            dimx_id = ncfile.addDimension("dimx", mxx);
            dimy_id = ncfile.addDimension("dimy", mxy);

            grid_dims.add(dimp_id);
            grid_dims.add(dimy_id);
            grid_dims.add(dimx_id);

            /* define variables */
            switch (dtype) {

            case 1:
                cdf_grid_id = ncfile.addVariable("qpe_grid", DataType.FLOAT,
                        grid_dims);
                break;

            case 2:
                cdf_grid_id = ncfile.addVariable("qte_grid", DataType.FLOAT,
                        grid_dims);
                break;

            case 3:
                cdf_grid_id = ncfile.addVariable("qze_grid", DataType.FLOAT,
                        grid_dims);
                break;

            default:
                cdf_grid_id = ncfile.addVariable("qpe_grid", DataType.FLOAT,
                        grid_dims);
            }

            /* define attributes */
            cdf_grid_id.addAttribute(new Attribute("siteID", ga.siteID));

            ArrayList<Number> dims = new ArrayList<Number>();
            dims.add(ga.latLonLL.x);
            dims.add(ga.latLonLL.y);
            cdf_grid_id.addAttribute(new Attribute("latLonLL", dims));

            dims = new ArrayList<Number>();
            dims.add(ga.latLonUR.x);
            dims.add(ga.latLonUR.y);
            cdf_grid_id.addAttribute(new Attribute("latLonUR", dims));

            dims = new ArrayList<Number>();
            dims.add(ga.gridSize[0]);
            dims.add(ga.gridSize[1]);
            cdf_grid_id.addAttribute(new Attribute("gridSize", dims));

            dims = new ArrayList<Number>();
            dims.add(ga.domainOrigin.x);
            dims.add(ga.domainOrigin.y);
            cdf_grid_id.addAttribute(new Attribute("domainOrigin", dims));

            dims = new ArrayList<Number>();
            dims.add(ga.domainExtent.x);
            dims.add(ga.domainExtent.y);
            cdf_grid_id.addAttribute(new Attribute("domainExtent", dims));

            dims = new ArrayList<Number>();
            dims.add(ga.gridPointLL[0]);
            dims.add(ga.gridPointLL[1]);
            cdf_grid_id.addAttribute(new Attribute("gridPointLL", dims));

            dims = new ArrayList<Number>();
            dims.add(ga.gridPointUR[0]);
            dims.add(ga.gridPointUR[1]);
            cdf_grid_id.addAttribute(new Attribute("gridPointUR", dims));

            cdf_grid_id.addAttribute(new Attribute("gridType", ga.gridType));

            cdf_grid_id.addAttribute(new Attribute("projectionType",
                    ga.projectionType));

            cdf_grid_id.addAttribute(new Attribute("descriptiveName",
                    ga.descriptiveName));

            cdf_grid_id.addAttribute(new Attribute("units", ga.units));

            cdf_grid_id.addAttribute(new Attribute("lonOrigin", lonOrigin[0]));

            dims = new ArrayList<Number>();
            dims.add(minmax_allowable[0]);
            dims.add(minmax_allowable[1]);
            cdf_grid_id
                    .addAttribute(new Attribute("minMaxAllowedValues", dims));

            cdf_grid_id.addAttribute(new Attribute("level", data_level));

            dims = new ArrayList<Number>();
            dims.add(time_constraints[0]);
            dims.add(time_constraints[1]);
            dims.add(time_constraints[2]);
            cdf_grid_id.addAttribute(new Attribute("timeConstraints", dims));

            cdf_grid_id
                    .addAttribute(new Attribute("precision", data_precision));

            cdf_grid_id.addAttribute(new Attribute("rateDependent",
                    data_ratedep));

            cdf_grid_id.addAttribute(new Attribute("fillValue", fill_value));

            cdf_grid_id.addAttribute(new Attribute("databaseID", databaseid));

            try {
                ncfile.create();
            } catch (IOException e) {
                System.out.println("ERROR creating file "
                        + ncfile.getLocation() + "\n" + e);
            }
        }// end first time for hydro day, end define.

        /* define grid */

        for (int j = 0; j < mxy; j++) {
            for (int i = 0; i < mxx; i++) {
                // cdf_grid.setDouble(k, (pcp.value[i][j] / 100.));
                cdf_grid.setDouble(k, dataArray[i][j]);
                k++;
            }
        } // end for k loop
        valid_times[vt_count] = (int) ga.validTimes[pnum][0];
        vt_count++;
        valid_times[vt_count] = (int) ga.validTimes[pnum][1];
        vt_count++;

        if (gcount == num_period_qc) {

            /* write the variables */
            /* first reenter define mode to add the valid time attributes */

            try {
                ncfile.setRedefineMode(true);
                ArrayList<Number> dims = new ArrayList<Number>();
                for (int r = 0; r < valid_times.length; r++) {
                    if (valid_times[r] != 0) {
                        dims.add(valid_times[r]);
                    }
                }
                cdf_grid_id.addAttribute(new Attribute("validTimes", dims));
                ncfile.setRedefineMode(false);

                ncfile.write(cdf_grid_id.getFullNameEscaped(), cdf_grid);
                ncfile.close();

            } catch (InvalidRangeException e) {
                System.out
                        .println("ERROR: in write_dqc_netcdf_grids: InvalidRangeException in writing cdf_grid. "
                                + e.getMessage());
            } catch (IOException e) {
                System.out.println("Error redefining netCDF file, Exiting....");
                return;
            }
            if (cdf_grid != null) {
                cdf_grid = null;
            }
            k = 0;
            gcount = 1;
            vt_count = 0;

        } else {
            gcount++;
        }
    }
}
