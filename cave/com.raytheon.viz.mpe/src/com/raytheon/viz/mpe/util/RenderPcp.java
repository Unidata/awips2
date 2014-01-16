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

import java.util.ArrayList;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Grid;
import com.raytheon.viz.mpe.util.DailyQcUtils.Pcp;
import com.raytheon.viz.mpe.util.DailyQcUtils.Pdata;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;

/**
 * DESCRIPTION: Maps precipitation station values to an HRAP grid. Produces the
 * DailyQC version of the GageOnly analysis.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2009            snaples     Initial creation
 * May 02, 2011   8962     snaples     Added render24hrPcpUsingFour6hr() method
 * Jan 10, 2014  16976     cgobs       Fixed issue on line 153. 
 * 									   Changed pcp.value[row][col] to pcp.value[col][row]
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class RenderPcp {

	private static boolean first = true;

	Pcp pcp = DailyQcUtils.pcp;

	/*
	 * QC codes
	 */
	private static final int MISSING = -1;

	private static final int STANDARD = 0;

	private static final int SCREENED = 0;

	private static final int FAILED = 1;

	private static final int MANUAL = 2;

	private static final int QUESTIONABLE = 3;

	private static final int PARTIAL = 4;

	private static final int ESTIMATED = 5;

	private static final int TIMEDISTRIBUTED = 6;

	private static final int VERIFIED = 8;

	private static final String MPE_DQC_GRID_RENDERING_METHOD_TOKEN = "mpe_dqc_grid_render_method";

	private static final int maxDistSquared = DailyQcUtils.mpe_dqc_grid_max_dist
			* DailyQcUtils.mpe_dqc_grid_max_dist;

	/**
	 * Converts point precipitation data in struct pdata to hrap grid. The 20
	 * closest stations are precalculated for each grid point - this speeds
	 * computations for data sets with many precipitation points. If there are
	 * no good precipitation points for a grid point, then a recalculation is
	 * made using all precipitation points. 1/R**2 interpolation is used. If
	 * requested, the final interpolation is scaled using seasonal isohyets. The
	 * grid is saved as a disk file and used later for a raster or vector (HRAP)
	 * plot.
	 * 
	 * @param m
	 * @param k
	 * @param mk
	 * @param numPstations
	 * @param
	 * @param hrap_grid
	 * @param pdata
	 * @param pcp_in_use
	 */
	// mpe_dqc_grid_render_method

	// ---------------------------------------------------------------------------------------

	public void render_pcp(int pcpn_day, int pcpn_time, int pcpn_time_step,
			int numPstations, ArrayList<Station> precip_stations,
			Hrap_Grid hrap_grid, Pdata[] pdata, int[] pcp_in_use) {

		String header = "RenderPcp.render_pcp(): ";

		AppsDefaults appsDefaults = AppsDefaults.getInstance();

		String tokenValue = appsDefaults
				.getToken(MPE_DQC_GRID_RENDERING_METHOD_TOKEN);

		// System.out.println(header + "tokenValue = " + tokenValue);

		// tokenValue = "BLOCKING";
		// tokenValue = "DISTANCE_SQUARED";

		System.out.println(header + "now tokenValue = " + tokenValue);

		if (tokenValue != null && tokenValue.equalsIgnoreCase("BLOCKING")) {
			RenderPcpBlocking renderer = new RenderPcpBlocking();

			renderer.render_pcp(pcpn_day, pcpn_time, pcpn_time_step,
					numPstations, precip_stations, hrap_grid, pdata, pcp_in_use);
		}

		else {
			RenderPcpStandard renderer = new RenderPcpStandard();

			renderer.render_pcp(pcpn_day, pcpn_time, pcpn_time_step,
					numPstations, precip_stations, hrap_grid, pdata, pcp_in_use);

		}

		determineMaxPrecip(hrap_grid);

	}

	private int determineMaxPrecip(Hrap_Grid hrap_grid) {
		// TODO Auto-generated method stub

		String header = "RenderPcp.determineMaxPrecip(): ";

		int value = 0;
		int maxValue = -1;

		for (int col = 0; col < hrap_grid.maxi; col++) {
			for (int row = 0; row < hrap_grid.maxj; row++) {
				value = pcp.value[col][row];

				if (value > maxValue) {
					maxValue = value;
				}

			}

		}

		System.out.println(header + "maxValue = " + maxValue);

		return maxValue;

	} // end determineMaxPrecip()

	// ---------------------------------------------------------------------------------------
} // end class RenderPcp