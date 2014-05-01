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
 * Jan 10, 2014  16976     cgobs       Fixed issue on line 290. 
 * 									   Changed pcp.value[row][col] to pcp.value[col][row]
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class RenderPcpBlocking {

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

	// private static final String MPE_DQC_GRID_RENDERING_METHOD_TOKEN =
	// "mpe_dqc_grid_render_method";

	private static final int maxDistSquared = DailyQcUtils.mpe_dqc_grid_max_dist
			* DailyQcUtils.mpe_dqc_grid_max_dist;

	// private static final int maxDistSquared = 400;

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

	private boolean usingSingleDirectionCloseOutMode = true;
	private static final int MIN_PREFERRED_USED_GRID_CELLS = 4;
	private static final int MIN_REQUIRED_USED_GRID_CELLS = 1;

	private SearchDirection north = new SearchDirection();
	private SearchDirection south = new SearchDirection();
	private SearchDirection east = new SearchDirection();
	private SearchDirection west = new SearchDirection();

	private int colMin;
	private int colMax;
	private int rowMin;
	private int rowMax;
	private int prevColMin;
	private int prevColMax;
	private int prevRowMin;
	private int prevRowMax;

	// weighting variables
	private int usedGridCellCount = 0;
	private double totalWeightedValue = 0;
	private double totalDistanceWeight = 0;

	// grid variables
	private int binStationCount[][];

	private int usableStationCount = 0;

	private double valueGrid[][];
	private double prismStationGrid[][];

	private int isom = -1;

	class SearchDirection {
		private boolean isOpen;
		private int level;

		public SearchDirection() {
			reset();
		}

		public void reset() {
			setOpen(true);
			setLevel(0);
		}

		public boolean isOpen() {
			return isOpen;
		}

		public void setOpen(boolean isOpen) {
			this.isOpen = isOpen;
		}

		public void close() {
			setOpen(false);
		}

		void expandSearchIfAllowed() {
			if (isOpen()) {
				incrementLevel();

				if (getLevel() > DailyQcUtils.mpe_dqc_grid_max_dist) {
					setLevel(DailyQcUtils.mpe_dqc_grid_max_dist);
					close();
				}

			}
		}

		public void incrementLevel() {
			this.level++;
		}

		private void setLevel(int level) {
			this.level = level;
		}

		public int getLevel() {
			return level;
		}

	} // end inner class SearchDirection

	// ---------------------------------------------------------------------------------------

	// ---------------------------------------------------------------------------------------
	int getTokenValue24hrGridGenMeth() {

		int token_of_24hr_grid_gen_method = 0;

		if (first == true) {
			String tokenName = "mpe_dqc_24hr_precip_grid_meth";

			String dqc24hrPrecipMeth;

			// char message[GAGEQC_MESSAGE_LEN] = { '\0' };

			AppsDefaults appsDefaults = AppsDefaults.getInstance();
			dqc24hrPrecipMeth = appsDefaults.getToken(tokenName);

			if (dqc24hrPrecipMeth != null) {
				/* allowed token values: (ACCUM_6HR, USE_24HR) */
				if (dqc24hrPrecipMeth.equalsIgnoreCase("ACCUM_6HR")) {
					token_of_24hr_grid_gen_method = 1;
				}
			}
			first = false;
		}

		return token_of_24hr_grid_gen_method;
	}

	// ----------------------------------------------------------------------------

	// ----------------------------------------------------------------------------
	public void render_pcp(int pcpn_day, int pcpn_time, int pcpn_time_step,
			int numPstations, ArrayList<Station> stationList,
			Hrap_Grid hrap_grid, Pdata[] pdata, int[] pcp_in_use) {

		isom = DailyQcUtils.isom;

		int time_pos;

		if (pcpn_time_step == 0) {
			time_pos = pcpn_time; // for 6 hour data: 0,1,2,3.

			boolean save_grids = true;
			boolean accumulate_grids = false;

			render_pcp_internal(pcpn_day, pcpn_time, pcpn_time_step,
					numPstations, stationList, hrap_grid, pdata, pcp_in_use,
					save_grids, accumulate_grids);
		} else {
			time_pos = 4; // for 24 hour data

			/*
			 * in case the 24hr grid rendering is required, we check
			 * 24hr_grid_gen_method_token() to determine how to generate the
			 * grid. New Post OB9.2
			 */

			// debug
			// first = true;

			if (getTokenValue24hrGridGenMeth() == 1) {
				render24hrPcpUsingFour6hr(pcpn_day, pcpn_time, numPstations,
						stationList, hrap_grid, pdata, pcp_in_use);
				return;
			} else // calculate 24-hour grids the regular way
			{

				boolean save_grids = true;
				boolean accumulate_grids = false;

				render_pcp_internal(pcpn_day, pcpn_time, pcpn_time_step,
						numPstations, stationList, hrap_grid, pdata,
						pcp_in_use, save_grids, accumulate_grids);

			}
		}

	}

	private void render_pcp_internal(int pcpn_day, int pcpn_time,
			int pcpn_time_step, int numPstations,
			ArrayList<Station> stationList, Hrap_Grid hrap_grid, Pdata[] pdata,
			int[] pcp_in_use, boolean save_grids, boolean should_accumulate) {
		// String header = "RenderPcpBlocking.render_pcp_internal(): ";

		int isom = DailyQcUtils.isom;

		double resultingPrecipValue = 0.0;

		/*-----------------------------------------------------*/

		allocateGrids(hrap_grid);

		initializeGrids(hrap_grid, should_accumulate);

		placeStationsInGrid(pcpn_day, pcpn_time, pcpn_time_step, numPstations,
				stationList, hrap_grid, pdata);

		// for every grid location, determine its estimated value
		for (int col = 0; col < hrap_grid.maxi; col++) {
			for (int row = 0; row < hrap_grid.maxj; row++) {

				/*
				 * Check if the grid cell is covered by an HSA. If not, then do
				 * not estimate precipitation for it.
				 */
				if (hrap_grid.owner[col][row] == -1) {
					pcp.value[col][row] = 0;
					continue;
				}

				if (binStationCount[col][row] > 0) // if any station is in this
													// grid bin
				{
					resultingPrecipValue = valueGrid[col][row];

					// adjust grid bin with actual gage by the prism factor
					if (prismStationGrid[col][row] > 0) {
						double prismStationValue = prismStationGrid[col][row] * 25.4;
						double prismGridValue = hrap_grid.isoh[isom][col][row];

						double prismFactor = (double) prismGridValue
								/ prismStationValue;

						resultingPrecipValue *= prismFactor;
					} else {
						resultingPrecipValue = 0.0;
					}

					// pcp.value[col][row] is the value of grid,
					// so we don't need to estimate a value for [row][col]
				}

				else // this grid location requires an estimate to be generated
				{
					resultingPrecipValue = estimateValue(isom, col, row,
							hrap_grid);
				}

				if (resultingPrecipValue >= 0.0) {
					int precipInHundredthsOfMm = (int) Math
							.floor((resultingPrecipValue * 100.0));

					if (should_accumulate) { // for case where we want to make
												// 24 = 4 6hr periods added
												// together
						pcp.value[col][row] += precipInHundredthsOfMm;
					} else {
						pcp.value[col][row] = precipInHundredthsOfMm;
					}
				}

			} /* end for (col = 0 ... */

		} /* end for (row = 0 ... */

		// System.out.println(header + "maxPrecip in hundredths of mm = " +
		// maxPrecip);

		int time_pos;
		/*
		 * notice that time_pos is a variable defined inside the function, and
		 * its value do not need to be limited as 0,1,2,3,or 4. Here it is
		 * assigned with bigger numbers.
		 */
		if (pcpn_time_step == 0) // one of the 6-hr periods
		{
			time_pos = pcpn_day * 4 + 3 - pcpn_time;
		} else // 24 hr
		{
			time_pos = 40 + pcpn_day;
		}

		if (save_grids) {
			/*
			 * pcp_in_use[i] = 1 -- grids rendered via Render button OR Save
			 * Level2 option = -1 --grids for this time period not rendered
			 * (initialization value)
			 */
			pcp_in_use[time_pos] = 1;

			/*
			 * results of grid rendering routines (render_t, render_t6,
			 * render_pcp, render_z) are written to scratch files using the
			 * write_file routine. scratch file is stored in the scratch
			 * directory.
			 */

			ReadQPFGrids rqp = new ReadQPFGrids();
			rqp.write_file("pcp", time_pos, pcp);
		}

	} // end render_pcp_internal()

	private double estimateValue(int isom, int col, int row, Hrap_Grid hrap_grid) {

		// String header = "RenderPcpBlocking.estimateValue(): ";
		int r;
		int c;

		double estimatedPrecipValue = 0.0;

		if (hrap_grid.isoh[isom][col][row] < 0.01) {
			estimatedPrecipValue = 0;
			return estimatedPrecipValue; // can't do anything more, quit
		}

		// look through surrounding grid bins only for
		// values to be used in the distance-weighted interpolation

		// set to open and level = 0
		north.reset();
		south.reset();
		east.reset();
		west.reset();

		// initialize
		totalWeightedValue = 0.0;
		totalDistanceWeight = 0.0;
		usedGridCellCount = 0;

		rowMax = row;
		rowMin = row;
		colMax = col;
		colMin = col;

		prevRowMax = rowMax;
		prevRowMin = rowMin;
		prevColMax = colMax;
		prevColMin = colMin;

		while (north.isOpen() || south.isOpen() || east.isOpen()
				|| west.isOpen()) {
			// expand search levels as appropriate
			// changes rowMin, rowMax, colMin, colMax
			expandGridSearch(col, row, hrap_grid);

			if (rowMin < prevRowMin) // grew the top side
			{
				r = rowMin;
				for (c = colMin; c <= colMax; c++) {
					processGriddedStationValues(col, row, c, r, hrap_grid);
				}
			} else // didn't grow the top side
			{
				if (colMin < prevColMin) // still need to check top left corner
				{
					c = colMin;
					r = rowMin;
					processGriddedStationValues(col, row, c, r, hrap_grid);
				}

				if (colMax > prevColMax) // still need to check top right corner
				{
					c = colMax;
					r = rowMin;
					processGriddedStationValues(col, row, c, r, hrap_grid);
				}

			}

			if (rowMax > prevRowMax) // grew the bottom side
			{
				r = rowMax;
				for (c = colMin; c <= colMax; c++) {
					processGriddedStationValues(col, row, c, r, hrap_grid);
				}
			} else // didn't grow the bottom side
			{
				if (colMin < prevColMin) // still need to check bottom left
											// corner
				{
					c = colMin;
					r = rowMax;
					processGriddedStationValues(col, row, c, r, hrap_grid);
				}
				if (colMax > prevColMax) // still need to check bottom right
											// corner
				{
					c = colMax;
					r = rowMax;
					processGriddedStationValues(col, row, c, r, hrap_grid);
				}
			}

			if (colMin < prevColMin) // grew left side
			{
				c = colMin;
				for (r = rowMin + 1; r < rowMax; r++) {
					processGriddedStationValues(col, row, c, r, hrap_grid);
				}
			}

			if (colMax > prevColMax) // grew right side
			{
				c = colMax;
				for (r = rowMin + 1; r < rowMax; r++) {
					processGriddedStationValues(col, row, c, r, hrap_grid);
				}
			}

			if (usedGridCellCount >= MIN_PREFERRED_USED_GRID_CELLS) // have
																	// enough
																	// cells to
																	// look at,
																	// can quit
																	// now
			{
				north.close();
				south.close();
				east.close();
				west.close();

				// System.out.println(header +
				// "met minimum cell preference, resultingPrecipValue = " +
				// resultingPrecipValue);
			}

		} /* end while (northLevelOpen ... */

		// set weighted value to the cell or set value to 0.0 if there is not
		// enough data
		if (usedGridCellCount >= MIN_REQUIRED_USED_GRID_CELLS) {
			estimatedPrecipValue = totalWeightedValue / totalDistanceWeight;

			// System.out.println(header +
			// "met minimum cell requirement, resultingPrecipValue = " +
			// resultingPrecipValue);
		} else // set to zero precip
		{
			estimatedPrecipValue = 0.0;
		}

		return estimatedPrecipValue;

	} // end estimateValue()

	// *--------------------------------------------------------------------------

	void processGriddedStationValues(int renderingCol, int renderingRow,
			int gridCol, int gridRow, Hrap_Grid hrap_grid) {

		String header = "RenderPcpBlocking.processGriddedStationValues(): ";

		int usedCells = 0;
		// sprintf(message,
		// "renderingCol = %d, renderingRow = %d, gridCol = %d, gridRow = %d \n",
		// renderingCol, renderingRow, gridCol, gridRow);

		// logMessage(message);

		if ((gridCol < 0) || (gridCol > hrap_grid.maxi) || (gridRow < 0)
				|| (gridRow > hrap_grid.maxj)) {
			System.out.println(header + "OUT OF RANGE gridCol = " + gridCol
					+ " gridRow = " + gridRow);
		}

		double gageValue = valueGrid[gridCol][gridRow];

		if (gageValue >= 0.0) // there is a gage
		// located at (gridCol,gridRow)
		// with a valid precip value
		{

			usedCells = adjustWeights(gageValue, gridRow, gridCol,
					renderingRow, renderingCol, hrap_grid, isom,
					prismStationGrid);

			usedGridCellCount += usedCells;
			// if data was found, close off a direction, usually 2 directions,
			// such as north and west

			if (usedCells > 0) {
				determineDirectionFoundAndCloseOff(renderingCol, renderingRow,
						gridCol, gridRow);
			}

		}

		return;
	} // end processGriddedStationValues()

	// *--------------------------------------------------------------------------

	void expandGridSearch(int col, int row, Hrap_Grid hrap_grid) {
		// expand search levels as appropriate

		north.expandSearchIfAllowed();
		south.expandSearchIfAllowed();
		east.expandSearchIfAllowed();
		west.expandSearchIfAllowed();

		// save previous values of row and col min and max
		prevRowMax = rowMax;
		prevRowMin = rowMin;
		prevColMax = colMax;
		prevColMin = colMin;

		// determine nested for loop ranges
		rowMax = row + north.getLevel();
		if (rowMax >= hrap_grid.maxj) {
			rowMax = hrap_grid.maxj - 1;
			north.close();
		}

		rowMin = row - south.getLevel(); // row
		if (rowMin < 0) {
			rowMin = 0;
			south.close();
		}

		colMin = col - west.getLevel();
		if (colMin < 0) {
			colMin = 0;
			west.close();
		}

		colMax = col + east.getLevel();
		if (colMax >= hrap_grid.maxi) {
			colMax = hrap_grid.maxi - 1;
			east.close();
		}

		return;
	} // end expandGridSearch()

	/*--------------------------------------------------------------------------*/

	void determineDirectionFoundAndCloseOffSimpleVersion(int renderingCol,
			int renderingRow, int stationCol, int stationRow) {
		if (stationRow < renderingRow) {
			south.close();
		} else if (stationRow > renderingRow) {
			north.close();
		}

		if (stationCol < renderingCol) {
			west.close();
		} else if (stationCol > renderingCol) {
			east.close();
		}

	}

	/*--------------------------------------------------------------------------*/

	void determineDirectionFoundAndCloseOff(int renderingCol, int renderingRow,
			int stationCol, int stationRow) {

		final int closeEnoughToBeEqual = 2;

		// String header =
		// "RenderPcpBlocking.determineDirectionFoundAndCloseOff(): ";

		// consider taking a diff and a point is considered in a straightline if
		// its difference is < 3 from the grid bin to render

		int northSouthDifference = Math.abs(stationRow - renderingRow);
		int eastWestDifference = Math.abs(stationCol - renderingCol);

		boolean isNorthOrSouth = false;
		boolean isEastOrWest = false;

		if (northSouthDifference - eastWestDifference <= closeEnoughToBeEqual) {
			isNorthOrSouth = true;
			isEastOrWest = true;
		} else if (northSouthDifference > eastWestDifference) {
			isNorthOrSouth = true;
		} else {
			isEastOrWest = true;
		}

		if (usingSingleDirectionCloseOutMode) {
			// System.out.println(header +
			// "Using usingSingleDirectionCloseOutMode difference-based close off.");

			if (isNorthOrSouth) {
				// this point functions as the north-south representative, since
				// it is primarily north or south

				if (stationRow < renderingRow) {
					south.close();
				} else if (stationRow > renderingRow) {
					north.close();
				}
			}

			if (isEastOrWest) {
				if (stationCol < renderingCol) {
					west.close();
				} else if (stationCol > renderingCol) {
					east.close();
				}
			}
		}

		else // in mode in which points represent 2 directions (unless directly
				// North-south or east-west)
		{
			// System.out.println(header +
			// "Using traditional difference cutoff.");
			if (stationRow < renderingRow) {
				south.close();
			} else if (stationRow > renderingRow) {
				north.close();
			}

			if (stationCol < renderingCol) {
				west.close();
			} else if (stationCol > renderingCol) {
				east.close();
			}

		}

		return;
	} // determineDirectionFoundAndCloseOff()

	/*--------------------------------------------------------------------------*/
	void allocateGrids(Hrap_Grid hrap_grid) {

		int maxI = hrap_grid.maxi;
		int maxJ = hrap_grid.maxj;

		binStationCount = new int[maxI][maxJ];

		valueGrid = new double[maxI][maxJ];

		prismStationGrid = new double[maxI][maxJ];

		return;
	}

	/*--------------------------------------------------------------------------*/

	void initializeGrids(Hrap_Grid hrap_grid, boolean should_accumulate) {
		int i = 0;
		int j = 0;

		/* initialize grids */
		for (i = 0; i < hrap_grid.maxi; i++) {
			for (j = 0; j < hrap_grid.maxj; j++) {
				binStationCount[i][j] = 0;
				valueGrid[i][j] = DailyQcUtils.MOSAIC_DEFAULT; // precip in
																// inches
				prismStationGrid[i][j] = DailyQcUtils.MOSAIC_DEFAULT; // prism
																		// station
																		// value
																		// mapped
																		// to
																		// grid
																		// (this
																		// algorithm
																		// only)

				if (!should_accumulate) {
					// in special 24hr = sum of 4 6hr periods mode,
					// we accumulate in this grid, so we don't reinit every time

					pcp.value[i][j] = (int) DailyQcUtils.MOSAIC_DEFAULT; // final
																			// precip
																			// in
																			// hundredths
																			// of
																			// mm
				}
			}
		}
	}

	/*--------------------------------------------------------------------------*/

	void placeStationsInGrid(int pcpn_day, int pcpn_time, int pcpn_time_step,
			int max_stations, ArrayList<Station> stationList,
			Hrap_Grid hrap_grid, Pdata pdata[]) {

		int method = DailyQcUtils.method;
		String header = "RenderPcpBlocking.placeStationsInGrid(): ";

		int time_pos;

		if (pcpn_time_step == 0) {
			time_pos = pcpn_time; // for 6 hour data: 0,1,2,3.
		} else {
			time_pos = 4; // for 24 hour data
		}

		int hx, hy;
		int h = 0;

		int noPrismCount = 0;

		// System.out.println("max_stations = " + max_stations);

		int maxPrecip = 0;

		for (h = 0; h < max_stations; h++) {
			Station station = stationList.get(h);

			hx = (int) (station.hrap_x - hrap_grid.hrap_minx);
			hy = (int) (station.hrap_y - hrap_grid.hrap_miny);

			/* check that station is within the site's area */
			if (hx >= hrap_grid.maxi || hy >= hrap_grid.maxj || (hx < 0)
					|| (hy < 0)) {
				// This station cannot be used, because its coordinates are out
				// of range
				continue;
			}

			// debug only

			int stationCount = binStationCount[hx][hy];

			double precipValue = pdata[pcpn_day].stn[h].frain[time_pos].data;

			short qualCode = pdata[pcpn_day].stn[h].frain[time_pos].qual;

			// skip if not an acceptable quality code
			if (qualCode != SCREENED && qualCode != VERIFIED
					&& qualCode != TIMEDISTRIBUTED && qualCode != QUESTIONABLE
					&& qualCode != PARTIAL && qualCode != MANUAL) {
				continue;
			}

			if ((method == 2) && (station.isoh[isom] <= 0)) // no prism data for
															// the station
			{
				noPrismCount++;
				continue;
			}

			/* if station value is missing, ignore */
			if (precipValue >= 0.0) {

				int precipIn_h_mm = (int) Math.floor(precipValue * 100.0);

				if (precipIn_h_mm > maxPrecip) {
					maxPrecip = precipIn_h_mm;
				}

				// we have data and no other station has been assigned to this
				// bin yet
				if (binStationCount[hx][hy] == 0) {
					binStationCount[hx][hy] = 1;
					valueGrid[hx][hy] = precipValue;
					prismStationGrid[hx][hy] = station.isoh[isom];

					usableStationCount++;
				}

				else if (stationCount > 0) // we have at least 1 value for this
											// grid location
				{

					double valueGridTotalValue = (valueGrid[hx][hy] * stationCount)
							+ precipValue;
					double prismGridTotalValue = (prismStationGrid[hx][hy] * stationCount)
							+ station.isoh[isom];
					binStationCount[hx][hy]++;
					stationCount++;

					double newGridAvgValue = valueGridTotalValue / stationCount;
					double newPrismAvgValue = prismGridTotalValue
							/ stationCount;

					valueGrid[hx][hy] = newGridAvgValue;
					prismStationGrid[hx][hy] = newPrismAvgValue;

					usableStationCount++;

				} // end else
			} // end if

		} // end for (h = 0; h < max_stations; h++)

		System.out.println(header + " maxPrecip in hundredths of mm = "
				+ maxPrecip);

		// System.out.println(header +
		// " number of stations missing PRISM data = " + noPrismCount);
		// System.out.println(header + " usableStationCount = " +
		// usableStationCount);
	}

	// ---------------------------------------------------------------------------------------
	/*
	 * get the token value of token mpe_24hr_grid_gen_method. This token will
	 * determine how we generate the 24hr grid. We can either use the 24hr gage
	 * values, which is the old way, or we can use four 6hr gage values and add
	 * them together.
	 */
	/* there is some problem with the static */

	private void render24hrPcpUsingFour6hr(int pcpn_day, int pcpn_time,
			int numPstations, ArrayList<Station> stationList,
			Hrap_Grid hrap_grid, Pdata[] pdata, int[] pcp_in_use) {

		/* initialization of the pcp.value */
		for (int i = 0; i < hrap_grid.maxi; i++) {
			for (int j = 0; j < hrap_grid.maxj; j++) {
				pcp.value[i][j] = 0;
			}
		}

		boolean save_grids = false;
		boolean should_accumulate = true;

		for (int k = 0; k < 4; k++) {
			int pcpn_time_internal = k;

			int pcpn_time_step = 0; // means it is one of the 6-hr periods

			render_pcp_internal(pcpn_day, pcpn_time_internal, pcpn_time_step,
					numPstations, stationList, hrap_grid, pdata, pcp_in_use,
					save_grids, should_accumulate);

		}

	} // end render24hrPcpUsingFour6hr()

	// --------------------------------------------------------------------------
	int adjustWeights(double originalValue, int otherRow, int otherColumn,
			int renderingRow, int renderingColumn, Hrap_Grid hrap_grid,
			int monthIndex, double[][] prismStationGrid) {

		String header = "RenderPcpBlocking.adjustWeights(): ";

		final double MIN_DISTANCE = 0.00001;
		int usedCount = 0;

		double distanceWeight = 0.0;
		double weightedValue = 0.0;

		int rowDiff = otherRow - renderingRow;
		int colDiff = otherColumn - renderingColumn;
		double distanceSquared = (rowDiff * rowDiff) + (colDiff * colDiff);

		// int maxDistSquared = DailyQcUtils.mpe_dqc_grid_max_dist *
		// DailyQcUtils.mpe_dqc_grid_max_dist;

		if (distanceSquared < MIN_DISTANCE) {
			distanceSquared = MIN_DISTANCE;
		}

		/*
		 * mpe_dqc_grid_max_dist = max distance of influence of a gage in units
		 * of grid bins
		 */

		if (distanceSquared <= maxDistSquared) {
			distanceWeight = 1.0 / distanceSquared;
			weightedValue = originalValue * distanceWeight;

			// adjust by PRISM factor
			if (prismStationGrid[renderingColumn][renderingRow] > 0) {
				double prismFactor = (double) hrap_grid.isoh[monthIndex][renderingColumn][renderingRow]
						/ (prismStationGrid[renderingColumn][renderingRow] * 25.4);
				weightedValue *= prismFactor;

				System.out.println(header + " prismFactor > 0 " + prismFactor);

			}

			totalWeightedValue += weightedValue;
			totalDistanceWeight += distanceWeight;

			usedCount++;
		}

		return usedCount;
	}

	// ---------------------------------------------------------------------------------------

}
