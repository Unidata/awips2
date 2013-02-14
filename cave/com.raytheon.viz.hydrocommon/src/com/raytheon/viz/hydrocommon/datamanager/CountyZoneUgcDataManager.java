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
package com.raytheon.viz.hydrocommon.datamanager;

import java.util.ArrayList;
import java.util.Collections;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.CountiesData;
import com.raytheon.viz.hydrocommon.data.CountyInfoData;
import com.raytheon.viz.hydrocommon.data.EligZoneData;
import com.raytheon.viz.hydrocommon.data.ZoneInfoData;

/**
 * Class for managing database query calls. QcAlertAlarmLimitsDataManager.java
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 5, 2009  1802       askripsky   Initial Creation
 * Sep 11,2012  15362      wkwock      Fix selected zones
 * Dec 4, 2012  15522      wkwock      Fix added zones
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class CountyZoneUgcDataManager {
	protected static CountyZoneUgcDataManager manager = null;

	// Cache for selected counties
	private ArrayList<CountyInfoData> countiesSelected = null;

	// Cache for available counties
	private ArrayList<CountiesData> countiesAvailable = null;

	// Cache for selected zones
	private ArrayList<ZoneInfoData> zonesSelected = null;

	// Cache for available zones
	private ArrayList<EligZoneData> zonesAvailable = null;

	// Counties/Zones for this Location
	private String lid = "";

	/**
	 * Private constructor.
	 */
	private CountyZoneUgcDataManager() {
	}

	/**
	 * Singleton pattern of data manager.
	 * 
	 * @return manager
	 */
	public static synchronized CountyZoneUgcDataManager getInstance() {
		if (manager == null) {
			manager = new CountyZoneUgcDataManager();
		}

		return (CountyZoneUgcDataManager) manager;
	}

	/**
	 * Sets the location for the data.
	 * 
	 * @param lid
	 *            The location for the counties/zones
	 */
	public void setLid(String lid) {
		this.lid = lid;
	}

	/**
	 * Gets the available counties.
	 * 
	 * @return The counties available.
	 * @throws VizException
	 */
	public ArrayList<CountiesData> getCountiesAvailable() throws VizException {
		if (countiesAvailable == null) {
			countiesAvailable = HydroDBDataManager.getInstance().getData(
					CountiesData.class);
		}

		return countiesAvailable;
	}

	/**
	 * Gets the selected counties.
	 * 
	 * @return The counties selected.
	 * @throws VizException
	 */
	public ArrayList<CountyInfoData> getCountiesSelected() throws VizException {
		return getCountiesSelected(false);
	}

	/**
	 * Gets the selected counties.
	 * 
	 * @return The counties selected.
	 * @throws VizException
	 */
	public ArrayList<CountyInfoData> getCountiesSelected(boolean forceLoad)
			throws VizException {
		if (countiesSelected == null || forceLoad) {
			CountyInfoData seedData = new CountyInfoData();
			seedData.setLid(lid);

			countiesSelected = HydroDBDataManager.getInstance().getData(
					seedData);
		}

		Collections.sort(countiesSelected);

		return countiesSelected;
	}

	/**
	 * Gets the available zones.
	 * 
	 * @return The zones available.
	 * @throws VizException
	 */
	public ArrayList<EligZoneData> getZonesAvailable() throws VizException {
		if (zonesAvailable == null) {
			zonesAvailable = HydroDBDataManager.getInstance().getData(
					EligZoneData.class);
		}

		return zonesAvailable;
	}

	/**
	 * Gets the selected Zones.
	 * 
	 * @return The zones selected.
	 * @throws VizException
	 */
	public ArrayList<ZoneInfoData> getZonesSelected() throws VizException {
		return getZonesSelected(false);
	}

	/**
	 * Gets the selected Zones.
	 * 
	 * @return The zones selected.
	 * @throws VizException
	 */
	public ArrayList<ZoneInfoData> getZonesSelected(boolean forceLoad)
			throws VizException {
		if (zonesSelected == null || forceLoad) {
			ZoneInfoData seedData = new ZoneInfoData();
			seedData.setLid(lid);

			zonesSelected = HydroDBDataManager.getInstance().getData(seedData);
		}

		Collections.sort(zonesSelected);

		return zonesSelected;
	}

	/**
	 * Adds the available county to the selected counties.
	 * 
	 * @param selectedAvailableCounty
	 *            The index of the selected available county to select.
	 */
	public void addSelectedCounty(int selectedAvailableCounty) {
		CountiesData availableCounty = countiesAvailable
				.get(selectedAvailableCounty);

		CountyInfoData countyToAdd = new CountyInfoData();
		countyToAdd.setLid(lid);
		countyToAdd.setState(availableCounty.getState());
		countyToAdd.setCounty(availableCounty.getCounty());
		countyToAdd.setCountyNumber(availableCounty.getCountyNumber());

		if (!countiesSelected.contains(countyToAdd)) {
			countiesSelected.add(countyToAdd);
		}
	}

	/**
	 * Adds the available zone to the selected zones.
	 * 
	 * @param selectedAvailableZone
	 *            The index of the selected available zone to select.
	 */
	public void addSelectedZone(int selectedAvailableZone) {
		EligZoneData availableZone = zonesAvailable.get(selectedAvailableZone);

		ZoneInfoData zoneToAdd = new ZoneInfoData();
		zoneToAdd.setLid(lid);
		zoneToAdd.setState(availableZone.getState());
		zoneToAdd.setZoneNumber(availableZone.getZoneNumber());
		zoneToAdd.setDescription(availableZone.getDescription());

		if (!zonesSelected.contains(zoneToAdd)) {
			zonesSelected.add(zoneToAdd);
		}
	}

	/**
	 * Removes all existing counties for the station and inserts all of selected
	 * counties.
	 * 
	 * @throws VizException
	 */
	public void saveCounties() throws VizException {
		// Remove all counties for the lid
		CountyInfoData dataToDelete = new CountyInfoData();
		dataToDelete.setLid(lid);
		HydroDBDataManager.getInstance().deleteRecord(dataToDelete);

		// Insert the currently selected counties
		for (CountyInfoData currCounty : countiesSelected) {
			HydroDBDataManager.getInstance().putData(currCounty);
		}
	}

	/**
	 * Removes all existing zones for the station and inserts all of selected
	 * zones.
	 * 
	 * @throws VizException
	 */
	public void saveZones() throws VizException {
		// Remove all zones for the lid
		ZoneInfoData dataToDelete = new ZoneInfoData();
		dataToDelete.setLid(lid);
		HydroDBDataManager.getInstance().deleteRecord(dataToDelete);

		// Insert the currently selected zones
		for (ZoneInfoData currZone : zonesSelected) {
			HydroDBDataManager.getInstance().putData(currZone);
		}
	}

	/**
	 * Removes the selected county.
	 * 
	 * @param selectedIndex
	 *            The county to be removed.
	 */
	public void removeSelectedCounty(int selectedIndex) {
		countiesSelected.remove(selectedIndex);
	}

	/**
	 * Removes the selected zone.
	 * 
	 * @param selectedIndex
	 *            The zone to be removed.
	 */
	public void removeSelectedZone(int selectedIndex) {
		zonesSelected.remove(selectedIndex);
	}

	/**
	 * Removes all selected counties.
	 */
	public void clearSelectedCounties() {
		if (countiesSelected != null) {
			countiesSelected.clear();
		} else {
			countiesSelected = new ArrayList<CountyInfoData>();
		}
	}

	/**
	 * Removes all selected zones.
	 */
	public void clearSelectedZones() {
		if (zonesSelected != null) {
			zonesSelected.clear();
		} else {
			zonesSelected = new ArrayList<ZoneInfoData>();
		}
	}
}
