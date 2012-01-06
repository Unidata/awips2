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
package com.raytheon.viz.mpe.ui;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.action.MenuManager;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 1, 2011            rgeorge     Initial creation
 *
 * </pre>
 *
 * @author rgeorge
 * @version 1.0	
 */

public class RFCPrecipFieldsPopulator extends FieldsPopulator {

	private static MenuManager menuMgr = new MenuManager("PrecipFields",
			"com.raytheon.viz.mpe.PrecipFields");

	private static DisplayFieldData[] menuItems = new DisplayFieldData[] {
			DisplayFieldData.rfcbMosaic, DisplayFieldData.rfcmMosaic,
			DisplayFieldData.rfcMosaic };

	private static Map<DisplayFieldData, MenuData> textMap = new HashMap<DisplayFieldData, MenuData>();
	static {
		textMap.put(DisplayFieldData.rfcbMosaic, new MenuData(
				"RFC Field Bias Mosaic", "B"));
		textMap.put(DisplayFieldData.rfcmMosaic, new MenuData(
				"RFC Multisensor Mosaic", "i"));
		textMap.put(DisplayFieldData.rfcMosaic, new MenuData("RFC QPE Mosaic",
				"C"));
	}

	@Override
	protected Map<DisplayFieldData, MenuData> getTexMap() {
		return RFCPrecipFieldsPopulator.textMap;
	}

	@Override
	protected DisplayFieldData[] getMenuItems() {
		return RFCPrecipFieldsPopulator.menuItems;
	}

	@Override
	protected MenuManager getMenuManger() {
		return RFCPrecipFieldsPopulator.menuMgr;
	}
}
