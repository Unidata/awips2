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
package com.raytheon.uf.viz.radarapps.alertreq.impl;

public class AlertCategory {

	// group? but that is in the AAP message...
	public int id;
	public String name;
	public String units;
	public int nThresholdCodes;
	
	public AlertCategory(int id, String units, String name, int thresholdCodes) {
		super();
		this.id = id;
		this.name = name;
		this.units = units;
		this.nThresholdCodes = thresholdCodes;
	}

	private static final AlertCategory[] categories = {
		new AlertCategory( 1,  "kt",     "1  Grid Velocity", 6),
		new AlertCategory( 2,  "dBZ",    "2  Grid Comp Refl", 6),
		new AlertCategory( 3,  "kft",    "3  Grid Echo Tops", 4),
		new AlertCategory( 4,  "%",      "4  Grid SWP", 5),
		new AlertCategory( 6,  "kg/m2",  "6  Grid VIL", 6),
		new AlertCategory( 7,  "kt",     "7  Vol VAD", 6),
		new AlertCategory( 8,  "x.25in", "8  Vol Max Hail Size", 6),
		//new AlertCategory( 9,  "---",    "Mesocyclone"),// Obsolete
		new AlertCategory(10,  "---",    "10 Vol TVS", 2),
		new AlertCategory(11,  "dBZ",    "11 Vol Max Storm Ref", 6),
		new AlertCategory(12,  "%",      "12 Vol Prob Hail", 6),
		new AlertCategory(13,  "%",      "13 Vol Prob Svr Hail", 6),
		new AlertCategory(14,  "kft",    "14 Vol Storm Top", 6),
		new AlertCategory(15,  "x.1in",  "15 Vol Max 1hr PARA", 4),
		new AlertCategory(16,  "---",    "16 Vol MDA Strength Rank", 6),
		new AlertCategory(25,  "x.25in", "25 Fcst Max Hail Size", 6),
		//new AlertCategory(26,  "---",    "Mesocyclone"),// Obsolete
		new AlertCategory(27,  "---",    "27 Fcst TVS", 2),
		new AlertCategory(28,  "dBZ",    "28 Fcst Max Storm Ref", 6),
		new AlertCategory(29,  "%",      "29 Fcst Prob Hail", 6),
		new AlertCategory(30,  "%",      "30 Fcst Prob Svr Hail", 6),
		new AlertCategory(31,  "kft",    "31 Fcst Storm Top", 6),
		new AlertCategory(32,  "---",    "32 Fcst MDA Strength Rank", 6)		
	};

	public static AlertCategory[] getAlertCategories() {
		return categories;
	}
	
	public static AlertCategory getAlertCategory(int id) {
		for (AlertCategory ac : categories) {
			if (ac.id == id)
				return ac;
		}
		return null;
	}

	public int getId() {
		return id;
	}

	public String getName() {
		return name;
	}

	public String getUnits() {
		return units;
	}
}
