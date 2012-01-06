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
package com.raytheon.uf.viz.radarapps.core;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ListDialog;

import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.config.RadarType;
import com.raytheon.rcm.config.Util;
import com.raytheon.rcm.mqsrvr.ReqObj;
import com.raytheon.rcm.mqsrvr.ReplyObj.ConfigReply;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.radarapps.client.RcmSystem;


public class RadarApps {
	private static RcmSystem rcmSystem;
	public static RcmSystem getRcmSystem() {
		synchronized (RadarApps.class) {
			if (rcmSystem == null)
				rcmSystem = new RcmSystem();
		}
		return rcmSystem;
	}
	
	public static String chooseRpg(Shell shell, Boolean dedicatedRestriction, RadarType typeRestriction, boolean alwaysPrompt) {
		// Could have automatically updating list, but do we really want that?
		// have to filter anyway for dedicated...
		ConfigReply reply = (ConfigReply)
			getRcmSystem().sendCheckedAndHandled(new ReqObj.GetRadarConfig(), shell);
		if (reply == null)
			return null;
		
		ArrayList<String> rpgs = new ArrayList<String>();
		
		for (RadarConfig rc: reply.config) {
			if (dedicatedRestriction != null && 
					rc.isDedicated() != dedicatedRestriction.booleanValue())
				continue;
			if (typeRestriction != null && 
					Util.getRadarType(rc) != typeRestriction)
				continue;
			rpgs.add(rc.getRadarID());
		}
		
		if (rpgs.size() == 0)
			return null;
		else if (rpgs.size() == 1 && ! alwaysPrompt)
			return rpgs.get(0);
				
		ListDialog ld = new ListDialog(shell);
		ld.setMessage("Please select a RPG.");
		ld.setLabelProvider(new LabelProvider() {
			public String getText(Object element) {
				return super.getText(element).toUpperCase();
			}
		});
		ld.setContentProvider(new ArrayContentProvider());
		ld.setInput(rpgs.toArray());
		
		int result = ld.open();
		if (result == Window.OK)
			return (String) ld.getResult()[0];
		else
			return null;
	}
	
	private static final String METADATA_DB = "metadata";
	
	public static float[] getRadarLocation(String radar) {
		String query = "SELECT lat, lon, elevmeter FROM radar_spatial WHERE rda_id = '" +
			radar.toUpperCase() + "';";
		List<Object[]> rows;
		try {
			rows = DirectDbQuery.executeQuery(query, METADATA_DB, QueryLanguage.SQL);
		} catch (VizException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
		}
		if (rows.size() > 0) {
			Object[] row = rows.get(0);
			float[] result = new float[3];
			for (int i = 0; i < result.length; ++i) {
				if (row[i] instanceof String) {
					// TODO: Can this happen?
					result[i] = Float.parseFloat((String) row[i]); 
				} else {
					result[i] = ((Number) row[i]).floatValue();
				}
			}
			return result;
		}
		return null;
	}
	
}
