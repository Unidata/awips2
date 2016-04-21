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
package com.raytheon.uf.viz.radarapps.rps;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;

import org.eclipse.core.databinding.observable.list.WritableList;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.rcm.config.RadarType;
import com.raytheon.rcm.products.ElevationInfo;
import com.raytheon.rcm.request.Request;
import com.raytheon.rcm.request.RpsList;
import com.raytheon.rcm.request.RpsListFormatter;


/** Not an Eclipse editor.  Main container class for an RPS list editing
 * session.
 * 
 */
public class ListEditor {
	ListEditorWindow window;
	private boolean dirty;
	private boolean untitled;
	private File path = new File("");
	private int vcp;
	private String radarID;
	private WritableList requestList;
	
	public ListEditor() {
		vcp = -1;
		requestList = new WritableList();
		window = new ListEditorWindow(this);
		int vcp = -1;
		try {
			vcp = ElevationInfo.getInstance().getVcpInfo().iterator().next().vcp;
		} catch (RuntimeException e) {
			// nothing
		}
		newList(null, vcp);
	}
	
	public void newList(String radarID, int vcp) {
		setRadarID(radarID);
		setVcp(vcp);
		getRequestList().clear();
		String name = String.format("Untitled.%sVCP%d.rps",
				radarID != null ? radarID.toUpperCase() + "." : "", getVcp());
		setPath(new File(name));
		setUntitled(true);
		setDirty(false);
	}

	public void startEditor() {
		window.open();
	}
	
	public void dispose() {
		window.shell.dispose();
	}
	
	public Shell getShell() {
		if (window != null)
			return window.shell;
		return null;
	}

	public RpsList getRpsList() {
		RpsList list = new RpsList(
		        ElevationInfo.getInstance().getOpModeForVcp(getVcp()),
		        getVcp(), 
		        (Request[]) getRequestList().toArray(new Request[0]));
		return list;
	}

	public WritableList getRequestList() {
		return requestList;
	}

	private void updateTitle() {
		StringBuilder sb = new StringBuilder();
		sb.append("RPS List Editor: ");
		if (isDirty())
			sb.append("* ");
		if (path != null)
			sb.append(path.getName());
		getShell().setText(sb.toString());
	}

	public void setVcp(int vcp) {
		this.vcp = vcp;
	}

	public int getVcp() {
		return vcp;
	}

	public void setRadarID(String radarID) {
		this.radarID = radarID;
	}

	public String getRadarID() {
		return radarID;
	}

	public void setPath(File path) {
		this.path = path;
		updateTitle();
	}

	public File getPath() {
		return path;
	}

	public boolean isTdwrVcp(int vcp) {
		return vcp == 80 || vcp == 90;
	}
	
	public RadarType getTypeRestriction() {
		if (vcp > 0)
			return isTdwrVcp(vcp) ? RadarType.TDWR : RadarType.WSR;
		else
			return null;
	}

	public void setDirty(boolean dirty) {
		this.dirty = dirty;
		updateTitle();
	}

	public boolean isDirty() {
		return dirty;
	}

	public void setUntitled(boolean untitled) {
		this.untitled = untitled;
	}

	public boolean isUntitled() {
		return untitled;
	}

	public boolean saveList() {
		if (path.getName().length() == 0 || isUntitled())
			return false;

		try {
			FileOutputStream fo = new FileOutputStream(path);
			try {
				PrintWriter p = new PrintWriter(fo);
				RpsListFormatter.formatAwips1RpsList(getRpsList(), path.getName(),p);
				p.flush();
			} finally {
				fo.close();
			}
		} catch (RuntimeException e) {
			window.showError(String.format("Could not save list: %s",
					e.getMessage()));
		} catch (IOException e) {
			window.showError(String.format("Could not write list to '%s: %s'",
					path, e.getMessage()));
			return false;
		}
		
		setDirty(false);
		return true;
	}
}
