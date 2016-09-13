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
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.databinding.observable.list.WritableList;

import com.raytheon.rcm.config.RadarType;
import com.raytheon.rcm.products.ElevationInfo;
import com.raytheon.rcm.products.ElevationInfo.VCPInfo;
import com.raytheon.rcm.request.Request;
import com.raytheon.rcm.request.RpsList;
import com.raytheon.rcm.request.RpsListFormatter;

/**
 * Not an Eclipse editor. Main container class for an RPS list editing session.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ??? ??, 20??            ????????     Initial creation
 * Mar 28, 2016  #5511     dgilling     Renamed from ListEditor, code cleanup.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */
public class RpsListRequestContainer {

    private final WritableList requestList;

    private boolean dirty;

    private boolean untitled;

    private File path;

    private int vcp;

    private String radarID;

    public RpsListRequestContainer() {
        this.requestList = new WritableList();
        this.path = null;

        Collection<VCPInfo> vcpInfo = ElevationInfo.getInstance().getVcpInfo();
        this.vcp = (vcpInfo.isEmpty()) ? -1 : vcpInfo.iterator().next().vcp;

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

    public RpsList getRpsList() {
        RpsList list = new RpsList(ElevationInfo.getInstance().getOpModeForVcp(
                getVcp()), getVcp(), (Request[]) getRequestList().toArray(
                new Request[0]));
        return list;
    }

    public WritableList getRequestList() {
        return requestList;
    }

    public void add(Request req) {
        getRequestList().add(req);
        setDirty(true);
    }

    public void removeAll(List<?> list) {
        setDirty(getRequestList().removeAll(list));
    }

    public void setItem(int i, Request newReq) {
        getRequestList().set(i, newReq);
        setDirty(true);
    }

    private void setVcp(int vcp) {
        this.vcp = vcp;
    }

    public int getVcp() {
        return vcp;
    }

    private void setRadarID(String radarID) {
        this.radarID = radarID;
    }

    public String getRadarID() {
        return radarID;
    }

    private void setPath(File path) {
        this.path = path;
    }

    public File getPath() {
        return path;
    }

    public boolean isTdwrVcp(int vcp) {
        return vcp == 80 || vcp == 90;
    }

    public RadarType getTypeRestriction() {
        if (vcp > 0) {
            return isTdwrVcp(vcp) ? RadarType.TDWR : RadarType.WSR;
        } else {
            return null;
        }
    }

    private void setDirty(boolean dirty) {
        this.dirty = dirty;
    }

    public boolean isDirty() {
        return dirty;
    }

    private void setUntitled(boolean untitled) {
        this.untitled = untitled;
    }

    public boolean isUntitled() {
        return untitled;
    }

    public boolean saveList() throws FileNotFoundException, IOException {
        if (path.getName().length() == 0 || isUntitled()) {
            return false;
        }

        try (OutputStream outStream = new FileOutputStream(path);
                PrintWriter out = new PrintWriter(outStream)) {
            RpsListFormatter.formatAwips1RpsList(getRpsList(), path.getName(),
                    out);
        }
        setDirty(false);

        return true;
    }

    public boolean saveList(File file) throws FileNotFoundException,
            IOException {
        setPath(file);
        setUntitled(false);
        return saveList();
    }

    public void replaceList(RpsList newList, String radarID, File file) {
        replaceList(newList, radarID, file, false);
    }

    public void replaceList(RpsList newList, String radarID, File file,
            boolean isDummyFileName) {
        newList(radarID, newList.getVcp());
        getRequestList().addAll(Arrays.asList(newList.getRequests()));
        setPath(file);
        setUntitled(isDummyFileName);
    }
}
