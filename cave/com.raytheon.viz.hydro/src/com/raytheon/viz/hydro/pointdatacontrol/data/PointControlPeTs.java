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
package com.raytheon.viz.hydro.pointdatacontrol.data;

import java.util.ArrayList;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants;
import com.raytheon.viz.hydro.pointdatacontrol.db.PDCDataManager;
import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;

/**
 * Structure used for managing the scrolled lists in the GUI; including the
 * PhysicalElement, TypeSource, and DataSource lists
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 21, 2008            mpduff     Initial creation
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PointControlPeTs {
    private static int procAsObs = 1;

    private static PointControlPeTs pcPeTsRef = null;

    /** Number of river items */
    private int nriver = 0;

    /** Number of rain items */
    private int nrain = 0;

    /** Number of snow items */
    private int nsnow = 0;

    /** Number of temp items */
    private int ntemp = 0;

    private int elementCount = PDCConstants.MISSING_VALUE;

    private static String elementTypeText = null;

    /** Holds the PE followed by the TS */
    private static ArrayList<String[]> orgBuf = null;

    private static ArrayList<String> riverBuf = new ArrayList<String>();

    private static ArrayList<String> rainBuf = new ArrayList<String>();

    private static ArrayList<String> snowBuf = new ArrayList<String>();

    private static ArrayList<String> tempBuf = new ArrayList<String>();

    private static String otherBuf = null;

    /** current unique active pe list */
    private static String elementBuffer = null;

    /** current unique active ts list */
    private static String[] adhocTypeSourceBuffer = null;

    private static String[] adhocDataSrcBuf = null;

    private static String[] timestepTypeSourceBuffer = null;

    private String shefpost = null;

    private boolean postLatest = false;

    private PointControlPeTs() {
    }

    public static synchronized PointControlPeTs getInstance() {
        if (pcPeTsRef == null) {
            pcPeTsRef = new PointControlPeTs();
            initialize();
        }

        return pcPeTsRef;
    }

    private static void initialize() {
        PDCDataManager dataManager = PDCDataManager.getInstance();
        PDCOptionData pcOptions = PDCOptionData.getInstance();

        if (pcOptions.getProcessMode() == 1) {
            procAsObs = 1;
        } else {
            procAsObs = 0;
        }

        try {
            ArrayList<String> result = dataManager.getTelmType(null);
            adhocDataSrcBuf = new String[result.toArray().length];
            adhocDataSrcBuf = result.toArray(new String[result.size()]);

            // ArrayList<String[]> peTsList = dataManager.getPeTs();
            orgBuf = dataManager.getPeTs();
            // orgBuf = peTsList;
            // nitems = orgBuf.size();

            // ArrayList<String> riverList = new ArrayList<String>();
            // ArrayList<String> rainList = new ArrayList<String>();
            // ArrayList<String> snowList = new ArrayList<String>();
            // ArrayList<String> tempList = new ArrayList<String>();

            /* add pe = Primary and pe = "PC and PP" to physical element list */
            riverBuf.add("Primary");
            riverBuf.add("PC and PP");

            /*
             * count how many items are in the pe-ts lists for each data type
             * and load the items into the permanent buffers. Check whether
             * processed observations are being comingled with regular
             * observations or if they are being treated separately.
             */
            if (pcOptions.getProcessMode() == procAsObs) {
                for (int i = 0; i < orgBuf.size(); i++) {
                    if (orgBuf.get(i)[0].startsWith("H")
                            || orgBuf.get(i)[0].startsWith("Q")) {
                        riverBuf.add(orgBuf.get(i)[0]);
                    } else if (orgBuf.get(i)[0].startsWith("PA")
                            || orgBuf.get(i)[0].startsWith("PE")
                            || orgBuf.get(i)[0].startsWith("PD")
                            || orgBuf.get(i)[0].startsWith("PL")) {
                        rainBuf.add(orgBuf.get(i)[0]);
                    } else if (orgBuf.get(i)[0].startsWith("S")) {
                        snowBuf.add(orgBuf.get(i)[0]);
                    } else if (orgBuf.get(i)[0].startsWith("T")) {
                        tempBuf.add(orgBuf.get(i)[0]);
                    }
                }
            } else {
                for (int i = 0; i < orgBuf.size(); i++) {
                    if ((orgBuf.get(i)[0].startsWith("H") || orgBuf.get(i)[0]
                            .startsWith("Q"))
                            && !orgBuf.get(i)[1].startsWith("P")) {
                        riverBuf.add(orgBuf.get(i)[0]);
                    } else if (orgBuf.get(i)[0].startsWith("P")
                            && !orgBuf.get(i)[1].startsWith("P")) {
                        char ch = orgBuf.get(i)[0].charAt(1);
                        if ((ch != 'A') && (ch != 'E') && (ch != 'D')
                                && (ch != 'L')) {
                            rainBuf.add(orgBuf.get(i)[0]);
                        }
                    } else if (orgBuf.get(i)[0].startsWith("S")
                            && !orgBuf.get(i)[1].startsWith("P")) {
                        snowBuf.add(orgBuf.get(i)[0]);
                    } else if (orgBuf.get(i)[0].startsWith("T")
                            && !orgBuf.get(i)[1].startsWith("P")) {
                        tempBuf.add(orgBuf.get(i)[0]);
                    }
                }
            }
        } catch (VizException e) {
            e.printStackTrace();
        }
    }

    public boolean checkShefPostLatest() {
        if (shefpost == null) {
            shefpost = AppsDefaults.getInstance().getToken("shef_post_latest");
            if (shefpost.equalsIgnoreCase("Off")) {
                postLatest = false;
            } else {
                postLatest = true;
            }
        }

        return postLatest;
    }

    /**
     * @return the nitems
     */
    public int getNitems() {
        return orgBuf.size();
    }

    /**
     * @return the nriver
     */
    public int getNriver() {
        return nriver;
    }

    /**
     * @param nriver
     *            the nriver to set
     */
    public void setNriver(int nriver) {
        this.nriver = nriver;
    }

    /**
     * @return the nrain
     */
    public int getNrain() {
        return nrain;
    }

    /**
     * @param nrain
     *            the nrain to set
     */
    public void setNrain(int nrain) {
        this.nrain = nrain;
    }

    /**
     * @return the nsnow
     */
    public int getNsnow() {
        return nsnow;
    }

    /**
     * @param nsnow
     *            the nsnow to set
     */
    public void setNsnow(int nsnow) {
        this.nsnow = nsnow;
    }

    /**
     * @return the ntemp
     */
    public int getNtemp() {
        return ntemp;
    }

    /**
     * @param ntemp
     *            the ntemp to set
     */
    public void setNtemp(int ntemp) {
        this.ntemp = ntemp;
    }

    /**
     * @return the elementCount
     */
    public int getElementCount() {
        return elementCount;
    }

    /**
     * @param elementCount
     *            the elementCount to set
     */
    public void setElementCount(int elementCount) {
        this.elementCount = elementCount;
    }

    /**
     * @return the adhocTypeSourceCount
     */
    public int getAdhocTypeSourceCount() {
        return adhocTypeSourceBuffer.length;
    }

    /**
     * @return the elementTypeText
     */
    public String getElementTypeText() {
        return elementTypeText;
    }

    /**
     * @param elementTypeText
     *            the elementTypeText to set
     */
    public void setElementTypeText(String elementTypeText) {
        PointControlPeTs.elementTypeText = elementTypeText;
    }

    /**
     * @return the orgBuf
     */
    public ArrayList<String[]> getOrgBuf() {
        return orgBuf;
    }

    /**
     * @param orgBuf
     *            the orgBuf to set
     */
    public void setOrgBuf(ArrayList<String[]> orgBuf) {
        PointControlPeTs.orgBuf = orgBuf;
    }

    /**
     * @return the riverBuf
     */
    public ArrayList<String> getRiverBuf() {
        return riverBuf;
    }

    /**
     * @param riverBuf
     *            the riverBuf to set
     */
    public void setRiverBuf(ArrayList<String> riverBuf) {
        PointControlPeTs.riverBuf = riverBuf;
    }

    /**
     * @return the rainBuf
     */
    public ArrayList<String> getRainBuf() {
        return rainBuf;
    }

    /**
     * @param rainBuf
     *            the rainBuf to set
     */
    public void setRainBuf(ArrayList<String> rainBuf) {
        PointControlPeTs.rainBuf = rainBuf;
    }

    /**
     * @return the snowBuf
     */
    public ArrayList<String> getSnowBuf() {
        return snowBuf;
    }

    /**
     * @param snowBuf
     *            the snowBuf to set
     */
    public void setSnowBuf(ArrayList<String> snowBuf) {
        PointControlPeTs.snowBuf = snowBuf;
    }

    /**
     * @return the tempBuf
     */
    public ArrayList<String> getTempBuf() {
        return tempBuf;
    }

    /**
     * @param tempBuf
     *            the tempBuf to set
     */
    public void setTempBuf(ArrayList<String> tempBuf) {
        PointControlPeTs.tempBuf = tempBuf;
    }

    /**
     * @return the otherBuf
     */
    public String getOtherBuf() {
        return otherBuf;
    }

    /**
     * @param otherBuf
     *            the otherBuf to set
     */
    public void setOtherBuf(String otherBuf) {
        PointControlPeTs.otherBuf = otherBuf;
    }

    /**
     * @return the elementBuffer
     */
    public String getElementBuffer() {
        return elementBuffer;
    }

    /**
     * @param elementBuffer
     *            the elementBuffer to set
     */
    public void setElementBuffer(String elementBuffer) {
        PointControlPeTs.elementBuffer = elementBuffer;
    }

    /**
     * @return the adhotTypeSourceBuffer
     */
    public String[] getAdhocTypeSourceBuffer() {
        return adhocTypeSourceBuffer;
    }

    /**
     * @param adhotTypeSourceBuffer
     *            the adhotTypeSourceBuffer to set
     */
    public void setAdhocTypeSourceBuffer(String[] adhocTypeSourceBuffer) {
        PointControlPeTs.adhocTypeSourceBuffer = adhocTypeSourceBuffer;
    }

    /**
     * @return the adhocDataSrcBuf
     */
    public String[] getAdhocDataSrcBuf() {
        return adhocDataSrcBuf;
    }

    /**
     * @param adhocDataSrcBuf
     *            the adhocDataSrcBuf to set
     */
    public void setAdhocDataSrcBuf(String[] adhocDataSrcBuf) {
        PointControlPeTs.adhocDataSrcBuf = adhocDataSrcBuf;
    }

    /**
     * @return the timestepTypeSourceBuffer
     */
    public String[] getTimestepTypeSourceBuffer() {
        return timestepTypeSourceBuffer;
    }

    /**
     * @param timestepTypeSourceBuffer
     *            the timestepTypeSourceBuffer to set
     */
    public void setTimestepTypeSourceBuffer(String[] timestepTypeSourceBuffer) {
        PointControlPeTs.timestepTypeSourceBuffer = timestepTypeSourceBuffer;
    }

    /**
     * @return the timestepTypeSourceCount
     */
    public int getTimestepTypeSourceCount() {
        return timestepTypeSourceBuffer.length;
    }
}
