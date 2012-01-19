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
package com.raytheon.edex.uengine.tasks.process;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.raytheon.edex.msg.Service;
import com.raytheon.edex.services.ArchiveSrv;
import com.raytheon.uf.common.message.response.ResponseMessageGeneric;

/**
 * This script task will send control messages to the ArchiveSrvs to allow for
 * the turning on and off of the Archive locations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 12/12/2007   561         dfitch      Initial Creation
 * 
 * </pre>
 * 
 * @author dfitch
 * @version 1
 */
public class ArchiveSrvControl {

    private static final String SET_ARCHIVE_DIR_LOCATION = "SET_ARCHIVE_DIR_LOCATION";

    private static final String SET_TEE_MODE = "SET_TEE_MODE";

    // private static final String ADD_ONE_TEE_MODE_CRONTAB =
    // "ADD_ONE_TEE_MODE_CRONTAB";

    // private static final String REMOVE_ONE_TEE_MODE_CRONTAB =
    // "REMOVE_ONE_TEE_MODE_CRONTAB";

    private static final String SET_RECORD_LOCATIONS = "SET_RECORD_LOCATIONS";

    private String archiveDirectoryLocation = null;

    private boolean teeModeOn = false;

    private String targetName = null;

    /**
     * What should happen when execute is invoked? There are several available
     * choices
     * 
     * <ul>
     * <li>null -- Just return a list of what IngestSrv objects are out there</li>
     * <li>change -- perform what ever changes have been asked for... then
     * return the list of IngestSrv objects are out there</li>
     * </ul>
     */
    private List<String> lstCommands = null;

    private String recordLocations;

    private String startTeeModeCrontab;

    private String stopTeeModeCrontab;

    public ArchiveSrvControl() {
        super();
        lstCommands = new ArrayList<String>();
    }

    /**
     * Replay the archive returning an XML string of some information about what
     * happened...
     * 
     */
    public Object execute() {
        List<ArchiveSrv> lstArchiveSrv = ArchiveSrv.getLstOfArchiveSrv();
        synchronized (lstArchiveSrv) {
            // logger.info("lstIngestSrv.size()=" + lstArchiveSrv.size());
            if ((null != lstCommands) && (lstCommands.size() > 0)) {
                for (Iterator<ArchiveSrv> it = lstArchiveSrv.iterator(); it
                        .hasNext();) {
                    ArchiveSrv archiveSrv = it.next();
                    if ((null == targetName)
                            || targetName
                                    .equalsIgnoreCase(archiveSrv.getName())) {

                        executeCommand(archiveSrv);

                    }
                }
            }
        }
        lstArchiveSrv = ArchiveSrv.getLstOfArchiveSrv();
        List<Service> serviceList = new ArrayList<Service>();
        for (Iterator<ArchiveSrv> it = lstArchiveSrv.iterator(); it.hasNext();) {
            ArchiveSrv archiveSrv = it.next();
            if ((null == targetName)
                    || targetName.equalsIgnoreCase(archiveSrv.getName())) {

                Service tmpSrv = new Service();
                tmpSrv.setArchiveDirectoryLocation(archiveSrv
                        .getArchiveDirectoryLocation());
                tmpSrv.setJmxModeOn(archiveSrv.isJmxModeOn());
                tmpSrv.setName(archiveSrv.getName());
                tmpSrv.setTeeModeOn(archiveSrv.isTeeModeOn());
                serviceList.add(tmpSrv);

            }
        }
        ResponseMessageGeneric response = new ResponseMessageGeneric(
                (ArrayList<Service>) serviceList);
        return response;

    }

    /**
     * @param archiveSrv
     */
    private void executeCommand(ArchiveSrv archiveSrv) {
        for (Iterator<String> it = lstCommands.iterator(); it.hasNext();) {

            String command = it.next();
            if (null == command) {

            } else if (SET_RECORD_LOCATIONS.equals(command)) {
                archiveSrv.setRecordLocations(recordLocations);
            } else if (SET_ARCHIVE_DIR_LOCATION.equals(command)) {
                archiveSrv
                        .setArchiveDirectoryLocation(archiveDirectoryLocation);
            } else if (SET_TEE_MODE.equals(command)) {
                archiveSrv.setTeeModeOn(teeModeOn);
            }

        }
    }

    /**
     * @return
     */
    // private String statusOfAllIngestSrv() {
    // String result = "<?xml version='1.0' encoding='ISO-8859-1'?>\n"
    // + "<IngestSrvs>\n";
    //
    // List<ArchiveSrv> lstArchiveSrv = ArchiveSrv.getLstOfArchiveSrv();
    // synchronized (lstArchiveSrv) {
    // logger.info("lstIngestSrv.size()=" + lstArchiveSrv.size());
    // for (Iterator<ArchiveSrv> it = lstArchiveSrv.iterator(); it
    // .hasNext();) {
    // ArchiveSrv archiveSrv = it.next();
    // String dirLocation = archiveSrv.getArchiveDirectoryLocation();
    // int execCount = archiveSrv.getExecCount();
    // String name = archiveSrv.getName();
    // int numMessagesCopied = archiveSrv.getNumMessagesCopied();
    // boolean t = archiveSrv.isTeeModeOn();
    // result += "<IngestSrv><name>" + name + "</name><execCount>"
    // + execCount + "</execCount><teeModeOn>" + t
    // + "</teeModeOn>" + "<numCopied>" + numMessagesCopied
    // + "</numCopied><dirLocation>" + dirLocation
    // + "</dirLocation></IngestSrv>\n";
    // }
    // }
    // result += "</IngestSrvs>\n";
    // return result;
    // }
    /**
     * @return the archiveDirectoryLocation
     */
    public String getArchiveDirectoryLocation() {
        return archiveDirectoryLocation;
    }

    /**
     * @param archiveDirectoryLocation
     *            the archiveDirectoryLocation to set
     */
    public void setArchiveDirectoryLocation(String archiveDirectoryLocation) {
        this.archiveDirectoryLocation = archiveDirectoryLocation;
        lstCommands.add(SET_ARCHIVE_DIR_LOCATION);
    }

    /**
     * @return the teeModeOn
     */
    public boolean isTeeModeOn() {
        return teeModeOn;
    }

    /**
     * @param teeModeOn
     *            the teeModeOn to set
     */
    public void setTeeModeOn(boolean teeModeOn) {
        lstCommands.add(SET_TEE_MODE);
        this.teeModeOn = teeModeOn;
    }

    /**
     * @return the targetName
     */
    public String getTargetName() {
        return targetName;
    }

    /**
     * @param targetName
     *            the targetName to set
     */
    public void setTargetName(String targetName) {
        this.targetName = targetName;
    }

    /**
     * @return the command
     */
    public List<String> getCommands() {
        return lstCommands;
    }

    /**
     * @return the startTeeModeCrontab
     */
    public String getStartTeeModeCrontab() {
        return startTeeModeCrontab;
    }

    /**
     * @param startTeeModeCrontab
     *            the startTeeModeCrontab to set
     */
    public void setStartTeeModeCrontab(String startTeeModeCrontab) {
        this.startTeeModeCrontab = startTeeModeCrontab;
    }

    /**
     * @return the stopTeeModeCrontab
     */
    public String getStopTeeModeCrontab() {
        return stopTeeModeCrontab;
    }

    /**
     * @param stopTeeModeCrontab
     *            the stopTeeModeCrontab to set
     */
    public void setStopTeeModeCrontab(String stopTeeModeCrontab) {
        this.stopTeeModeCrontab = stopTeeModeCrontab;
    }

    /**
     * @return the recordLocations
     */
    public String getRecordLocations() {
        return recordLocations;
    }

    /**
     * @param recordLocations
     *            the recordLocations to set
     */
    public void setRecordLocations(String recordLocations) {
        this.recordLocations = recordLocations;
        lstCommands.add(SET_RECORD_LOCATIONS);
    }

}
