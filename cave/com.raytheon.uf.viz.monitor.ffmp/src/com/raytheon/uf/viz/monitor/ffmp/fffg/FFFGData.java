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
package com.raytheon.uf.viz.monitor.ffmp.fffg;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPCounties;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPCounty;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates.MODE;
import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.monitor.xml.FFMPRunXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.viz.core.localization.LocalizationManager;

/**
 * Forced Flash Flood Guidace Dialog data class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 13, 2011 11007      mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class FFFGData implements Runnable {
    private FFMPTemplates templates;

    private FFMPRunXML runner = null;

    private FFMPCounties counties;
    
    private IFFFGData callback;
    
    private LinkedHashMap<String, SrcDisplayDurationData> guidData = null;

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Runnable#run()
     */
    @Override
    public void run() {
        guidData = new LinkedHashMap<String, SrcDisplayDurationData>();

        getTemplates();
        getCounties();
        getGuidData();
        callback.setCounties(counties);
        callback.setTemplates(templates);
        callback.setGuidances(guidData);
        callback.dataLoadComplete();
    }

    /**
     * Get the FFMPTemplates.
     */
    private FFMPTemplates getTemplates() {
        if (templates == null) {
            String siteName = LocalizationManager.getInstance()
                    .getCurrentSite().toUpperCase();
            FFMPRunConfigurationManager frcm = FFMPRunConfigurationManager
                    .getInstance();
            runner = frcm.getRunner(siteName);

            this.templates = FFMPTemplates.getInstance(
                    runner.getPrimaryDomain(), MODE.CAVE);

            // backup domains
            ArrayList<DomainXML> backupDomainList = runner.getBackupDomains();
            if (backupDomainList != null) {
                for (DomainXML backup : backupDomainList) {
                    templates.addDomain(backup);
                }
            } else {
                templates.done = true;
            }
        }

        return templates;
    }

    /**
     * Populate the counties data.
     */
    private void getCounties() {

        if (runner == null) {
            String cwaName = LocalizationManager.getInstance().getCurrentSite()
                    .toUpperCase();
            // load a runner by finding the primary domain
            FFMPRunConfigurationManager frcm = FFMPRunConfigurationManager
                    .getInstance();
            runner = frcm.getRunner(cwaName);
        }
        for (ProductRunXML product : runner.getProducts()) {
            FFMPCounties countyList = getTemplates().getCounties(
                    product.getProductKey());
            if (counties == null) {
                counties = countyList;
                counties.getCounties().addAll(countyList.getCounties());
            } else {
                for (FFMPCounty county : countyList.getCounties()) {
                    if (!counties.getCounties().contains(county)) {
                        counties.addCounty(county);
                    }
                }
            }
            // Create a HashSet which allows no duplicates
            HashSet<FFMPCounty> hashSet = new HashSet<FFMPCounty>(
                    counties.getCounties());

            // Assign the HashSet to a new ArrayList
            ArrayList<FFMPCounty> newCounties = new ArrayList<FFMPCounty>(
                    hashSet);

            // Ensure correct order, since HashSet doesn't
            Collections.sort(newCounties);
            counties.setCounties(newCounties);
        }
    }
    
    /**
     * Get the Guidance data for the FFFG dialog.
     */
    private void getGuidData() {
        FFMPSourceConfigurationManager srcConfigMgr = FFMPSourceConfigurationManager.getInstance();
        ArrayList<String> guidances = srcConfigMgr.getGuidances();
        
        for (String guidance: guidances) {
            SourceXML source = srcConfigMgr.getSource(guidance);
            double dur = source.getDurationHour();
            String dispName = source.getDisplayName();
            
            SrcDisplayDurationData sddd = new SrcDisplayDurationData(dispName, dur);
            guidData.put(source.getSourceName(), sddd);
        }
    }

    /**
     * Set the callback for this class.
     * 
     * @param IFFFGData FFFGData interface callback
     */
    public void setCallback(IFFFGData callback) {
        this.callback = callback;
    }
}
