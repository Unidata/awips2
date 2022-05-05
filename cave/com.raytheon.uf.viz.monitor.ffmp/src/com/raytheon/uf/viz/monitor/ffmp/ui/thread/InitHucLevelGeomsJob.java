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
package com.raytheon.uf.viz.monitor.ffmp.ui.thread;

import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.dataplugin.ffmp.HucLevelGeometriesFactory;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Initialization job that initializes the huc level geometries in the
 * HucLevelGeometriesFactory's internal cache. This is to speed up overall
 * loading of the display so the FFMPResource's paintInternal() does not have to
 * wait on these geometries.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 11, 2013 2075           njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class InitHucLevelGeomsJob extends Job {

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(InitHucLevelGeomsJob.class);

    private String siteKey;

    private FFMPTemplates templates;

    private List<String> hucs;

    public InitHucLevelGeomsJob(String siteKey, FFMPTemplates templates,
            List<String> hucs) {
        super("Initializing HUC Level Geometries");
        this.setSystem(true);
        this.siteKey = siteKey;
        this.templates = templates;
        this.hucs = hucs;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        HucLevelGeometriesFactory hucGeomFactory = HucLevelGeometriesFactory
                .getInstance();
        for (DomainXML domain : templates.getDomains()) {
            String cwa = domain.getCwa();
            for (String huc : hucs) {
                try {
                    // since there's only one instance and it caches the
                    // results, this will speed up all future calls to the
                    // factory, ie speed up the initial display
                    hucGeomFactory.getGeometries(templates, siteKey, cwa, huc);
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error with early initialization of huc geometries: cwa="
                                    + cwa + ", huc=" + huc, e);
                }
            }
        }

        return Status.OK_STATUS;
    }

}
