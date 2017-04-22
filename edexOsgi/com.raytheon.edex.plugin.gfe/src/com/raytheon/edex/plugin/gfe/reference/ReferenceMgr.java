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
package com.raytheon.edex.plugin.gfe.reference;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.region.RegionLookup;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Manages reference sets for the ifpServer.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 24, 2012            dgilling    Initial creation
 * Aug 07, 2013       1561 njensen     Use pm.listFiles() instead of pm.listStaticFiles()
 * Sep 30, 2013       2361 njensen     Use JAXBManager for XML
 * Sep 08, 2104       3592 randerso    Changed to use new pm listStaticFiles()
 * Feb 19, 2015       4125 rjpeter     Fix jaxb performance issue
 * Apr 10, 2015       4383 dgilling    Fix getData so it searches correct localization
 *                                     directories for secondary sites.
 * Jan 27, 2016       5237 tgurney     Remove deprecated LocalizationFile
 *                                     method call
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class ReferenceMgr {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReferenceMgr.class);

    private static final String EDIT_AREAS_DIR = FileUtil.join("gfe",
            "editAreas");

    private final IPathManager pathMgr;

    private final GridLocation dbGridLocation;

    public ReferenceMgr(final IFPServerConfig config) {
        this.pathMgr = PathManagerFactory.getPathManager();
        this.dbGridLocation = config.dbDomain();
    }

    /**
     * Returns the inventory of data on disk.
     * 
     * @return <code>ReferenceID</code>s for every defined edit area on disk.
     */
    public ServerResponse<List<ReferenceID>> getInventory() {
        List<ReferenceID> refIDs = new ArrayList<ReferenceID>();
        LocalizationFile[] contents = pathMgr.listStaticFiles(
                getSiteSearchContexts(), EDIT_AREAS_DIR,
                new String[] { ".xml" }, false, true);
        if (contents != null) {
            for (LocalizationFile lf : contents) {
                String s = LocalizationUtil.extractName(lf.getPath());
                String area = s.replace(".xml", "");
                refIDs.add(new ReferenceID(area, lf.isProtected(), lf
                        .getContext().getLocalizationLevel()));
            }
        }

        statusHandler.debug("ReferenceID inventory: " + refIDs);

        ServerResponse<List<ReferenceID>> sr = new ServerResponse<List<ReferenceID>>();
        sr.setPayload(refIDs);
        return sr;
    }

    /**
     * Retrieves the specified <code>ReferenceData</code>, which is identified
     * by <code>ReferenceID</code>s, and returns it to the caller.
     * 
     * @param ids
     *            <code>ReferenceID</code>s of the data to retrieve.
     * @return The requested <code>ReferenceData</code>.
     */
    public ServerResponse<List<ReferenceData>> getData(
            final List<ReferenceID> ids) {
        ServerResponse<List<ReferenceData>> sr = new ServerResponse<List<ReferenceData>>();
        List<ReferenceData> data = new ArrayList<ReferenceData>();
        SingleTypeJAXBManager<ReferenceData> jaxbManager = ReferenceData
                .getJAXBManager();

        // process each ReferenceID requested
        for (ReferenceID id : ids) {
            String path = FileUtil.join(EDIT_AREAS_DIR, id.getName() + ".xml");
            LocalizationFile lf = pathMgr.getStaticLocalizationFile(
                    getSiteSearchContexts(), path);

            // does it exist?
            if (lf == null) {
                sr.addMessage("Unable to find reference data [" + id + "]");
                data = Collections.emptyList();
                sr.setPayload(data);
                return sr;
            }

            // open and read the file
            ReferenceData refData = null;
            try {
                refData = jaxbManager.unmarshalFromXmlFile(lf.getFile()
                        .getPath());
            } catch (Exception e) {
                sr.addMessage("Unable to read reference data [" + id + "]");
                data = Collections.emptyList();
                sr.setPayload(data);
                return sr;
            }

            // assemble the actual ReferenceID, with the protect and access
            // flags
            ReferenceID referenceID = new ReferenceID(id.getName(),
                    lf.isProtected(), lf.getContext().getLocalizationLevel());

            // assemble the ReferenceData
            refData.setGloc(dbGridLocation);
            refData.setId(referenceID);
            data.add(refData);
        }

        sr.setPayload(data);
        return sr;
    }

    @Override
    public String toString() {
        return "ReferenceMgr [" + dbGridLocation.getSiteId() + "]";
    }

    private LocalizationContext[] getSiteSearchContexts() {
        String siteId = dbGridLocation.getSiteId();
        /*
         * regionName could be returned as null, and if so, we will not add it
         * to the final list of LocalizationContexts.
         */
        String regionName = RegionLookup.getWfoRegion(siteId);

        LocalizationContext[] searchContexts = pathMgr
                .getLocalSearchHierarchy(LocalizationType.COMMON_STATIC);
        List<LocalizationContext> fixedContexts = new ArrayList<>(
                searchContexts.length);
        for (LocalizationContext ctx : searchContexts) {
            if (((ctx.getLocalizationLevel().equals(LocalizationLevel.SITE)) || (ctx
                    .getLocalizationLevel()
                    .equals(LocalizationLevel.CONFIGURED)))
                    && (!ctx.getContextName().equals(siteId))) {
                ctx.setContextName(siteId);
            } else if (ctx.getLocalizationLevel().equals(
                    LocalizationLevel.REGION)) {
                if (regionName == null) {
                    continue;
                } else if (!ctx.getContextName().equals(regionName)) {
                    ctx.setContextName(regionName);
                }
            }

            fixedContexts.add(ctx);
        }

        return fixedContexts.toArray(new LocalizationContext[fixedContexts
                .size()]);
    }
}