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
package com.raytheon.uf.common.dataplugin.gfe.reference;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData.CoordinateType;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.region.RegionLookup;
import com.raytheon.uf.common.protectedfiles.ProtectedFileLookup;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

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
 * Jun 07, 2017       6298 mapeters    Moved from com.raytheon.edex.plugin.gfe.reference,
 *                                     added getData() with boolean flag parameters and
 *                                     edit area group handling
 * Aug 07, 2017       6379 njensen     Use ProtectedFileLookup
 * Jul 31, 2017       6342 randerso    Added save and delete edit area methods.
 *                                     Code cleanup.
 *
 * </pre>
 *
 * @author dgilling
 */

public class ReferenceMgr {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReferenceMgr.class);

    private static final String EDIT_AREAS_DIR = LocalizationUtil.join("gfe",
            "editAreas");

    private static final String EDIT_AREA_GROUPS_DIR = LocalizationUtil
            .join("gfe", "editAreaGroups");

    /** Edit area files extension */
    private static final String XML_EXT = ".xml";

    /** Edit area group files extension */
    private static final String TXT_EXT = ".txt";

    private final IPathManager pathMgr;

    private final GridLocation dbGridLocation;

    public ReferenceMgr(final GridLocation gridLoc) {
        this.pathMgr = PathManagerFactory.getPathManager();
        this.dbGridLocation = gridLoc;
    }

    /**
     * Get list of available edit area ReferenceIDs.
     *
     * @return the edit area ReferenceIDs
     */
    public ServerResponse<List<ReferenceID>> getInventory() {
        List<ReferenceID> refIDs = new ArrayList<>();
        IPathManager pm = PathManagerFactory.getPathManager();
        // Get all edit area files in the given siteIds search contexts
        LocalizationFile[] contents = pm.listStaticFiles(
                getSiteSearchContexts(), EDIT_AREAS_DIR,
                new String[] { XML_EXT }, false, true);
        if (contents != null) {
            // Extract edit area name and build ReferenceID for each file
            for (LocalizationFile lf : contents) {
                String s = LocalizationUtil.extractName(lf.getPath());
                String area = s.replace(XML_EXT, "");
                refIDs.add(new ReferenceID(area,
                        ProtectedFileLookup.isProtected(lf),
                        lf.getContext().getLocalizationLevel()));
            }
        }

        statusHandler.debug("ReferenceID inventory: " + refIDs);

        ServerResponse<List<ReferenceID>> sr = new ServerResponse<>();
        sr.setPayload(refIDs);
        return sr;
    }

    /**
     * Retrieves the specified <code>ReferenceData</code>, which is identified
     * by <code>ReferenceID</code>s, and returns it to the caller.
     *
     * @param id
     *            <code>ReferenceID</code>s of the data to retrieve.
     * @return The requested <code>ReferenceData</code>.
     */
    public ServerResponse<ReferenceData> getData(final ReferenceID id) {
        ServerResponse<List<ReferenceData>> sr = getData(Arrays.asList(id));
        ServerResponse<ReferenceData> srReal = new ServerResponse<>();
        if (sr.isOkay()) {
            srReal.setPayload(sr.getPayload().get(0));
        } else {
            srReal.setMessages(sr.getMessages());
        }
        return srReal;
    }

    /**
     * Retrieves the specified <code>ReferenceData</code>, which is identified
     * by name, and returns it to the caller.
     *
     * @param name
     *            name of the desired edit area
     * @return The requested <code>ReferenceData</code>.
     */
    public ServerResponse<ReferenceData> getEditArea(String name) {
        return getData(new ReferenceID(name));
    }

    /**
     * Get list of edit area ReferenceData corresponding to the given
     * ReferenceIDs. If one of the requested ReferenceIDs doesn't exist, an
     * empty list with an error message will be returned. Query-based reference
     * data will be retrieved.
     *
     * @param ids
     *            the edit area ids to get data for
     * @return the edit area ReferenceData
     */
    public ServerResponse<List<ReferenceData>> getData(
            final Collection<ReferenceID> ids) {
        return getData(ids, false, true);
    }

    /**
     * Get list of edit area ReferenceData corresponding to the given
     * ReferenceIDs.
     *
     * @param ids
     *            the edit area ids to get data for
     * @param skipMissing
     *            how to handle ids for which the site doesn't have data: if
     *            true, simply skip such ids, otherwise immediately stop and
     *            return an empty list with an error message
     * @param allowQueries
     *            how to handle ids for which the reference data is query-based:
     *            if true, include such ids, otherwise immediately stop and
     *            return an empty list with an error message
     * @return the edit area ReferenceData
     */
    public ServerResponse<List<ReferenceData>> getData(
            final Collection<ReferenceID> ids, boolean skipMissing,
            boolean allowQueries) {
        List<ReferenceData> data = new ArrayList<>();
        SingleTypeJAXBManager<ReferenceData> jaxbManager = ReferenceData
                .getJAXBManager();

        LocalizationContext[] searchCtxs = getSiteSearchContexts();

        String siteId = dbGridLocation.getSiteId();

        // Process each ReferenceID requested
        for (ReferenceID id : ids) {
            String path = LocalizationUtil.join(EDIT_AREAS_DIR,
                    id.getName() + XML_EXT);
            LocalizationFile lf = pathMgr.getStaticLocalizationFile(searchCtxs,
                    path);

            // Check if it exists
            if (lf == null || !lf.exists()) {
                if (skipMissing) {
                    continue;
                }
                return ServerResponse
                        .errorResponseList("Unable to find reference data ["
                                + id + "] for site " + siteId);
            }

            // Open and read the file
            ReferenceData refData = null;
            try (InputStream is = lf.openInputStream()) {
                refData = jaxbManager.unmarshalFromInputStream(is);
            } catch (Exception e) {
                String msg = "Unable to read reference data [" + id
                        + "] for site " + siteId;
                statusHandler.handle(Priority.WARN, msg, e);
                return ServerResponse.errorResponseList(msg);
            }

            if (!allowQueries && refData.isQuery()) {
                return ServerResponse.errorResponseList(
                        "Unable to retrieve query-based reference data [" + id
                                + "] for site " + siteId);
            }

            // Assemble the actual ReferenceID with protect and access flags
            ReferenceID referenceID = new ReferenceID(id.getName(),
                    ProtectedFileLookup.isProtected(lf),
                    lf.getContext().getLocalizationLevel());

            // Assemble the ReferenceData
            refData.setId(referenceID);
            refData.setGloc(dbGridLocation);
            data.add(refData);
        }

        ServerResponse<List<ReferenceData>> sr = new ServerResponse<>();
        sr.setPayload(data);
        return sr;
    }

    /**
     * Save an edit area to the CONFIGURED level
     *
     * @param name
     *            edit area name to be used
     * @param refData
     *            ReferenceData for edit area
     */
    public void saveEditArea(String name, ReferenceData refData) {
        refData.getPolygons(CoordinateType.LATLON);

        String siteId = dbGridLocation.getSiteId();
        String path = LocalizationUtil.join(EDIT_AREAS_DIR, name + ".xml");

        LocalizationContext commonStaticConfig = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        commonStaticConfig.setContextName(siteId);
        LocalizationFile lf = pathMgr.getLocalizationFile(commonStaticConfig,
                path);
        try (SaveableOutputStream stream = lf.openOutputStream()) {
            ReferenceData.getJAXBManager().marshalToStream(refData, stream);
            stream.close();
            stream.save();
        } catch (IOException | LocalizationException
                | SerializationException e) {
            statusHandler.error("Error saving edit area: " + lf.toString(), e);
        }
    }

    /*
     * Delete an edit area
     *
     * @param name name of edit area to be deleted
     */
    public void deleteEditArea(String name) {
        String siteId = dbGridLocation.getSiteId();
        LocalizationContext commonStaticConfig = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        commonStaticConfig.setContextName(siteId);
        String path = LocalizationUtil.join(EDIT_AREAS_DIR, name + ".xml");
        LocalizationFile lf = pathMgr.getLocalizationFile(commonStaticConfig,
                path);
        try {
            lf.delete();
        } catch (LocalizationException e) {
            statusHandler.error("Error deleting edit area: " + lf.toString(),
                    e);
        }
    }

    /**
     * Get list of available edit area GroupIds.
     *
     * @return the GroupIDs
     */
    public ServerResponse<List<GroupID>> getGroupsInventory() {
        List<GroupID> groupIds = new ArrayList<>();
        IPathManager pm = PathManagerFactory.getPathManager();
        // Get all edit area group files in the given site's search contexts
        LocalizationFile[] contents = pm.listStaticFiles(
                getSiteSearchContexts(), EDIT_AREA_GROUPS_DIR,
                new String[] { TXT_EXT }, false, true);
        if (contents != null) {
            for (LocalizationFile lf : contents) {
                // Extract group name and build GroupID for each
                String s = LocalizationUtil.extractName(lf.getPath());
                String area = s.replace(TXT_EXT, "");
                groupIds.add(
                        new GroupID(area, ProtectedFileLookup.isProtected(lf),
                        lf.getContext().getLocalizationLevel()));
            }
        }

        ServerResponse<List<GroupID>> sr = new ServerResponse<>();
        sr.setPayload(groupIds);
        return sr;
    }

    /**
     * Get edit area ReferenceData corresponding to the given edit area
     * GroupIDs.
     *
     * @param groupIds
     *            the edit area groupIds to get data for
     * @param skipMissing
     *            how to handle edit areas for which the site doesn't have data:
     *            if true, simply skip such ids, otherwise immediately stop and
     *            return an empty list with an error message
     * @param allowQueries
     *            how to handle edit areas for which the reference data is
     *            query-based: if true, include such ids, otherwise immediately
     *            stop and return an empty list with an error message
     * @return map of group name to edit area ReferenceData
     */
    public ServerResponse<Map<String, List<ReferenceData>>> getGroupData(
            final Collection<GroupID> groupIds, boolean skipMissing,
            boolean allowQueries) {
        ServerResponse<Map<String, List<ReferenceData>>> rval = new ServerResponse<>();
        Map<String, List<ReferenceData>> groupDataMap = new HashMap<>();
        for (GroupID groupId : groupIds) {
            String groupName = groupId.getName();

            // Get list of edit areas within group
            List<ReferenceID> editAreaIds;
            try {
                editAreaIds = getGroupContents(groupName);
            } catch (IOException | LocalizationException e) {
                String msg = "Unable to read group file [" + groupName
                        + "] for site " + dbGridLocation.getSiteId();
                statusHandler.handle(Priority.WARN, msg, e);
                return ServerResponse.errorResponseMap(msg);
            }

            // Get data for contained edit areas
            ServerResponse<List<ReferenceData>> dataSr = getData(editAreaIds,
                    skipMissing, allowQueries);
            if (!dataSr.isOkay()) {
                return ServerResponse.errorResponseMap(dataSr.message());
            }
            // Add group to map
            groupDataMap.put(groupName, dataSr.getPayload());
        }

        rval.setPayload(groupDataMap);
        return rval;
    }

    /**
     * Get the edit area ids contained within the given group.
     *
     * @param groupName
     * @return edit area ids
     * @throws LocalizationException
     * @throws IOException
     */
    private List<ReferenceID> getGroupContents(String groupName)
            throws IOException, LocalizationException {
        List<ReferenceID> editAreas = new ArrayList<>();
        IPathManager pm = PathManagerFactory.getPathManager();
        String path = LocalizationUtil.join(EDIT_AREA_GROUPS_DIR,
                groupName + TXT_EXT);
        // Get group file from localization
        LocalizationFile lf = pm
                .getStaticLocalizationFile(getSiteSearchContexts(), path);
        if (lf != null && lf.exists()) {
            try (BufferedReader in = new BufferedReader(
                    new InputStreamReader(lf.openInputStream()))) {
                // First line is number of edit areas listed in file
                String countStr = in.readLine();
                int count = Integer.parseInt(countStr);
                // Read in specified number of edit areas
                for (int i = 0; i < count; ++i) {
                    String editArea = in.readLine();
                    if (editArea == null) {
                        // Count was wrong, EOF reached
                        break;
                    }
                    editAreas.add(new ReferenceID(editArea));
                }
            }
        }
        return editAreas;
    }

    /**
     * Get the localization search contexts for the given site.
     *
     * @param siteId
     *            the site to get search contexts for
     * @return the site's search contexts
     */
    private LocalizationContext[] getSiteSearchContexts() {
        String siteId = dbGridLocation.getSiteId();

        String regionName = RegionLookup.getWfoRegion(siteId);

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext[] searchContexts = pm
                .getLocalSearchHierarchy(LocalizationType.COMMON_STATIC);
        List<LocalizationContext> fixedContexts = new ArrayList<>(
                searchContexts.length);

        // Set context names on appropriate localization levels
        for (LocalizationContext ctx : searchContexts) {
            LocalizationLevel level = ctx.getLocalizationLevel();
            if (((level.equals(LocalizationLevel.SITE))
                    || (level.equals(LocalizationLevel.CONFIGURED)))) {
                ctx.setContextName(siteId);
            } else if (level.equals(LocalizationLevel.REGION)) {
                if (regionName == null) {
                    // Don't include REGION context if no regionName
                    continue;
                } else {
                    ctx.setContextName(regionName);
                }
            }

            fixedContexts.add(ctx);
        }

        return fixedContexts.toArray(new LocalizationContext[0]);
    }

    @Override
    public String toString() {
        return "ReferenceMgr [" + dbGridLocation.getSiteId() + "]";
    }
}