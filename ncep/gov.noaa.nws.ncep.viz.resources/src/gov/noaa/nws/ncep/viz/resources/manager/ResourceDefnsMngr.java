package gov.noaa.nws.ncep.viz.resources.manager;

import gov.noaa.nws.ncep.common.dataplugin.mcidas.McidasRecord;
import gov.noaa.nws.ncep.edex.plugin.mosaic.common.MosaicRecord;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationConstants;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationResourcePathConstants;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceExtPointMngr;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.ResourceType;

/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/09/10		  #273		Greg Hull	 Created
 * 08/31/10       #303	    Greg Hull    Update PGEN AttrSetGroups
 * 09/07/10       #307      Greg Hull    add dfltTimeRange, timelineGenMethod
 * 10/14/10	      #227		M. Li		 add EnsembleRscCategory
 * 11/27/10       #365      Greg Hull    dynamically generated resource types and sub-types
 * 02/08/11       #365      Greg Hull    dynamically generated local radars.
 * 02/28/11       #408      Greg Hull    Replace Forecast/Observed with a filter
 * 06/07/11       #445       Xilin Guo   Data Manager Performance Improvements
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

public class ResourceDefnsMngr {

    // one instance per user. (Not used)
    //
    private static Map<String, ResourceDefnsMngr> instanceMap = new HashMap<String, ResourceDefnsMngr>();

    private ResourceDefinitionTable resourceDefnTable = null;

    private HashMap<String, ResourceDefinition> resourceDefnsMap = null;

    private static File resourceImplementationsDir;

    private File resourceDefnFile = null;

    private File attrSetGroupsDir = null;

    // map from the rscType+groupName to the AttrSetGroup that
    // holds the list of attrSets
    private Map<String, AttrSetGroup> attrSetGroupsMap;

    // read dynamically based on what is in the resourceDefinitions.xml
    private static String[] AvailResourceCategories = {
            ResourceName.SatelliteRscCategory,
            ResourceName.RadarRscCategory,
            ResourceName.SurfaceRscCategory,
            ResourceName.UpperAirRscCategory,
            ResourceName.GridRscCategory,
            ResourceName.PGENRscCategory,
            ResourceName.MiscRscCategory,
            ResourceName.EnsembleRscCategory,
            ResourceName.OverlayRscCategory,
            // These are meant to allow the user to create categories like NMAP
            // w/o a code change if they
            // don't like the delivered version using SurfaceRscCategory &
            // UpperAirRscCategory
            ResourceName.SurfaceFcstRscCategory,
            ResourceName.SurfaceObsRscCategory,
            ResourceName.UpperAirFcstRscCategory,
            ResourceName.UpperAirObsRscCategory };

    private static List<String> availResourceCategoriesList = Arrays
            .asList(AvailResourceCategories);

    private static String RSC_IMPLS_DIR = "resourceImpls" + File.separator;

    private static String BNDL_TMPLTS_DIR = "bundleTemplates" + File.separator;

    private static String RSC_DEFNS_DIR = "resourceDefns" + File.separator;

    private static String RSC_DEFNS_FILENAME = "resourceDefinitions.xml";

    private static String ATTR_SET_GROUPS_DIR = "AttrSetGroups";

    private static String ATTR_SET_FILE_EXT = ".attr";

    public static synchronized ResourceDefnsMngr getInstance()
            throws VizException {
        return getInstance("base");
    }

    public static synchronized ResourceDefnsMngr getInstance(String user)
            throws VizException {
        ResourceDefnsMngr instance = instanceMap.get(user);
        if (instance == null) {
            try {
                instance = new ResourceDefnsMngr(user);

                instance.readResourceDefnTable();

            } catch (VizException ve) {
                throw ve;
            }

            instanceMap.put(user, instance);
        }

        return instance;
    }

    // used for development to avoid restarting cave each time.
    // public static void reset() {
    // instanceMap.clear();
    // this.getInstance();
    // }
    private ResourceDefnsMngr(String user) {
        String rscsBaseDir = LocalizationConstants.DEFAULT_ROOT_DIR_PORTION_FOR_NCEP_BASE
                + File.separator
                + LocalizationResourcePathConstants.RESOURCES_BASE_DIR;

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        resourceImplementationsDir = pathMgr.getFile(pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.BASE),
                rscsBaseDir + File.separator + RSC_IMPLS_DIR);

        String resourceDefnFilePath = rscsBaseDir + File.separator
                + RSC_DEFNS_DIR + RSC_DEFNS_FILENAME;
        resourceDefnFile = pathMgr.getStaticFile(resourceDefnFilePath);

        String attrSetGroupDirPath = rscsBaseDir + File.separator
                + RSC_DEFNS_DIR + ATTR_SET_GROUPS_DIR;
        attrSetGroupsDir = pathMgr.getFile(pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.BASE),
                attrSetGroupDirPath);

        if (!resourceDefnFile.exists()) {
            System.out.println("Error opening the Resource Definitions file !");
        }

        if (!resourceImplementationsDir.exists()) {
            System.out
                    .println("Error opening the Resource Implementations dir !");
        }

    }

    public ArrayList<String> getAvailResourceCategories() {
        return (ArrayList<String>) Arrays.asList(AvailResourceCategories);
    }

    // Do we need to be able to re-read this file? For now just read it once and
    // assume it will not change outside of a save.
    //
    private void readResourceDefnTable() throws VizException {
        if (resourceDefnsMap != null) {
            return;
        }

        int defnIndx = 0;

        Object xmlObj;
        try {
            xmlObj = SerializationUtil
                    .jaxbUnmarshalFromXmlFile(resourceDefnFile
                            .getAbsolutePath());

        } catch (SerializationException e) {
            System.out.println("Error parsing "
                    + resourceDefnFile.getAbsolutePath() + " - "
                    + e.getMessage());
            throw new VizException(e);
        }

        if (!(xmlObj instanceof ResourceDefinitionTable)) {
            VizException ve = new VizException(
                    "Unmarshalled rsc defn file is not a valid ResourceDefinitionTable object???");
            throw ve;
        }

        resourceDefnTable = (ResourceDefinitionTable) xmlObj;

        resourceDefnsMap = new HashMap<String, ResourceDefinition>();

        List<String> availCategories = Arrays.asList(AvailResourceCategories);

        // copy the resourceDefnTable to the resourceDefnsMap and then
        // verify the implementation, and add the resource Definitions to the
        // map.
        //
        for (ResourceDefinition rscDefn : resourceDefnTable
                .getResourceDefinitionsList()) {

            // TODO : If the definitions are modified and written out, this will
            // drop any invalid resourceDefns.
            // Should we save these write them out anyway? Make them disabled?

            // Validate the category
            if (!availCategories.contains(rscDefn.getResourceCategory())) {
                System.out.println("Error configuring resource: "
                        + rscDefn.getResourceDefnName()
                        + " : Invalid Resource Category.");
                continue;
            }

            // Validate that the resource implementation is present.
            //
            String rscImpl = rscDefn.getRscImplementation();

            if (!ResourceExtPointMngr.getInstance().getAvailResources()
                    .contains(rscImpl)) {
                System.out.println("Error configuring resource: "
                        + rscDefn.getResourceDefnName());
                System.out.println("The Resource implementation: " + rscImpl
                        + " is not "
                        + "specified in a NC-Resource extention point");
            } else {
                rscDefn.setDefinitionIndex(defnIndx++);

                resourceDefnsMap.put(rscDefn.getResourceDefnName(), rscDefn);
            }
        }

        // read in the attrSetGroupsMap (this needs the resourceDefnsMap
        // to be set.)
        //
        readAttrSetGroups();

        // Next loop thru the resource defns and update with the parameter info,
        // and the sub-types or attrSetGroups
        //
        for (ResourceDefinition rscDefn : resourceDefnsMap.values()) {

            // read in the parameters file and set the string in the rscDefn.
            //
            File prmFile = getResourceParamsFile(rscDefn.getResourceDefnName());

            if (prmFile == null || !prmFile.exists()) {
                rscDefn.setRscTypeParameters("");
            } else {
                try {
                    FileReader fr = new FileReader(prmFile);
                    char[] prmBufr = new char[(int) prmFile.length()];
                    fr.read(prmBufr);
                    fr.close();

                    rscDefn.setRscTypeParameters(new String(prmBufr));

                } catch (FileNotFoundException fnf) {
                    rscDefn.setRscTypeParameters("");
                } catch (IOException ioe) {
                    rscDefn.setRscTypeParameters("");
                    System.out.println("I/O error reading rsc params file");
                }
            }

            // check if

            // fill in the list of available attrSets
            //
            findAvailAttrSets(rscDefn);
        }

        // query the database to generate dynamic resource names.
        //
        // generateDynamicResources( );
    }

    // regenerate any dynamic types or subTypes.
    //
    public void generateDynamicResources() {
        for (ResourceDefinition rscDefn : resourceDefnsMap.values()) {
            if (!rscDefn.getSubTypeGenerator().isEmpty()
                    || !rscDefn.getRscTypeGenerator().isEmpty()) {

                generateDynamicResources(rscDefn);
            }
        }
    }

    // for resources that either have a type generator (some GRIDs and
    // Ensembles) or a subType
    // generator (Satellite and Radars)
    //
    public void generateDynamicResources(ResourceDefinition rscDefn) {

        // PGEN doesn't currently query the database so treat it differently
        if (!rscDefn.isPgenResource()) {
            // create request constraints for the query
            HashMap<String, RequestConstraint> queryTerms = new HashMap<String, RequestConstraint>();
            String genParam = (rscDefn.getSubTypeGenerator().isEmpty() ? rscDefn
                    .getRscTypeGenerator() : rscDefn.getSubTypeGenerator());

            try { // get the pluginName from the rscTypeParameters
                File prmFile = getResourceParamsFile(rscDefn
                        .getResourceDefnName());
                if (prmFile == null) {
                    System.out.println("No Paramter file found for resource: "
                            + rscDefn.getResourceDefnName());
                    return;
                }

                HashMap<String, String> paramsMap = readParamsFile(prmFile);

                if (!paramsMap.containsKey("pluginName")) {
                    System.out.println("Error Requesting " + genParam + " for "
                            + rscDefn.getResourceDefnName()
                            + " - The pluginName "
                            + "must be given as a resource parameter.");
                } else {
                    rscDefn.getSubTypesMap().clear();

                    // for types (GRIDS/Radar) create a CatalogQuery cause its
                    // faster.
                    //
                    if (!rscDefn.getRscTypeGenerator().isEmpty()) {

                        queryTerms.put("pluginName", new RequestConstraint(
                                paramsMap.get("pluginName")));

                        if (rscDefn.getResourceCategory().equals(
                                ResourceName.GridRscCategory)
                                || rscDefn.getResourceCategory().equals(
                                        ResourceName.EnsembleRscCategory)) {

                            // the parameter name is 'GDFILE' but the DB table
                            // is 'modelName'
                            queryTerms.put("modelName", new RequestConstraint(
                                    paramsMap.get("GDFILE")));
                        } else if (rscDefn.getResourceCategory().equals(
                                ResourceName.RadarRscCategory)) {
                        }

                        String dynParamValues[] = CatalogQuery.performQuery(
                                genParam, queryTerms);

                        ArrayList<String> genTypesList = new ArrayList<String>();

                        //
                        for (String prmVal : dynParamValues) {
                            if (rscDefn.getResourceCategory().equals(
                                    ResourceName.GridRscCategory)) {
                                String eventName = prmVal;
                                String genType = rscDefn.getResourceDefnName()
                                        + ":" + eventName;
                                if (!genTypesList.contains(genType)) {
                                    genTypesList.add(genType);
                                }
                            }
                            // Will the ensemble members be in the GRID or
                            // Ensemble category?
                            else if (rscDefn.getResourceCategory().equals(
                                    ResourceName.EnsembleRscCategory)) {

                                // Ensemble member dynamic types are not
                                // implemented yet.
                            } else if (rscDefn.getResourceCategory().equals(
                                    ResourceName.RadarRscCategory)) {

                                // TODO : NMAP2 gives the state that the radar
                                // is in but this data is not in
                                // the DB so add code to look it up.
                                String genType = rscDefn.getResourceDefnName()
                                        + ":" + prmVal.toUpperCase();
                                if (!genTypesList.contains(genType)) {
                                    genTypesList.add(genType);
                                }
                            }
                        }

                        rscDefn.setGeneratedTypesList(genTypesList);
                    } else { // for subTypes (for Satellite) we may have 2
                             // parameters so we can't use a CatalogQuery...
                        rscDefn.getGeneratedTypesList().clear();

                        // TODO : This works because it so happens that all the
                        // parameters for resources that
                        // generate subTypes are request parameters. (ie. not
                        // something like METAR's spiFile which
                        // is not part of the query.)
                        // TODO : Also the names of the parameters need to match
                        // the names of the DB tbl.
                        // It would be better to somehow specify the query
                        // constraints.
                        for (String prm : paramsMap.keySet()) {
                            queryTerms.put(prm,
                                    new RequestConstraint(paramsMap.get(prm)));
                        }

                        LayerProperty prop = new LayerProperty();
                        prop.setDesiredProduct(ResourceType.PLAN_VIEW);
                        prop.setEntryQueryParameters(queryTerms, false);
                        prop.setNumberOfImages(15000);
                        String script = null;
                        script = ScriptCreator.createScript(prop);

                        if (script == null)
                            return;

                        Object[] pdoList = Connector.getInstance().connect(
                                script, null, 60000);

                        HashMap<String, ArrayList<String>> subTypesMap = new HashMap<String, ArrayList<String>>();

                        for (Object pdo : pdoList) {
                            String subType = null;
                            String attrSetKey = null;

                            if (rscDefn.getResourceCategory().equals(
                                    ResourceName.SatelliteRscCategory)) {
                                if (rscDefn.getRscImplementation().equals(
                                        "GiniSatellite")) {
                                    SatelliteRecord satRec = (SatelliteRecord) pdo;
                                    subType = satRec.getSectorID();
                                    attrSetKey = satRec.getPhysicalElement();
                                } else if (rscDefn.getRscImplementation()
                                        .equals("McidasSatellite")) {
                                    McidasRecord satRec = (McidasRecord) pdo;
                                    subType = satRec.getAreaName() + "_"
                                            + satRec.getResolution().toString()
                                            + "km";
                                    attrSetKey = satRec.getImageType();
                                }
                            } else if (rscDefn.getResourceCategory().equals(
                                    ResourceName.RadarRscCategory)) {
                                // if Mosaic there is no subType so just use
                                // 'mosaic'
                                MosaicRecord rdrRec = (MosaicRecord) pdo;
                                subType = "mosaic";
                                attrSetKey = rdrRec.getProdName();
                                // if Radar then use the name of the radar as
                                // the subType
                            }

                            if (subType != null) {
                                if (!subTypesMap.containsKey(subType)) {

                                    subTypesMap.put(subType,
                                            new ArrayList<String>());
                                }

                                subTypesMap.get(subType).add(attrSetKey);
                            }
                        }

                        rscDefn.setSubTypesMap(subTypesMap);
                    }
                }
            } catch (VizException e1) {
                System.out.println("Error Generating Dynamic Resource "
                        + rscDefn.getResourceDefnName() + ": "
                        + e1.getMessage());
            }
        } else { // PGEN
            HashMap<String, ArrayList<String>> subTypesMap = new HashMap<String, ArrayList<String>>();

            // PGEN has multiple .prm files which are each treated as a SubType
            File cnfgDir = new File(resourceDefnFile.getParentFile(),
                    rscDefn.getConfigDir());
            if (cnfgDir.exists()) {
                String[] prmFileNames = cnfgDir.list(new FilenameFilter() {
                    @Override
                    public boolean accept(File dir, String name) {
                        return name.endsWith(".xml");
                    }
                });
                rscDefn.getGeneratedTypesList().clear();

                for (String prmFileName : prmFileNames) {
                    String productName = prmFileName.substring(0,
                            prmFileName.length() - 4);
                    // for PGEN there aren't any 'attrSetKeys' (these are used
                    // by Satellite and Radar
                    // to determine which attrSets
                    if (!subTypesMap.containsKey(productName)) {
                        subTypesMap.put(productName, new ArrayList<String>());
                        subTypesMap.get(productName).add(
                                "show all attribute sets");
                    }
                }
            } else {
                System.out.println("Error: PGEN directory "
                        + cnfgDir.getAbsolutePath() + " doesn't exist");
            }

            rscDefn.setSubTypesMap(subTypesMap);
        }
    }

    // Dynamic update Data Resources
    public void dynamicUpdateDataResource(String pluginName, String dataURI) {
        // System.out.println ("--updateDataResource: pluginName:" + pluginName
        // + " dataURI:" + dataURI);
        for (ResourceDefinition rscDefn : resourceDefnsMap.values()) {
            if ((!rscDefn.getSubTypeGenerator().isEmpty() || !rscDefn
                    .getRscTypeGenerator().isEmpty())
                    && (!rscDefn.isPgenResource())) {

                dynamicUpdateDataResource(rscDefn, pluginName, dataURI);
            }
        }
    }

    // dynamic update resources that either have a type generator (some GRIDs
    // and Ensembles) or a subType
    // generator (Satellite and Radars)
    private void dynamicUpdateDataResource(ResourceDefinition rscDefn,
            String pluginName, String dataURI) {
        if (pluginName.equals("mcidas") || pluginName.equals("satellite")) {
            dynamicUpdateMcidasDataResource(rscDefn, dataURI);
        } else if (pluginName.equals("ncgrib")) {
            dynamicUpdateGrid2DataResource(rscDefn, dataURI);
        } else if (pluginName.equals("radar")) {
            dynamicUpdateRadarDataResource(rscDefn, dataURI);
        } else if (pluginName.equals("mosaic")) {
            dynamicUpdateMosaicDataResource(rscDefn, dataURI);
        }
    }

    // dynamic update mcidas/satellite data resources
    private void dynamicUpdateMcidasDataResource(ResourceDefinition rscDefn,
            String dataURI) {
        String subType = null;
        String attrSetKey = null;
        if (!rscDefn.getSubTypeGenerator().isEmpty()
                && rscDefn.getResourceCategory().equals(
                        ResourceName.SatelliteRscCategory)) {

            String[] attribs = dataURI.split("/", 8);

            if (rscDefn.getRscImplementation().equals("McidasSatellite")) {
                if (rscDefn.getRscTypeParameters().contains(
                        "satelliteName=" + attribs[3])) {
                    subType = attribs[4] + "_" + attribs[5] + "km";
                    attrSetKey = attribs[6];
                }
            } else if (rscDefn.getRscImplementation().equals("GiniSatellite")) {
                if (rscDefn.getRscTypeParameters().contains(
                        "creatingEntity=" + attribs[4])) {
                    subType = attribs[5];
                    attrSetKey = attribs[6];
                }
            }
            if (subType != null) {
                HashMap<String, ArrayList<String>> subTypesMap = rscDefn
                        .getSubTypesMap();
                if (!subTypesMap.containsKey(subType)) {
                    subTypesMap.put(subType, new ArrayList<String>());
                    subTypesMap.get(subType).add(attrSetKey);
                } else {
                    if (!subTypesMap.get(subType).contains(attrSetKey)) {
                        subTypesMap.get(subType).add(attrSetKey);
                    }
                }
                rscDefn.setSubTypesMap(subTypesMap);
            }
        }
    }

    // dynamic update GRID2 data resources
    private void dynamicUpdateGrid2DataResource(ResourceDefinition rscDefn,
            String dataURI) {
        String genType = null;

        if (!rscDefn.getRscTypeGenerator().isEmpty()
                && rscDefn.getResourceCategory().equals(
                        ResourceName.GridRscCategory)) {

            String[] attribs = dataURI.split("/", 15);

            if (rscDefn.getRscTypeParameters().contains("GDFILE=" + attribs[3])) {
                String[] fileNameAttrs = attribs[12].split(".", 3);
                genType = rscDefn.getResourceDefnName() + ":"
                        + fileNameAttrs[0];
            }

            if (genType != null) {
                ArrayList<String> genTypesList = rscDefn
                        .getGeneratedTypesList();

                if (!genTypesList.contains(genType)) {
                    genTypesList.add(genType);
                    rscDefn.setGeneratedTypesList(genTypesList);
                }
            }
        }
    }

    // dynamic update RADAR data resources
    private void dynamicUpdateRadarDataResource(ResourceDefinition rscDefn,
            String dataURI) {
        String genType = null;

        if (!rscDefn.getRscTypeGenerator().isEmpty()
                && rscDefn.getResourceCategory().equals(
                        ResourceName.RadarRscCategory)) {

            String[] attribs = dataURI.split("/", 5);

            if (rscDefn.getRscTypeParameters().contains(
                    "pluginName=" + attribs[1])) {
                genType = rscDefn.getResourceDefnName() + ":"
                        + attribs[1].toUpperCase();
            }

            if (genType != null) {
                ArrayList<String> genTypesList = rscDefn
                        .getGeneratedTypesList();

                if (!genTypesList.contains(genType)) {
                    genTypesList.add(genType);
                    rscDefn.setGeneratedTypesList(genTypesList);
                }
            }
        }
    }

    // dynamic update MOSAIC data resources
    private void dynamicUpdateMosaicDataResource(ResourceDefinition rscDefn,
            String dataURI) {
        String subType = null;
        String attrSetKey = null;

        if (!rscDefn.getSubTypeGenerator().isEmpty()
                && rscDefn.getResourceCategory().equals(
                        ResourceName.RadarRscCategory)) {

            String[] attribs = dataURI.split("/", 5);

            if (rscDefn.getRscTypeParameters().contains(
                    "pluginName=" + attribs[1])) {
                subType = "mosaic";
                attrSetKey = attribs[4];
            }

            if (subType != null) {
                HashMap<String, ArrayList<String>> subTypesMap = rscDefn
                        .getSubTypesMap();
                if (!subTypesMap.containsKey(subType)) {
                    subTypesMap.put(subType, new ArrayList<String>());
                    subTypesMap.get(subType).add(attrSetKey);
                } else {
                    if (!subTypesMap.get(subType).contains(attrSetKey)) {
                        subTypesMap.get(subType).add(attrSetKey);
                    }
                }
                rscDefn.setSubTypesMap(subTypesMap);
            }
        }
    }

    // open the directories and files under ATTR_SET_GROUPS_DIR and
    // initialize the attrSetGroupsMap
    private void readAttrSetGroups() throws VizException {

        attrSetGroupsMap = new HashMap<String, AttrSetGroup>();

        // read in the AttrSetGroups into a map
        //
        File attrSetGrpsForRscImpl[] = attrSetGroupsDir
                .listFiles(new FileFilter() {
                    @Override
                    public boolean accept(File pathname) {
                        return (pathname.isDirectory() && pathname
                                .getAbsolutePath().indexOf(".svn") == -1);
                    }
                });

        // the sub-dirs under the attrSetGroups dir must match a
        // resourceImplClass
        //
        for (File rscImplAsgDir : attrSetGrpsForRscImpl) {
            String rscImpl = rscImplAsgDir.getName();

            // validate that there is a resourceImpl for this attrSetGroup
            //
            if (!ResourceExtPointMngr.getInstance().getAvailResources()
                    .contains(rscImpl)) {
                System.out
                        .println("Can't  find Resource Implementation (class) for "
                                + "attrSetGroup : " + rscImpl);

                System.out
                        .println("The Resource implementation should be specified in "
                                + "a NC-Resource extention point");
                // don't fail; go ahead and put the attrset in the map
            }

            File asgFile = new File(rscImplAsgDir, rscImpl + ".xml");
            if (!asgFile.exists()) {
                System.out.println("Can't open AttrSetGroup file: "
                        + asgFile.getAbsolutePath());
                continue;
            }

            Object asgListObj;
            try {
                asgListObj = SerializationUtil.jaxbUnmarshalFromXmlFile(asgFile
                        .getAbsolutePath());

                if (!(asgListObj instanceof AttrSetGroupList)) {
                    System.out.println(asgFile.getAbsolutePath()
                            + " file is expected to be "
                            + "an attrSetGroupList file");
                    continue;
                }

            } catch (SerializationException e) {
                throw new VizException(e);
            }

            AttrSetGroupList attrSetGroupList = (AttrSetGroupList) asgListObj;
            if (attrSetGroupList.getAttrSetGroupList() == null) {
                continue;
            }

            // add the ASG's in the list to the map. (PGEN is a special case
            // since
            // 1 'default' ASG applies to all PGEN resources.)
            for (AttrSetGroup asg : attrSetGroupList.getAttrSetGroupList()) {
                asg.validateAttrSets(rscImplAsgDir);

                // sanity check that the resources in the attrSetGroup
                // exist and match this rscImpl. PGEN is a special case.
                //
                if (rscImpl.equals("PGEN")) {
                    attrSetGroupsMap.put(asg.getResource(), asg); // rscImpl,
                                                                  // asg );
                } else {
                    ResourceDefinition rscDefn = getResourceDefinition(asg
                            .getResource());

                    if (rscDefn == null) {
                        System.out.println("AttrSetGroup file "
                                + asgFile.getName()
                                + " has a unknown resource:"
                                + asg.getResource());
                        continue;
                    } else if (!rscDefn.getRscImplementation().equals(rscImpl)) {
                        System.out.println("AttrSetGroup file "
                                + asgFile.getName() + " has resource:"
                                + asg.getResource()
                                + " with mismatched implementation:"
                                + rscDefn.getRscImplementation());
                        continue;
                    } else {
                        attrSetGroupsMap.put(asg.getResource() + File.separator
                                + asg.getAttrSetGroupName(), asg);
                    }
                }
            }
        }
    }

    public ResourceDefinition getResourceDefnByIndx(int defnIndx) {
        for (ResourceDefinition rscDefn : resourceDefnsMap.values()) {
            if (rscDefn.getDefinitionIndex() == defnIndx) {
                return rscDefn;
            }
        }

        return null;
    }

    public void writeResourceDefnTable() throws VizException {

        ArrayList<ResourceDefinition> resourceDefnList = new ArrayList<ResourceDefinition>();

        for (int defnIndx = 0; defnIndx < resourceDefnsMap.values().size(); defnIndx++) {
            ResourceDefinition rscDefn = getResourceDefnByIndx(defnIndx);
            if (rscDefn == null)
                continue; // ???

            resourceDefnList.add(rscDefn);
        }

        // TODO : this will drop any 'invalid' resourceDefns. Should we save
        // these
        // when reading the file and write them out anyway?

        ResourceDefinitionTable rscDefnTbl = new ResourceDefinitionTable();
        rscDefnTbl.setResourceDefinitionsList(resourceDefnList);

        // loop thru the resources and write out the parameters only if
        // the parameters have been changed.
        for (ResourceDefinition rscDefn : resourceDefnsMap.values()) {
            //
            if (rscDefn.isPgenResource()) {
                continue;
            }

            if (rscDefn.getRscTypeParamsModified()) {

                File prmFile = getResourceParamsFile(rscDefn
                        .getResourceDefnName());
                if (prmFile != null && prmFile.exists()) {
                    try {
                        FileWriter fwriter = new FileWriter(prmFile);

                        fwriter.write(rscDefn.getRscTypeParameters());
                        fwriter.close();

                    } catch (FileNotFoundException fnf) {
                        // Log error
                        System.out.println("Error writing prm file for "
                                + rscDefn.getResourceDefnName());
                    } catch (IOException ioe) {
                        // Log error
                        System.out.println("Error writing prm file for "
                                + rscDefn.getResourceDefnName());
                    }
                }
            }
        }

        try {
            SerializationUtil.jaxbMarshalToXmlFile(rscDefnTbl,
                    resourceDefnFile.getAbsolutePath());

            writeAttrSetGroups();

        } catch (SerializationException e) {
            throw new VizException(e);
        }
    }

    // for each rscImp that uses attribute set,
    public void writeAttrSetGroups() throws VizException {

        File asgBaseDir = new File(resourceDefnFile.getParentFile(),
                "AttrSetGroups");

        if (!asgBaseDir.exists()) {
            throw new VizException("Can't locate AttrSetGroups Base Dir :"
                    + asgBaseDir.getAbsolutePath());
        }

        File attrSetGrpsForRscImpl[] = asgBaseDir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File pathname) {
                return pathname.isDirectory();
            }
        });

        // the sub-dirs under the attrSetGroups dir must match a
        // resourceImplClass
        //
        for (File rscImplAsgDir : attrSetGrpsForRscImpl) {
            String rscImpl = rscImplAsgDir.getName();

            AttrSetGroupList asgList = new AttrSetGroupList();
            boolean asgsModified = false;

            // the attrSetGroup file is the name of the rscImpl+xml.
            // find all of the attrSetGroups with resources that match
            // this impl.
            for (AttrSetGroup asg : attrSetGroupsMap.values()) {
                asgsModified = asgsModified || asg.isModified();

                if (asg.getResource().equals("PGEN")) {
                    if (rscImpl.equals("PGEN")) {
                        asgList.addAttrSetGroup(asg);
                    }
                } else {
                    ResourceDefinition rscDefn = getResourceDefinition(asg
                            .getResource());
                    if (rscDefn != null) {
                        if (rscDefn.getRscImplementation().equals(rscImpl)) {
                            asgList.addAttrSetGroup(asg);
                        }
                    }
                }
            }

            File asgFile = new File(rscImplAsgDir, rscImplAsgDir.getName()
                    + ".xml");

            try {
                SerializationUtil.jaxbMarshalToXmlFile(asgList,
                        asgFile.getAbsolutePath());
            } catch (SerializationException e) {
                throw new VizException(e);
            }
        }
    }

    public ResourceDefinition getResourceDefinition(ResourceName rscName) {
        return getResourceDefinition(rscName.getRscType());
    }

    public boolean findResourceDefinition(String rscType) {
        if (resourceDefnsMap.containsKey(rscType)) {
            return true;
        }

        // allow for generated types which will have a ':'
        //
        int indx = rscType.indexOf(":");

        if (indx == -1) {
            return false;
        }

        return resourceDefnsMap.containsKey(rscType.substring(0, indx));
    }

    public ResourceDefinition getResourceDefinition(String rscType) {
        if (resourceDefnsMap.containsKey(rscType)) {
            return resourceDefnsMap.get(rscType);
        }

        // allow for generated types which will have a ':'
        //
        int indx = rscType.indexOf(":");

        if (indx != -1) {
            if (resourceDefnsMap.containsKey(rscType.substring(0, indx))) {
                return resourceDefnsMap.get(rscType.substring(0, indx));
            }
        }

        System.out.println("sanity check: can't find ResourceDefinition for: "
                + rscType);
        return null;
    }

    public String[] getResourceCategories() {
        ArrayList<String> catsList = new ArrayList<String>();

        for (ResourceDefinition rscDefn : resourceDefnsMap.values()) {
            if (rscDefn.getIsEnabled()) {
                String rscCat = rscDefn.getResourceCategory();

                if (!catsList.contains(rscCat)) {
                    catsList.add(rscCat);
                }
            }
        }

        String[] catsArray = catsList.toArray(new String[0]);

        Arrays.sort(catsArray, new Comparator<String>() {
            @Override
            public int compare(String o1, String o2) {
                int indx1 = availResourceCategoriesList.indexOf(o1);
                int indx2 = availResourceCategoriesList.indexOf(o2);

                return ((indx1 == indx2 ? 0 : (indx1 < indx2 ? -1 : 1)));
            }
        });

        return catsArray;

        // return catsList.toArray( new String[0] );
        // return ResourceCategories;
    }

    // map the Full Resource Name to the location of the resource bundle
    // template file
    // for the resource.
    // The rsc name is the RBD Category/Type/Group/AttributeSet. The fcst/obs is
    // not saved
    // so we try both to find a match.
    public File getRscBundleTemplateFile(String rscType) {
        ResourceDefinition rscDefn = getResourceDefinition(rscType);

        if (rscDefn == null) {
            return null;
        }

        // get the name of the NC resource which will implement the resource
        String rscImplName = rscDefn.getRscImplementation();
        if (!rscImplName.endsWith(".xml")) {
            rscImplName = rscImplName + ".xml";
        }

        File rbtFile = new File(resourceImplementationsDir, BNDL_TMPLTS_DIR
                + rscImplName);

        return (rbtFile.exists() ? rbtFile : null);
    }

    public boolean isResourceEnabled(String rscType) {
        ResourceDefinition rscDefn = getResourceDefinition(rscType);

        return (rscDefn == null ? false : rscDefn.getIsEnabled());
    }

    public boolean setResourceEnabled(String rscType, boolean isEnabled) {
        ResourceDefinition rscDefn = getResourceDefinition(rscType);

        if (rscDefn == null) {
            return false;
        }

        rscDefn.setEnabled(isEnabled);
        return true;
    }

    public boolean doesResourceUseAttrSetGroups(String rscType) {
        ResourceDefinition rscDefn = getResourceDefinition(rscType);

        return (rscDefn == null ? false : rscDefn.applyAttrSetGroups());
    }

    public File getResourceParamsFile(String rscType) {
        String fname = getResourceParamsFilename(rscType, "");
        if (fname == null)
            return null;
        File f = new File(fname);
        return (f.exists() ? f : null);
    }

    public String getResourceParamsFilename(String rscType, String rscGroup) {
        ResourceDefinition rscDefn = getResourceDefinition(rscType);

        if (rscDefn == null) {
            return null;
        }

        // get the configDir which will have the parameter files and
        // attributeSets
        //
        File cnfgDir = new File(resourceDefnFile.getParentFile(),
                rscDefn.getConfigDir());

        if (!cnfgDir.exists()) {
            System.out.println("Config Dir not found for resource:" + rscType);
            return null;
        }

        if (rscDefn.isPgenResource()) {
            return cnfgDir.getParent() + File.separator + "PGEN.prm";

            // return cnfgDir.getAbsolutePath()+File.separator+rscGroup+".prm";
        } else if (!rscDefn.getRscTypeGenerator().isEmpty()) {
            int indx = rscType.indexOf(":");
            if (indx != -1) {
                return cnfgDir.getAbsolutePath() + File.separator
                        + rscType.substring(0, indx) + ".prm";
            }
        }
        return cnfgDir.getAbsolutePath() + File.separator + rscType + ".prm";
    }

    // this will find all the .prm files and the attrSet file, parse each into
    // a map and combine the maps into one parameters hashmap.
    public HashMap<String, String> getResourceParametersForType(
            ResourceName rscName) throws VizException {

        HashMap<String, String> paramsMap = new HashMap<String, String>();

        String prmFilename = getResourceParamsFilename(rscName.getRscType(),
                rscName.getRscGroup());
        if (prmFilename != null) {

            File prmFile = new File(prmFilename);

            try {
                if (prmFile.exists()) {
                    paramsMap.putAll(readParamsFile(prmFile));
                }
            } catch (VizException ve) {
                throw ve;
            }
        }

        return paramsMap;
    }

    public void createParameterFileForResource(ResourceName rscName,
            HashMap<String, String> paramsMap) throws VizException {
        ResourceDefinition rscDefn = getResourceDefinition(rscName.getRscType());

        if (rscDefn == null) {
            return;
        }

        try {
            String prmFilename = getResourceParamsFilename(
                    rscName.getRscType(), rscName.getRscGroup());

            if (prmFilename == null) {
                throw new VizException("Unable to create prm file.");
            }

            File prmFile = new File(prmFilename);

            // if creating the prm file and if there is a group
            if (!prmFile.exists()) {
                prmFile.createNewFile();

                // if( (!rscDefn.applyAttrSetGroups() ||
                // rscDefn.isPgenResource()) &&
                // !rscName.getRscGroup().isEmpty() ) {
                // rscDefn.addRscSubTypes( rscName.getRscGroup() );
                // }
            }

            FileWriter fwriter = new FileWriter(prmFile);

            // write out a comment line
            fwriter.write("! Parameter File for Resource " + rscName.toString()
                    + "\n");

            for (String prmName : paramsMap.keySet()) {
                fwriter.write(prmName + "=" + paramsMap.get(prmName) + "\n");
            }
            fwriter.close();
        } catch (IOException e) {
            throw new VizException(
                    "IOException creating parameter file for rsc: "
                            + rscName.toString());
        }
    }

    // find all the possible Attribute Set Files either from the attribute set
    // groups or
    // in the resource's config dir.
    //
    public void findAvailAttrSets(ResourceDefinition rscDefn) {

        String attrSetFilename;
        rscDefn.removeAllAttrSets();
        ArrayList<String> badAttrSetGroupList = new ArrayList<String>();

        if (rscDefn.applyAttrSetGroups()) {
            for (String asgName : rscDefn.getAttrSetGroupNames()) {
                String asgKey = rscDefn.getResourceDefnName() + File.separator
                        + asgName;

                if (rscDefn.isPgenResource()) {
                    asgKey = asgName;
                }

                if (attrSetGroupsMap.containsKey(asgKey)) {
                    AttrSetGroup asg = attrSetGroupsMap.get(asgKey);

                    for (String asName : asg.getAttrSetNames()) {
                        rscDefn.addAttrSetFile(asName,
                                asg.getAttrSetFile(asName));
                    }
                } else {
                    System.out.println("Resource has attrSetGroup " + asgKey
                            + " that is not in the AttrSetGroups Table? ");
                    badAttrSetGroupList.add(asgName);
                }
            }

            //
            if (!badAttrSetGroupList.isEmpty()) {
                for (String badasg : badAttrSetGroupList) {
                    rscDefn.removeAttrSetGroup(badasg);
                }
            }
        } else {
            ArrayList<String> attrSetList = new ArrayList<String>();

            File cnfgDir = new File(resourceDefnFile.getParentFile(),
                    rscDefn.getConfigDir());

            if (cnfgDir.exists()) {
                File[] attrSetFiles = cnfgDir.listFiles(new FilenameFilter() {
                    @Override
                    public boolean accept(File dir, String name) {
                        return name.endsWith(ATTR_SET_FILE_EXT);
                    }
                });

                for (File asFile : attrSetFiles) {
                    rscDefn.addAttrSetFile(
                            asFile.getName().substring(
                                    0,
                                    asFile.getName().length()
                                            - ATTR_SET_FILE_EXT.length()),
                            asFile);
                }
            }
        }
    }

    // get the Attribute Set File for the given resource name. This may either
    // be in the AttrSetGroup directory or directly under the resources config
    // dir.
    //
    public File getAttrSetFile(ResourceName rscName) {
        ResourceDefinition rscDefn = getResourceDefinition(rscName.getRscType());

        if (rscDefn == null) {
            return null;
        }

        if (rscDefn.applyAttrSetGroups()) {
            if (rscName.getRscGroup().isEmpty()) {
                System.out.println("No AttributeSetGroup for :" + rscName);
                return null;
            }
            AttrSetGroup asGroup = attrSetGroupsMap.get(rscName.getRscType()
                    + File.separator + rscName.getRscGroup());

            if (asGroup != null) {
                return asGroup.getAttrSetFile(rscName.getRscAttrSetName());
            } else {
                return null;
            }
        } else {
            return rscDefn.getAttrSetFile(rscName.getRscAttrSetName());
        }
    }

    // Get all parameters needed to instantiate the bundle template
    // This includes parameters from
    //
    public HashMap<String, String> getAllResourceParameters(ResourceName rscName)
            throws VizException {
        ResourceDefinition rscDefn = getResourceDefinition(rscName.getRscType());

        if (rscDefn == null) {
            return null;
        }

        // first get the parameters from the .prm file for this type.
        HashMap<String, String> paramsMap = getResourceParametersForType(rscName);

        // next get the attributes
        try {
            File attrSetFile = rscDefn.getAttrSetFile(rscName
                    .getRscAttrSetName());

            if (attrSetFile != null) {
                paramsMap.putAll(readParamsFile(attrSetFile));
            }
        } catch (VizException ve) {
            throw ve;
        }

        // and now create the parameters from the rscDefinitions file.
        // (frameInterval, timeMatchMethod)
        if (!rscDefn.getResourceCategory().equals(
                ResourceName.OverlayRscCategory)) {
            paramsMap
                    .put("frameSpan", Integer.toString(rscDefn.getFrameSpan()));
            paramsMap.put("timeMatchMethod", rscDefn.getTimeMatchMethod()
                    .toString());
            paramsMap.put("dfltNumFrames",
                    Integer.toString(rscDefn.getDfltFrameCount()));
            paramsMap.put("dfltTimeRange",
                    Integer.toString(rscDefn.getDfltTimeRange()));
            paramsMap.put("timelineGenMethod", rscDefn.getTimelineGenMethod()
                    .toString());
            paramsMap.put("isForecast", (rscDefn.isForecast() ? "true"
                    : "false"));
        }

        // finally add 'dynamic' parameters that were queried from the database
        //

        // if this is a generated type
        if (!rscDefn.getRscTypeGenerator().isEmpty()) {
            if (rscName.getRscCategory().equals(ResourceName.GridRscCategory)) {

                // Note this will replace the 'default' eventName parameter
                // ('%') which
                // is used for non-event GRIDS
                if (rscDefn.getRscTypeGenerator().equals("eventName")) {
                    String rscType = rscName.getRscType();
                    int indx = rscType.indexOf(":");
                    if (indx == -1) {
                        throw new VizException(
                                "sanity check: Can't parse eventName from Grid "
                                        + "Resource name :"
                                        + rscName.toString());
                    }

                    String eventName = rscType.substring(indx + 1);

                    paramsMap.put("eventName", eventName);
                }
            } else if (rscName.getRscCategory().equals(
                    ResourceName.RadarRscCategory)) {
                if (rscDefn.getRscTypeGenerator().equals("icao")) {
                    String rscType = rscName.getRscType();
                    int indx = rscType.indexOf(":");
                    if (indx == -1) {
                        throw new VizException(
                                "sanity check: Can't parse icao from " + ""
                                        + "LocalRadar Resource name :"
                                        + rscName.toString());
                    }

                    String icaoStr = rscType.substring(indx + 1).toLowerCase();

                    paramsMap.put("icao", icaoStr);
                }
            }
        }

        // If there is a generated sub-type then we will need to set a parameter
        // for this
        // (In this case the name of the parameter in the paramsMap must be the
        // same as the
        // name of the variable in the BundleTemplate.)
        //
        if (!rscDefn.getSubTypeGenerator().isEmpty()) {
            if (rscName.getRscGroup().isEmpty()) {
                System.out
                        .println("getAllResourceParameters: Sanity Check: the generated sub-type "
                                + "is null for : " + rscName.toString());
            } else if (rscName.getRscCategory().equals(
                    ResourceName.SatelliteRscCategory)) {

                // TODO: This whole thing needs to be reworked. We shouldn't
                // have to check
                // for the implementation here.
                // GINI creates subTypes frome the 'sector' and
                // McIdas uses a combination of the areaName and the resolution.
                if (rscDefn.getRscImplementation().equals("McidasSatellite")) {
                    //
                    int indx = rscName.getRscGroup().lastIndexOf('_');
                    String areaName = rscName.getRscGroup().substring(0, indx);
                    String resStr = rscName.getRscGroup().substring(indx + 1,
                            rscName.getRscGroup().length() - 2); // "km"
                    paramsMap.put("areaName", areaName);
                    paramsMap.put("resolution", resStr);
                } else if (rscDefn.getRscImplementation().equals(
                        "GiniSatellite")) {
                    paramsMap.put("sectorID", rscName.getRscGroup());
                }
            }
        }

        // just one more hack we can't have PGEN save a .prm file for some
        // reason.
        if (rscDefn.isPgenResource()) {
            if (!rscName.getRscGroup().isEmpty()) {
                paramsMap.put("productName", rscName.getRscGroup() + ".xml");
                paramsMap.put("legendString", "PGEN XML File");
            }
        }

        return paramsMap;
    }

    public HashMap<String, String> readParamsFile(File paramsFile)
            throws VizException {
        // parse the parameter file to get the parameters to substitude into the
        // Bundle Template file.
        HashMap<String, String> rscParams = new HashMap<String, String>();

        if (paramsFile.length() == 0) {
            return rscParams;
        }

        try {
            FileReader freader = new FileReader(paramsFile);
            BufferedReader breader = new BufferedReader(freader);
            String prmStr = breader.readLine().trim();

            while (prmStr != null) {
                if (prmStr.isEmpty() || prmStr.charAt(0) == '!') { // comments
                    prmStr = breader.readLine();
                    continue;
                }

                int eq_indx = prmStr.indexOf('=');
                if (eq_indx == -1) {
                    throw new VizException("The resource prm file, "
                            + paramsFile.getName()
                            + ", has a non-comment line with no '='");
                    // prmStr = breader.readLine(); // uncomment if this is not
                    // considered a fatal error.
                    // continue;
                } else {
                    String prmKey = prmStr.substring(0, eq_indx).trim();
                    String prmVal = prmStr.substring(eq_indx + 1).trim();

                    // if the first char is an '@' then the following parameter
                    // value is
                    // located in an .xml file in the same directory or in the
                    // parent directory
                    if (!prmVal.isEmpty() && prmVal.charAt(0) == '@') {
                        try {
                            File prmValFile = new File(
                                    paramsFile.getParentFile(),
                                    prmVal.substring(1));

                            if (!prmValFile.exists()) {
                                prmValFile = new File(paramsFile
                                        .getParentFile().getParentFile(),
                                        prmVal.substring(1));
                                if (!prmValFile.exists()) {
                                    throw new VizException(
                                            "Error reading file: "
                                                    + paramsFile
                                                            .getAbsolutePath()
                                                    + " : Unable to find referenced "
                                                    + " file '" + prmVal + "'.");
                                }
                            }

                            FileReader fr = new FileReader(prmValFile);
                            char[] b = new char[(int) prmValFile.length()];
                            fr.read(b);
                            fr.close();

                            prmVal = new String(b);

                        } catch (FileNotFoundException fnf) {
                            throw new VizException(fnf);
                        } catch (IOException ioe) {
                            throw new VizException(ioe);
                        }
                    }

                    rscParams.put(prmKey.trim(), prmVal.trim());
                }
                prmStr = breader.readLine();
            }
        } catch (FileNotFoundException fnf) {
            throw new VizException("Can't find referenced file: "
                    + paramsFile.getAbsolutePath());
        } catch (IOException fnf) {
            throw new VizException("Can't open referenced file: "
                    + paramsFile.getAbsolutePath());
        }

        return rscParams;
    }

    // loop thru all the rsc defns for this cat and return a list of all
    // filter labels.
    public List<String> getFilterLabelsForResourceCategory(String rscCat) {

        ArrayList<String> filterLabelsList = new ArrayList<String>();

        for (ResourceDefinition rscDefn : resourceDefnsMap.values()) {

            if (!rscDefn.getIsEnabled()
                    || !rscDefn.getResourceCategory().equals(rscCat)) {
                continue;
            }

            for (String filtStr : rscDefn.getFilterLabels()) {

                if (!filterLabelsList.contains(filtStr)) {
                    filterLabelsList.add(filtStr);
                }
            }
        }

        return filterLabelsList;
    }

    public List<String> getResourceTypesForCategory(String rscCat,
            String filterStr, Boolean includeGeneratedTypes) {
        ArrayList<String> resourceTypes = new ArrayList<String>();

        // for other resources, get all of the resources in the given category.
        //
        for (ResourceDefinition rscDefn : resourceDefnsMap.values()) {
            if (!rscDefn.getIsEnabled()) {
                continue;
            }

            if (filterStr == null || filterStr.isEmpty()
                    || rscDefn.getFilterLabels().contains(filterStr)) {

                if (rscDefn.getResourceCategory().equals(rscCat)) {
                    if (rscDefn.getRscTypeGenerator().isEmpty()) {
                        resourceTypes.add(rscDefn.getResourceDefnName());
                    }

                    if (includeGeneratedTypes) {
                        resourceTypes.addAll(rscDefn.getGeneratedTypesList());
                    } else if (!rscDefn.getRscTypeGenerator().isEmpty()) {
                        resourceTypes.add(rscDefn.getResourceDefnName());
                    }
                }
                // List<String> rscCatList = rscDefn.getRscCategories();
                // if( rscCatList.contains( rscCat ) ) {
                // resourceTypes.add(rscDefn.getResourceDefnName() );
                // }
            }
        }
        String typesArray[] = resourceTypes.toArray(new String[0]);

        // sort with the Obs types first and then the Fcst, and then
        // alphabetically
        Arrays.sort(typesArray, new Comparator<String>() {
            @Override
            public int compare(String o1, String o2) {
                ResourceDefinition rscDefn1 = getResourceDefinition(o1);
                ResourceDefinition rscDefn2 = getResourceDefinition(o2);
                if (rscDefn1 == null)
                    return 1;
                if (rscDefn2 == null)
                    return -1;

                // categories will be the same for the types but we may want to
                // order them differently
                // based on the category
                //
                // for Surf or UAIR, Obs before Fcst
                if (rscDefn1.getResourceCategory().equals(
                        ResourceName.SurfaceRscCategory)
                        || rscDefn1.getResourceCategory().equals(
                                ResourceName.UpperAirRscCategory)) {

                    if ((!rscDefn1.isForecast() && rscDefn2.isForecast())
                            || (rscDefn1.isForecast() && !rscDefn2.isForecast())) {

                        return (rscDefn1.isForecast() ? 1 : -1);
                    }
                }
                // for Radar, Mosaics before Local Radar
                else if (rscDefn1.getResourceCategory().equals(
                        ResourceName.RadarRscCategory)) {
                    if (rscDefn1.getFilterLabels().contains("RadarMosaic")
                            || rscDefn1.getRscImplementation().equals(
                                    "RadarMosaic")) {
                        return -1;
                    } else if (rscDefn2.getFilterLabels().contains(
                            "RadarMosaic")
                            || rscDefn2.getRscImplementation().equals(
                                    "RadarMosaic")) {
                        return 1;
                    }
                }

                // otherwise return the order based on the index in the table
                if (rscDefn1.getDefinitionIndex() == rscDefn2
                        .getDefinitionIndex()) {
                    return (o1).compareToIgnoreCase(o2);
                } else {
                    return (rscDefn1.getDefinitionIndex() < rscDefn2
                            .getDefinitionIndex() ? -1 : 1);
                }
            }
        });

        return Arrays.asList(typesArray);
    }

    // if these resource type uses attributeSetGroups then return the specified
    // attrSetGroups, if not then look for groups which are 'sub-types' which
    // are
    // given as
    public String[] getResourceSubTypes(String rscType) {

        ResourceDefinition rscDefn = getResourceDefinition(rscType);

        if (rscDefn == null) {
            return new String[0];
        }

        // the sub-types are stored in the resourceDefnTable but they still
        // have to refer to a sub-directory in the rscConfig Dir.
        return rscDefn.getRscSubTypes();
    }

    // this is all of the attribute set groups available for a resource
    // (the returned list references the actual AttrSetGroup objects)
    //
    public ArrayList<AttrSetGroup> getAllAttrSetGroupsForResource(String rsc) {
        // loop thru all the entries in the attrSetGroupsMap and return those
        // that match the rscImpl
        ArrayList<AttrSetGroup> attrSetGroupsList = new ArrayList<AttrSetGroup>();

        if (findResourceDefinition(rsc)) {
            for (String asgName : getResourceAttrSetGroups(rsc)) {
                String mKey = rsc + File.separator + asgName;

                if (attrSetGroupsMap.containsKey(mKey)) {
                    attrSetGroupsList.add(attrSetGroupsMap.get(mKey));
                } else {
                    // this means that there is an AttrSetGroup in the
                    // resourceDefin File
                    // that is not in the AttrSetGroups file. This shouldn't
                    // happen.
                    System.out.println("sanity check: ");

                }
            }
        }

        return attrSetGroupsList;
    }

    // look up using a resource name
    public AttrSetGroup getAttrSetGroupForResource(ResourceName rscName) {
        String rscType = rscName.getRscType();
        String asgName = rscName.getRscGroup();

        // for PGEN the group in the rscName is the PGEN file and we need the
        // attrSetGroup name which is 'PGEN'
        if (rscName.isPgenResource()) {
            asgName = "PGEN";
        }

        return getAttrSetGroupForResource(rscType, asgName);
    }

    // lookup usging the rscType and the asg name
    public AttrSetGroup getAttrSetGroupForResource(String rscType,
            String asgName) {
        ResourceDefinition rscDefn = getResourceDefinition(rscType);

        if (rscDefn == null) {
            return null;
        }

        String asgKey = rscDefn.getResourceDefnName() + File.separator
                + asgName;

        // look up the attrSetGroup in the map and return it.
        // for PGEN the key
        if (asgName.equals("PGEN")) {
            asgKey = "PGEN";
        }

        return attrSetGroupsMap.get(asgKey);
    }

    public List<String> getResourceAttrSetGroups(String rscType) {
        ResourceDefinition rscDefn = getResourceDefinition(rscType);

        return (rscDefn == null ? null : rscDefn.getAttrSetGroupNames());
    }

    // this is all of the attr sets in the attrSetGroups directory for this
    // impl.
    //
    public ArrayList<String> getAvailAttrSetsForRscImpl(String rscImpl) {
        ArrayList<String> attrSetList = new ArrayList<String>();

        File asgRscImplDir = new File(attrSetGroupsDir.getAbsolutePath()
                + File.separator + rscImpl);
        if (asgRscImplDir.exists()) {
            String[] attrSetFilenames = asgRscImplDir
                    .list(new FilenameFilter() {
                        @Override
                        public boolean accept(File dir, String name) {
                            return name.endsWith(ATTR_SET_FILE_EXT);
                        }
                    });

            for (String attrSetFname : attrSetFilenames) {
                attrSetList.add(attrSetFname.substring(0, attrSetFname.length()
                        - ATTR_SET_FILE_EXT.length()));
            }
        }
        return attrSetList;
    }

    public String[] getAttrSetsForResource(ResourceName rscName,
            boolean matchGroup) {
        ResourceDefinition rscDefn = getResourceDefinition(rscName.getRscType());

        if (rscDefn == null) {
            return null;
        }

        if (matchGroup
                && (rscName.getRscCategory()
                        .equals(ResourceName.SatelliteRscCategory))) { // ||
            // rscName.getRscCategory().equals( ResourceName.RadarRscCategory )
            // ) )
            // {

            String rscGrp = rscName.getRscGroup();
            // map from the subType to a list of 'keys' for available
            // attributes.
            HashMap<String, ArrayList<String>> subTypesMap = rscDefn
                    .getSubTypesMap();

            if (rscGrp == null || rscGrp.isEmpty()) {
                if (rscName.getRscCategory().equals(
                        ResourceName.SatelliteRscCategory)) {
                    // System.out.println("Sanity Check: Satellite resource should have a group. "
                    // );
                    return new String[0]; //
                } else { // Radar Mosaic
                    return rscDefn.getAvailAttrSets().toArray(new String[0]);
                }
            }

            ArrayList<String> attrSets = new ArrayList<String>();

            ArrayList<String> availAttrSetKeys = subTypesMap.get(rscGrp);
            if (availAttrSetKeys == null) {
                System.out
                        .println("Sanity Check: Satellite subType is not in the subTypeMap?? ");
                return new String[0]; //
            }

            for (String asName : rscDefn.getAvailAttrSets()) {
                try {
                    HashMap<String, String> attrsMap = readParamsFile(rscDefn
                            .getAttrSetFile(asName));

                    if (rscName.getRscCategory().equals(
                            ResourceName.SatelliteRscCategory)) {
                        if (!attrsMap.containsKey("imageType")) {
                            System.out
                                    .println("Sanity Check: Satellite attrSets must have an "
                                            + " 'imageType' parameter.");
                        } else {
                            if (availAttrSetKeys.contains(attrsMap
                                    .get("imageType"))) {
                                attrSets.add(asName);
                            }
                        }
                    } else if (rscName.getRscCategory().equals(
                            ResourceName.RadarRscCategory)) {
                        if (!attrsMap.containsKey("productName")) {
                            System.out
                                    .println("Sanity Check: Radar attrSets must have an "
                                            + " 'productName' parameter.");
                        } else {
                            if (availAttrSetKeys.contains(attrsMap
                                    .get("productName"))) {
                                attrSets.add(asName);
                            }
                        }
                    }
                } catch (VizException e) {
                    System.out.println("Error parsing AttrSet " + asName
                            + " : " + e.getMessage());
                }
            }
            return attrSets.toArray(new String[0]);
        } else if (rscDefn.applyAttrSetGroups()) {
            AttrSetGroup asg = getAttrSetGroupForResource(rscName);
            if (asg != null) {
                return asg.getAttrSetNames().toArray(new String[0]);
            }
        } else {
            return rscDefn.getAvailAttrSets().toArray(new String[0]);
        }

        return new String[0]; //
    }

    public String getResourceImplementation(String rscType) {
        ResourceDefinition rscDefn = getResourceDefinition(rscType);

        return (rscDefn == null ? null : rscDefn.getRscImplementation());
    }

    // for the given attr set group return a list of all the resources that
    // it applies to.
    public ArrayList<String> getAllResourcesWithAttrSetGroup(String attrSetGroup) {
        ArrayList<String> rscNames = new ArrayList<String>();

        for (ResourceDefinition rscDefn : resourceDefnsMap.values()) {
            if (rscDefn.getAttrSetGroupNames().contains(attrSetGroup)) {
                rscNames.add(rscDefn.getResourceDefnName());
            }
        }

        return rscNames;
    }

    // add/replace the given attrSetGroup in the map.
    //
    public boolean saveAttrSetGroup(AttrSetGroup attrSetGroup, boolean replace) {
        if (attrSetGroup == null)
            return false;

        //
        String mapKey = attrSetGroup.getResource() + File.separator
                + attrSetGroup.getAttrSetGroupName();
        if (attrSetGroup.getAttrSetGroupName().equals("PGEN")) {
            mapKey = "PGEN";
        }

        if (attrSetGroupsMap.containsKey(mapKey) && !replace) {
            return false;
        }

        attrSetGroupsMap.put(mapKey, attrSetGroup);

        return true;
    }

    // remove this from the attrSetGroupsMap and remove the name from
    // the resource definition.
    public boolean removeAttrSetGroup(String asgName, String rscType) {
        //
        ResourceDefinition rscDefn = getResourceDefinition(rscType);

        if (rscDefn == null) {
            return false;
        }

        String mapKey = rscType + File.separator + asgName;

        if (asgName.equals("PGEN")) {
            mapKey = "PGEN";
        }

        if (attrSetGroupsMap.containsKey(mapKey)) {
            attrSetGroupsMap.remove(mapKey);
        } else {
            // sanity check?
        }

        rscDefn.removeAttrSetGroup(asgName);

        return true;
    }

    // remove the attr set file and remove the attr set name from the attr set
    // groups
    // that reference it
    //
    public boolean removeAttrSet(ResourceName rscName) throws VizException {

        ResourceDefinition rscDefn = getResourceDefinition(rscName);
        if (rscDefn == null) {
            return false;
        }

        String attrSetName = rscName.getRscAttrSetName();
        // String attrSetGroup = rscName.getRscGroup();

        // if removing an attr set that is part of a group then check for
        // references
        // to it in other groups
        //
        if (rscDefn.applyAttrSetGroups()) {
            String rscImpl = rscDefn.getRscImplementation();

            // loop thru all the resources for this implementation and
            // check if there is a reference to this attrSet.
            for (String rscType : getRscTypesForRscImplementation(rscImpl)) {

                ResourceDefinition rd = getResourceDefinition(rscType);

                if (rscDefn == rd) {
                }

                if (rd == null) { // sanity check
                    continue;
                }

                // loop thru all of the attrSetGroups for this resource
                //
                for (String asgName : rd.getAttrSetGroupNames()) {
                    String asgKey = rscDefn.getResourceDefnName()
                            + File.separator + asgName;

                    if (!attrSetGroupsMap.containsKey(asgKey)) { // sanity check
                        continue;
                    }

                    AttrSetGroup asg = attrSetGroupsMap.get(asgKey);

                    // if this attrSetGroup contains this attrSet then remove
                    // the reference.
                    //
                    if (asg != null
                            && asg.getAttrSetNames().contains(attrSetName)) {

                        asg.removeAttrSet(attrSetName);
                    }
                }
            }
        }

        // delete the file and take it out of the rsc dfn list
        File asFile = rscDefn.getAttrSetFile(rscName.getRscAttrSetName());

        if (asFile == null) {
            throw new VizException("Attr Set File: " + asFile.getAbsolutePath()
                    + " not found");
        }

        asFile.delete();

        findAvailAttrSets(rscDefn);

        return true;
    }

    // when changing the name of an attr set we will need to check if this is
    // in an attribute set group and if so
    public boolean renameAttrSet(ResourceName rscName, String newAttrSetName) {
        ResourceDefinition rscDefn = getResourceDefinition(rscName.getRscType());

        if (rscDefn == null) {
            return false;
        }

        if (rscDefn.applyAttrSetGroups()) {
            return true;
        }

        String rscImpl = rscDefn.getRscImplementation();

        if (rscImpl == null) {
            return false;
        }

        getAvailAttrSetsForRscImpl(rscImpl);
        return true;
    }

    public ArrayList<String> getRscTypesForRscImplementation(String rscImpl) {
        ArrayList<String> rscTypes = new ArrayList<String>();

        for (ResourceDefinition rscDefn : resourceDefnsMap.values()) {
            if (rscDefn.getRscImplementation().equals(rscImpl)) {
                rscTypes.add(rscDefn.getResourceDefnName());
            }
        }
        return rscTypes;
    }

    public File getPgenDataDir(ResourceName pgenRscType) {
        ResourceDefinition pgenRscDefn = getResourceDefinition(pgenRscType
                .getRscType());
        if (pgenRscDefn == null) {
            return null;
        } else if (!pgenRscDefn.isPgenResource()) {
            return null; // ?????
        }

        return new File(resourceDefnFile.getParentFile(),
        // LocalizationManager.getInstance().getFilename("resourceDefns"),
                pgenRscDefn.getConfigDir());
    }

    // create a new resource defn with the given name and the values from the
    // existing rscDefn
    // create the necessary config dir and prm files.
    //
    public ResourceDefinition createNewResourceDefn(String newName,
            ResourceDefinition rscDefn) throws VizException {

        ResourceDefinition newRscDefn = new ResourceDefinition(rscDefn);
        newRscDefn.setResourceDefnName(newName);

        // some resources have there own directory tree so in order to copy the
        //
        File cnfgDir = new File(resourceDefnFile.getParentFile(),
                rscDefn.getConfigDir());

        // create a new directory at the same level and save the new
        // dir name in the rsc defn.
        File newCnfgDir = new File(cnfgDir.getParentFile(), newName);

        // if the directory already exists or we create it
        if ((newCnfgDir.exists() && newCnfgDir.isDirectory())
                || newCnfgDir.mkdir()) {

            String newDirName = newCnfgDir.getAbsolutePath();

            newRscDefn.setConfigDir(newDirName.substring(resourceDefnFile
                    .getParentFile().getAbsolutePath().length() + 1));

            // Create a .prm file in the new config directory, write out the
            // params
            // and copy the default attribute set and any referenced attr files
            try {
                if (!rscDefn.getRscTypeParameters().isEmpty()) {
                    String newPrmFilename = newDirName + File.separator
                            + newName + ".prm";
                    File newPrmFile = new File(newPrmFilename);

                    if (!newPrmFile.exists()) {
                        newPrmFile.createNewFile();
                    }

                    FileWriter fwriter = new FileWriter(newPrmFile);

                    fwriter.write(rscDefn.getRscTypeParameters());
                    fwriter.close();
                }

                // if this resource uses attrSetGroups, create an attrSetGroup
                // and add it to the map
                //
                if (rscDefn.applyAttrSetGroups()) {
                    if (!rscDefn.isPgenResource()) {
                        // create new attrSetGroups for the new resource.
                        ArrayList<String> asgNames = rscDefn
                                .getAttrSetGroupNames();

                        for (String asgName : asgNames) {
                            AttrSetGroup attrSetGroup = getAttrSetGroupForResource(
                                    rscDefn.getResourceDefnName(), asgName);
                            if (attrSetGroup != null) {
                                AttrSetGroup newAttrSetGroup = new AttrSetGroup(
                                        attrSetGroup);

                                newAttrSetGroup.setResource(newRscDefn
                                        .getResourceDefnName());

                                saveAttrSetGroup(newAttrSetGroup, false);
                            }
                        }
                    }
                } else { // else this resource has its own attr sets, so copy
                         // the default
                    Set<String> availAttrSets = rscDefn.getAvailAttrSets();

                    if (!availAttrSets.isEmpty()) { // there should be at least
                                                    // one attrSet

                        // if there is no default then just get the first to
                        // give the user
                        // something to edit
                        File dfltAttrSetFile = null;

                        if (availAttrSets.contains("default")) {
                            dfltAttrSetFile = rscDefn.getAttrSetFile("default");
                        } else {
                            dfltAttrSetFile = rscDefn
                                    .getAttrSetFile(availAttrSets.iterator()
                                            .next());
                        }

                        File newDfltAttrSetFile = new File(newDirName,
                                "default.attr");

                        FileReader in = new FileReader(dfltAttrSetFile);
                        FileWriter out = new FileWriter(newDfltAttrSetFile);
                        int c;

                        while ((c = in.read()) != -1) {
                            out.write(c);
                        }

                        in.close();
                        out.close();

                        // if there is a referenced attr (ex.
                        // @dfltMcidasColorBar) then copy it
                        //
                    }
                }

            } catch (FileNotFoundException fnf) {
                throw new VizException(fnf);
            } catch (IOException ioe) {
                throw new VizException(ioe);
            }

            // add the new rsc defn to the map
            //
            resourceDefnsMap.put(newRscDefn.getResourceDefnName(), newRscDefn);

            findAvailAttrSets(newRscDefn);

            // remove the subTypes and generate
            // (do this after it is added to the map since the call ends up
            // needing to look up
            // the rescDefn in the map.)
            if (!newRscDefn.getSubTypeGenerator().isEmpty()
                    || !newRscDefn.getRscTypeGenerator().isEmpty()) {

                generateDynamicResources(newRscDefn);
            }
        } else {
            throw new VizException(
                    "Error creating config dir for new resource: " + newName);
        }

        return newRscDefn;
    }

    // remove the existing entry from the map and replace with a new entry for
    // the new name.
    //
    public boolean renameResourceDefn(String existingType, String newType) {
        ResourceDefinition rscDefn = getResourceDefinition(existingType);

        if (rscDefn == null) {
            return false;
        }

        // if using AttrSetGroups then change the name in the map.
        // (can't use removeAttrSetGroup and saveAttrSetGroup since they will
        // get
        // confused with old/new types in the maps.)
        if (rscDefn.applyAttrSetGroups()) {
            if (!rscDefn.isPgenResource()) {

                // fetch each of the attrSetGroups for this resource change the
                // resource name and update the map
                ArrayList<String> asgNames = rscDefn.getAttrSetGroupNames();

                for (String asgName : asgNames) {
                    String mapKey = existingType + File.separator + asgName; // not
                                                                             // pgen

                    if (attrSetGroupsMap.containsKey(mapKey)) {

                        AttrSetGroup attrSetGroup = attrSetGroupsMap
                                .get(mapKey);
                        attrSetGroupsMap.remove(mapKey);

                        attrSetGroup.setResource(newType);

                        mapKey = newType + File.separator + asgName;

                        attrSetGroupsMap.put(mapKey, attrSetGroup);
                    }
                }
            }
        }

        File prmFile = getResourceParamsFile(rscDefn.getResourceDefnName());

        rscDefn.setResourceDefnName(newType);

        // rename the config dir
        File cnfgDir = new File(resourceDefnFile.getParentFile(),
                rscDefn.getConfigDir());

        File newCnfgDir = new File(cnfgDir.getParentFile(), newType);

        if (!cnfgDir.renameTo(newCnfgDir)) {
            System.out
                    .println("Error renaming the config directory for resource defn: "
                            + rscDefn.getResourceDefnName());
        }

        if (!newCnfgDir.exists()) {
            System.out
                    .println("Error renaming config dir for renamed resource: "
                            + newType);
            return false;
        }

        String newDirName = newCnfgDir.getAbsolutePath();
        newDirName = newDirName.substring(resourceDefnFile.getParentFile()
                .getAbsolutePath().length() + 1);
        rscDefn.setConfigDir(newDirName);

        // and rename the .prm file (except for PGEN since there is only 1)
        //
        if (!rscDefn.isPgenResource() && prmFile != null) {
            try {
                File newPrmFile = new File(newCnfgDir, newType + ".prm");
                FileWriter fwriter = new FileWriter(newPrmFile);

                fwriter.write(rscDefn.getRscTypeParameters());
                fwriter.close();

                prmFile.delete();

            } catch (FileNotFoundException fnf) {
                // Log error
                System.out.println("Error writing prm file for "
                        + rscDefn.getResourceDefnName());
            } catch (IOException ioe) {
                // Log error
                System.out.println("Error writing prm file for "
                        + rscDefn.getResourceDefnName());
            }
        }

        // reset the list of attr files since we just moved them.
        findAvailAttrSets(rscDefn);

        resourceDefnsMap.remove(existingType);

        resourceDefnsMap.put(newType, rscDefn);

        return true;
    }

    public boolean saveResourceDefn(ResourceDefinition rscDefn) {
        if (!resourceDefnsMap.containsKey(rscDefn.getResourceDefnName())) {
            System.out.println("sanity check: can't find rsc type, "
                    + rscDefn.getResourceDefnName() + ", in "
                    + "resourceDefnsMap?");
            return false;
        }

        resourceDefnsMap.put(rscDefn.getResourceDefnName(), rscDefn);

        return true;
    }

    public ArrayList<String> getAvailPgenTypes() {
        ArrayList<String> pgenTypes = new ArrayList<String>();
        for (ResourceDefinition rscDefn : resourceDefnsMap.values()) {
            if (rscDefn.isPgenResource()) {
                pgenTypes.add(rscDefn.getResourceDefnName());
            }
        }
        return pgenTypes;
    }

    public boolean removePgenResource(ResourceName pgenRscName) {
        // for PGEN the RscTypes are groupings/(directories) for the pgen files.
        //
        String pgenGroup = pgenRscName.getRscType();

        ResourceDefinition rscDefn = getResourceDefinition(pgenGroup);

        if (rscDefn == null) {
            return false;
        }

        File dataDir = new File(resourceDefnFile.getParentFile(),
                rscDefn.getConfigDir());

        File pgenPrmFile = new File(dataDir, pgenRscName.getRscGroup() + ".prm");
        File pgenDataFile = new File(dataDir, pgenRscName.getRscGroup()
                + ".xml");

        if (!pgenPrmFile.exists() && !pgenDataFile.exists()) {
            System.out.println("");
            return false;
        }

        if (pgenPrmFile.exists()) {
            pgenPrmFile.delete();
        }
        if (pgenDataFile.exists()) {
            pgenDataFile.delete();
        }

        return true;
    }
}
