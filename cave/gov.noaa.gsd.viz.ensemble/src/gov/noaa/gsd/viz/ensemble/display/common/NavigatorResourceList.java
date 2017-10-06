package gov.noaa.gsd.viz.ensemble.display.common;

import gov.noaa.gsd.viz.ensemble.control.EnsembleResourceManager;
import gov.noaa.gsd.viz.ensemble.navigator.ui.layer.EnsembleToolLayer;

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.xy.timeseries.display.TimeSeriesDescriptor;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResource;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Access and operate the Ensemble Tool resources. Any resources contained in
 * this list class will be associated with an editor and is assumed to be
 * controlled by the Ensemble Tool.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2014    5056     jing        Initial creation
 * Jan 17, 2016    13211    polster     Renamed GenericResourceHolder 
 *                                      to AbstractResourceHolder
 * Feb 08  2016    13211    polster     Use diamond operator to reduce verbosity
 * 
 * </pre>
 * 
 * @author jing
 * @author polster
 * @version 1.0
 */

public class NavigatorResourceList {

    public final static String TIME_SERIES_POINT_EMPTY_STR = "<no associated point>";

    public final static String TIME_BASIS_EMPTY_STR = "<no associated time>";

    private List<AbstractResourceHolder> ensembleToolResources;

    private EnsembleToolLayer theToolLayer;

    // contains which ensemble resource is currently used as
    // the calculation resource ...
    String calculationEnsembleName = null;

    public NavigatorResourceList(EnsembleToolLayer layer) {
        ensembleToolResources = new CopyOnWriteArrayList<>();
        theToolLayer = layer;
    }

    public boolean add(AbstractResourceHolder emr) {

        if (!containsResource(emr)) {
            ensembleToolResources.add(emr);
            return true;
        }
        return false;
    }

    public boolean remove(AbstractResourceHolder emr) {

        boolean removed = false;
        for (AbstractResourceHolder gr : ensembleToolResources) {
            if (gr == emr) {
                ensembleToolResources.remove(gr);
                removed = true;
            }
        }
        return removed;
    }

    public boolean remove(AbstractVizResource<?, ?> rcs) {

        boolean removed = false;
        for (AbstractResourceHolder gr : ensembleToolResources) {
            if (gr.getRsc() == rcs) {
                ensembleToolResources.remove(gr);
                removed = true;
            }
        }
        return removed;
    }

    public void clear() {

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                for (AbstractResourceHolder arh : ensembleToolResources) {
                    arh.getRsc().unload();
                }
                ensembleToolResources.clear();
                EditorUtil.getActiveVizContainer().refresh();
            }
        });

    }

    public List<AbstractResourceHolder> getResourceHolders() {
        return ensembleToolResources;
    }

    public boolean containsResource(AbstractResourceHolder rsc) {

        for (AbstractResourceHolder emr : ensembleToolResources) {
            if (emr.equals(rsc)) {
                return true;
            }
        }
        return false;
    }

    public boolean containsResource(AbstractVizResource<?, ?> rsc) {

        for (AbstractResourceHolder emr : ensembleToolResources) {
            if ((emr.getRsc() == rsc) || (emr.getRsc().equals(rsc))) {
                return true;
            }
        }
        return false;

    }

    /**
     * All resources
     * 
     * @return
     */
    public List<AbstractResourceHolder> getAllRscsAsList() {
        List<AbstractResourceHolder> rscs = new CopyOnWriteArrayList<>();
        for (AbstractResourceHolder emr : ensembleToolResources) {
            rscs.add(emr);
        }

        return rscs;
    }

    /**
     * Get all loaded and generated resources as GenericResourceHolder
     * instances.
     */
    public Map<String, List<AbstractResourceHolder>> getAllRscsAsMap() {

        Map<String, List<AbstractResourceHolder>> rscMap = new ConcurrentHashMap<>();

        EnsembleResourceManager.getInstance().checkExistingRegisteredResources(
                theToolLayer);

        List<AbstractResourceHolder> rscs = EnsembleResourceManager
                .getInstance().getResourceList(theToolLayer)
                .getUserLoadedRscs();

        String currRscName = null;

        // let's loop through and find the group names
        // and store them in a set of strings (no duplicates
        // allowed) so sub-members such as perturbations will
        // only be counted once.
        Set<String> rscGroupNames = new HashSet<String>();
        for (AbstractResourceHolder rsc : rscs) {
            currRscName = rsc.getGroupName();
            if (!rscGroupNames.contains(currRscName)) {
                rscGroupNames.add(currRscName);
            }
        }
        Iterator<String> iter = rscGroupNames.iterator();
        while (iter.hasNext()) {
            String currRsc = iter.next();
            List<AbstractResourceHolder> members = findAssociatedMembers(currRsc);
            if (!members.isEmpty()) {
                rscMap.put(currRsc, members);
            }
        }

        // Generated resource add on
        rscs = EnsembleResourceManager.getInstance()
                .getResourceList(theToolLayer).getUserGeneratedRscs();
        for (AbstractResourceHolder rsc : rscs) {
            currRscName = rsc.getSpecificName();
            if ((currRscName == null) || (currRscName.length() == 0)) {
                currRscName = rsc.getRsc().getName();
            }
            List<AbstractResourceHolder> members = new CopyOnWriteArrayList<>();
            members.add(rsc);

            rscMap.put(currRscName, members);
        }

        return rscMap;
    }

    /**
     * Get all loaded and generated resources as GenericResourceHolder
     * instances.
     */
    public Map<String, List<AbstractResourceHolder>> getAllRscsAsMap(
            Map<String, List<AbstractResourceHolder>> rscMap) {

        rscMap.clear();

        EnsembleResourceManager.getInstance().checkExistingRegisteredResources(
                theToolLayer);

        List<AbstractResourceHolder> rscs = EnsembleResourceManager
                .getInstance().getResourceList(theToolLayer)
                .getUserLoadedRscs();

        String currRscName = null;

        // let's loop through and find the group names
        // and store them in a set of strings (no duplicates
        // allowed) so sub-members such as perturbations will
        // only be counted once.
        Set<String> rscGroupNames = new HashSet<>();
        for (AbstractResourceHolder rsc : rscs) {
            currRscName = rsc.getGroupName();
            if (!rscGroupNames.contains(currRscName)) {
                rscGroupNames.add(currRscName);
            }
        }
        Iterator<String> iter = rscGroupNames.iterator();
        while (iter.hasNext()) {
            String currRsc = iter.next();
            List<AbstractResourceHolder> members = findAssociatedMembers(currRsc);
            if (!members.isEmpty()) {
                rscMap.put(currRsc, members);
            }
        }

        // Generated resource add on
        rscs = EnsembleResourceManager.getInstance()
                .getResourceList(theToolLayer).getUserGeneratedRscs();
        for (AbstractResourceHolder rsc : rscs) {
            currRscName = rsc.getSpecificName();
            if ((currRscName == null) || (currRscName.length() == 0)) {
                currRscName = rsc.getRsc().getName();
            }
            List<AbstractResourceHolder> members = new CopyOnWriteArrayList<>();
            members.add(rsc);

            rscMap.put(currRscName, members);
        }

        return rscMap;
    }

    private List<AbstractResourceHolder> findAssociatedMembers(
            String resourceGroupName) {

        List<AbstractResourceHolder> rscs = EnsembleResourceManager
                .getInstance().getResourceList(theToolLayer)
                .getUserLoadedRscs();

        List<AbstractResourceHolder> members = new CopyOnWriteArrayList<>();

        for (AbstractResourceHolder rsc : rscs) {

            if (rsc.getSpecificName().startsWith(resourceGroupName)) {
                members.add(rsc);
            }
        }

        return members;
    }

    /**
     * All resources for an editor that were not generated.
     * 
     * @return
     */
    public List<AbstractResourceHolder> getUserLoadedRscs() {
        EnsembleResourceManager.getInstance().checkExistingRegisteredResources(
                theToolLayer);

        List<AbstractResourceHolder> rscs = new CopyOnWriteArrayList<>();
        for (AbstractResourceHolder emr : ensembleToolResources) {
            if (!emr.isGenerated()) {
                rscs.add(emr);
            }
        }

        return rscs;
    }

    /**
     * All loaded resources of an ensemble model using its unique name (e.g
     * "SREF 500MB").
     * 
     * @return
     */
    public List<AbstractResourceHolder> getUserLoadedRscs(
            String uniqueResourceName) {
        if (uniqueResourceName == null)
            return null;

        EnsembleResourceManager.getInstance().checkExistingRegisteredResources(
                theToolLayer);

        List<AbstractResourceHolder> rscs = new CopyOnWriteArrayList<>();
        for (AbstractResourceHolder emr : ensembleToolResources) {
            if (!emr.isGenerated) {
                String fullName = emr.getGroupName();
                if (fullName.equals(uniqueResourceName)) {
                    rscs.add(emr);
                }
            }
        }

        return rscs;
    }

    /**
     * All loaded resources for ensemble.
     * 
     * @param descriptor
     *            : the descriptor of the resource
     * @param selected
     *            :if it's selected.
     * @return
     */
    public Map<String, List<AbstractResourceHolder>> getUserLoadedRscs(
            IDescriptor descriptor, boolean selected) {

        EnsembleResourceManager.getInstance().checkExistingRegisteredResources(
                theToolLayer);

        Map<String, List<AbstractResourceHolder>> rscMap = new ConcurrentHashMap<>();
        for (AbstractResourceHolder emr : ensembleToolResources) {
            if (!emr.isGenerated
                    && emr.isSelected == selected
                    && emr.getRsc().getDescriptor().getClass() == descriptor
                            .getClass()) { // same type descriptor like Map
                if (emr.getRsc().getName() == null
                        || emr.getRsc().getName().equals(""))
                    continue;
                String model = emr.getModel();
                addResource2Map(rscMap, model, emr);
            }
        }

        return rscMap;
    }

    /**
     * All loaded resources for ensemble.
     * 
     * @param descriptor
     *            :the descriptor of the resource
     * @param selected
     *            :if it's selected.
     * @param level
     *            :level of the resource.
     * @param unit
     *            : unit of the resource.
     * @return
     */
    public Map<String, List<AbstractResourceHolder>> getUserLoadedRscs(
            IDescriptor descriptor, boolean selected, String level, String unit) {
        EnsembleResourceManager.getInstance().checkExistingRegisteredResources(
                theToolLayer);

        Map<String, List<AbstractResourceHolder>> rscMap = new ConcurrentHashMap<>();

        for (AbstractResourceHolder emr : ensembleToolResources) {
            if (descriptor instanceof IMapDescriptor
                    && emr.getRsc().getDescriptor() instanceof IMapDescriptor) {

                if (!emr.isGenerated
                        && emr.isSelected == selected
                        && level.equals(emr.getLevel())
                        && unit.equals(emr.getUnits())
                        // same type descriptor like Map
                        && emr.getRsc().getDescriptor().getClass() == descriptor
                                .getClass()) {

                    // Model name is the first string
                    if (emr.getRsc().getName() == null
                            || emr.getRsc().getName().equals(""))
                        continue;
                    String model = emr.getModel();
                    addResource2Map(rscMap, model, emr);
                }
            } else if (descriptor instanceof TimeSeriesDescriptor
                    && emr.getRsc().getDescriptor() instanceof TimeSeriesDescriptor) {

                if (!emr.isGenerated
                        && emr.isSelected == selected
                        /**
                         * Remember to handle surface level name issue (e.g.
                         * "0SURFACE")
                         */
                        && level.equals(emr.getLevel())
                        && unit.equals(((TimeSeriesResource) (emr.getRsc()))
                                .getUnits())
                        && emr.getRsc().getDescriptor().getClass() == descriptor
                                .getClass()) { // same type descriptor like Map

                    // Model name is the first string
                    if (emr.getRsc().getName() == null
                            || emr.getRsc().getName().equals(""))
                        continue;
                    String model = emr.getModel();
                    addResource2Map(rscMap, model, emr);
                }
            }
        }

        return rscMap;
    }

    /**
     * All loaded resources for ensemble.
     * 
     * @param descriptor
     *            :the descriptor of the resource
     * @param selected
     *            :if it's selected.
     * @param unit
     *            : unit of the resource.
     * @return
     */
    public Map<String, List<AbstractResourceHolder>> getUserLoadedRscs(
            IDescriptor descriptor, boolean selected, String unit) {
        EnsembleResourceManager.getInstance().checkExistingRegisteredResources(
                theToolLayer);

        Map<String, List<AbstractResourceHolder>> rscMap = new ConcurrentHashMap<>();
        for (AbstractResourceHolder emr : ensembleToolResources) {
            if (!emr.isGenerated
                    && emr.isSelected == selected
                    && unit.equals(emr.getUnits())
                    // same type descriptor like Map
                    && emr.getRsc().getDescriptor().getClass() == descriptor
                            .getClass()) {

                if (emr.getRsc().getName() == null
                        || emr.getRsc().getName().equals(""))
                    continue;
                String model = emr.getModel();
                addResource2Map(rscMap, model, emr);
            }
        }

        return rscMap;
    }

    /**
     * All generated resources by ensemble display
     * 
     * @return
     */
    public List<AbstractResourceHolder> getUserGeneratedRscs() {
        List<AbstractResourceHolder> rscs = new CopyOnWriteArrayList<>();
        for (AbstractResourceHolder emr : ensembleToolResources) {
            if (emr.isGenerated) {
                rscs.add(emr);
            }
        }

        return rscs;
    }

    public List<AbstractResourceHolder> getUserGeneratedRscs(boolean selected) {
        List<AbstractResourceHolder> rscs = new CopyOnWriteArrayList<>();
        for (AbstractResourceHolder emr : ensembleToolResources) {
            if (!emr.isGenerated && emr.isSelected == selected) {
                rscs.add(emr);
            }
        }

        return rscs;
    }

    /**
     * 
     * @param rscMap
     * @param model
     * @param gr
     */
    private void addResource2Map(
            Map<String, List<AbstractResourceHolder>> rscMap, String model,
            AbstractResourceHolder gr) {
        if (rscMap.containsKey(model)) {
            rscMap.get(model).add(gr);
        } else {
            List<AbstractResourceHolder> rscList = new CopyOnWriteArrayList<>();
            rscList.add(gr);
            rscMap.put(model, rscList);
        }
    }

    public boolean isEmpty() {
        return ensembleToolResources.isEmpty();
    }

    /*
     * Get the Point letter id and lon/lat location as a String
     */
    public String getTimeSeriesPoint() {

        String timeSeriesPoint = null;

        if (ensembleToolResources.isEmpty()) {
            timeSeriesPoint = TIME_SERIES_POINT_EMPTY_STR;
        } else {
            AbstractResourceHolder g = ensembleToolResources.get(0);
            timeSeriesPoint = g.getLocation();
        }

        return timeSeriesPoint;

    }

    // Currently get the first resource loaded to get time from
    public String getTimeBasisLegendTime() {

        EnsembleResourceManager.getInstance().checkExistingRegisteredResources(
                theToolLayer);
        // for now this is the first resource name loaded into this editor
        String timeBasisLegendTime = null;

        if (ensembleToolResources.isEmpty()) {
            timeBasisLegendTime = TIME_BASIS_EMPTY_STR;
        } else {
            AbstractResourceHolder g = ensembleToolResources.get(0);
            timeBasisLegendTime = g.getDataTime();
        }

        return timeBasisLegendTime;
    }

    public String getTimeBasisResourceName() {

        EnsembleResourceManager.getInstance().checkExistingRegisteredResources(
                theToolLayer);
        // for now this is the first resource name loaded into this editor
        String timeBasisResourceName = null;

        if (ensembleToolResources.isEmpty()) {
            timeBasisResourceName = "";
        } else {
            AbstractResourceHolder g = ensembleToolResources.get(0);
            timeBasisResourceName = g.getGroupName();
        }

        return timeBasisResourceName;
    }

    synchronized public void setEnsembleCalculationResource(String rscGroupName) {
        calculationEnsembleName = rscGroupName;
    }

    synchronized public String getEnsembleCalculationResource() {
        return calculationEnsembleName;
    }

    // only update if the calculationEnsembleName is not already
    // set ... this will guarantee that only the first actual ensemble
    // resource will get set as the ensemble calculation resource.
    public void updateEnsembleCalculationResource() {
        if (calculationEnsembleName != null) {
            return;
        } else {
            Map<String, List<AbstractResourceHolder>> resourceMap = getAllRscsAsMap();

            List<AbstractResourceHolder> resources = null;
            Set<String> keys = resourceMap.keySet();
            Iterator<String> keyNames = keys.iterator();
            String currKey = null;
            while (keyNames.hasNext()) {
                currKey = keyNames.next();
                resources = resourceMap.get(currKey);
                // ensemble resource found
                if (resources.size() > 1) {
                    calculationEnsembleName = currKey;
                    break;
                }
            }
        }
    }

    public Map<String, List<AbstractResourceHolder>> getCalculationLoadedRscs(
            IDescriptor descriptor, boolean selected) {

        EnsembleResourceManager.getInstance().checkExistingRegisteredResources(
                theToolLayer);

        Map<String, List<AbstractResourceHolder>> rscMap = new ConcurrentHashMap<>();
        for (AbstractResourceHolder emr : ensembleToolResources) {
            if (!emr.isGenerated
                    && emr.isSelected == selected
                    && emr.getGroupName().equals(calculationEnsembleName)
                    // same type descriptor like Map
                    && emr.getRsc().getDescriptor().getClass() == descriptor
                            .getClass()) {

                if (emr.getRsc().getName() == null
                        || emr.getRsc().getName().equals(""))
                    continue;
                String model = emr.getModel();
                addResource2Map(rscMap, model, emr);
            }
        }

        return rscMap;

    }

    public int getSize() {
        int size = 0;
        if (ensembleToolResources != null) {
            size = ensembleToolResources.size();
        }
        return size;
    }

}
