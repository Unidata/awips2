package gov.noaa.gsd.viz.ensemble.display.common;

import gov.noaa.gsd.viz.ensemble.display.control.EnsembleResourceManager;

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.xy.timeseries.display.TimeSeriesDescriptor;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResource;
import com.raytheon.viz.ui.editor.AbstractEditor;

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
 * Nov 17, 2014    5056     jing     Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @author polster
 * @version 1.0
 */

public class NavigatorResourceList {

    private List<GenericResourceHolder> ensembleToolResources;

    private AbstractEditor theEditor;

    // contains which ensemble resource is currently used as
    // the calculation resource ...
    String calculationEnsembleName = null;

    public NavigatorResourceList(AbstractEditor editor) {
        ensembleToolResources = new CopyOnWriteArrayList<GenericResourceHolder>();
        theEditor = editor;
    }

    public boolean add(GenericResourceHolder emr) {

        if (!containsResource(emr)) {
            ensembleToolResources.add(emr);
            return true;
        }
        return false;
    }

    public boolean remove(GenericResourceHolder emr) {

        boolean removed = false;
        for (GenericResourceHolder gr : ensembleToolResources) {
            if (gr == emr) {
                ensembleToolResources.remove(gr);
                removed = true;
            }
        }
        return removed;
    }

    public boolean remove(AbstractVizResource<?, ?> rcs) {

        boolean removed = false;
        for (GenericResourceHolder gr : ensembleToolResources) {
            if (gr.getRsc() == rcs) {
                ensembleToolResources.remove(gr);
                removed = true;
            }
        }
        return removed;
    }

    public void clear() {
        ensembleToolResources.clear();
    }

    public List<GenericResourceHolder> getResourceHolders() {
        return ensembleToolResources;
    }

    public boolean containsResource(GenericResourceHolder rsc) {

        for (GenericResourceHolder emr : ensembleToolResources) {
            if (emr.equals(rsc)) {
                return true;
            }
        }
        return false;
    }

    public boolean containsResource(AbstractVizResource<?, ?> rsc) {

        for (GenericResourceHolder emr : ensembleToolResources) {
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
    public List<GenericResourceHolder> getAllRscsAsList() {
        List<GenericResourceHolder> rscs = new CopyOnWriteArrayList<GenericResourceHolder>();
        for (GenericResourceHolder emr : ensembleToolResources) {
            rscs.add(emr);
        }

        return rscs;
    }

    /**
     * Get all loaded and generated resources as GenericResourceHolder
     * instances.
     */
    public Map<String, List<GenericResourceHolder>> getAllRscsAsMap() {

        Map<String, List<GenericResourceHolder>> rscMap = new ConcurrentHashMap<String, List<GenericResourceHolder>>();

        EnsembleResourceManager.getInstance().syncRegisteredResource(theEditor);

        List<GenericResourceHolder> rscs = EnsembleResourceManager
                .getInstance().getResourceList(theEditor).getUserLoadedRscs();

        String currRscName = null;

        // let's loop through and find the group names
        // and store them in a set of strings (no duplicates
        // allowed) so sub-members such as perturbations will
        // only be counted once.
        Set<String> rscGroupNames = new HashSet<String>();
        for (GenericResourceHolder rsc : rscs) {
            currRscName = rsc.getGroupName();
            if (!rscGroupNames.contains(currRscName)) {
                rscGroupNames.add(currRscName);
            }
        }
        Iterator<String> iter = rscGroupNames.iterator();
        while (iter.hasNext()) {
            String currRsc = iter.next();
            List<GenericResourceHolder> members = findAssociatedMembers(currRsc);
            if (!members.isEmpty()) {
                rscMap.put(currRsc, members);
            }
        }

        // Generated resource add on
        rscs = EnsembleResourceManager.instance.getResourceList(theEditor)
                .getUserGeneratedRscs();
        for (GenericResourceHolder rsc : rscs) {
            currRscName = rsc.getUniqueName();
            if ((currRscName == null) || (currRscName.length() == 0)) {
                currRscName = rsc.getRsc().getName();
            }
            List<GenericResourceHolder> members = new CopyOnWriteArrayList<GenericResourceHolder>();
            members.add(rsc);

            rscMap.put(currRscName, members);
        }

        return rscMap;
    }

    /**
     * Get all loaded and generated resources as GenericResourceHolder
     * instances.
     */
    public Map<String, List<GenericResourceHolder>> getAllRscsAsMap(
            Map<String, List<GenericResourceHolder>> rscMap) {

        rscMap.clear();

        EnsembleResourceManager.getInstance().syncRegisteredResource(theEditor);

        List<GenericResourceHolder> rscs = EnsembleResourceManager.instance
                .getResourceList(theEditor).getUserLoadedRscs();

        String currRscName = null;

        // let's loop through and find the group names
        // and store them in a set of strings (no duplicates
        // allowed) so sub-members such as perturbations will
        // only be counted once.
        Set<String> rscGroupNames = new HashSet<String>();
        for (GenericResourceHolder rsc : rscs) {
            currRscName = rsc.getGroupName();
            if (!rscGroupNames.contains(currRscName)) {
                rscGroupNames.add(currRscName);
            }
        }
        Iterator<String> iter = rscGroupNames.iterator();
        while (iter.hasNext()) {
            String currRsc = iter.next();
            List<GenericResourceHolder> members = findAssociatedMembers(currRsc);
            if (!members.isEmpty()) {
                rscMap.put(currRsc, members);
            }
        }

        // Generated resource add on
        rscs = EnsembleResourceManager.instance.getResourceList(theEditor)
                .getUserGeneratedRscs();
        for (GenericResourceHolder rsc : rscs) {
            currRscName = rsc.getUniqueName();
            if ((currRscName == null) || (currRscName.length() == 0)) {
                currRscName = rsc.getRsc().getName();
            }
            List<GenericResourceHolder> members = new CopyOnWriteArrayList<GenericResourceHolder>();
            members.add(rsc);

            rscMap.put(currRscName, members);
        }

        return rscMap;
    }

    private List<GenericResourceHolder> findAssociatedMembers(
            String resourceGroupName) {

        List<GenericResourceHolder> rscs = EnsembleResourceManager.instance
                .getResourceList(theEditor).getUserLoadedRscs();

        List<GenericResourceHolder> members = new CopyOnWriteArrayList<GenericResourceHolder>();

        for (GenericResourceHolder rsc : rscs) {

            if (rsc.getUniqueName().startsWith(resourceGroupName)) {
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
    public List<GenericResourceHolder> getUserLoadedRscs() {
        EnsembleResourceManager.getInstance().syncRegisteredResource(theEditor);

        List<GenericResourceHolder> rscs = new CopyOnWriteArrayList<GenericResourceHolder>();
        for (GenericResourceHolder emr : ensembleToolResources) {
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
    public List<GenericResourceHolder> getUserLoadedRscs(
            String uniqueResourceName) {
        if (uniqueResourceName == null)
            return null;

        EnsembleResourceManager.getInstance().syncRegisteredResource(theEditor);

        List<GenericResourceHolder> rscs = new CopyOnWriteArrayList<GenericResourceHolder>();
        for (GenericResourceHolder emr : ensembleToolResources) {
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
    public Map<String, List<GenericResourceHolder>> getUserLoadedRscs(
            IDescriptor descriptor, boolean selected) {

        EnsembleResourceManager.getInstance().syncRegisteredResource(theEditor);

        Map<String, List<GenericResourceHolder>> rscMap = new ConcurrentHashMap<String, List<GenericResourceHolder>>();
        for (GenericResourceHolder emr : ensembleToolResources) {
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
    public Map<String, List<GenericResourceHolder>> getUserLoadedRscs(
            IDescriptor descriptor, boolean selected, String level, String unit) {
        EnsembleResourceManager.getInstance().syncRegisteredResource(theEditor);

        Map<String, List<GenericResourceHolder>> rscMap = new ConcurrentHashMap<String, List<GenericResourceHolder>>();

        for (GenericResourceHolder emr : ensembleToolResources) {
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
                        && level.equals(((TimeSeriesResource) (emr.getRsc()))
                                .getResourceData().getLevelKey())
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
    public Map<String, List<GenericResourceHolder>> getUserLoadedRscs(
            IDescriptor descriptor, boolean selected, String unit) {
        EnsembleResourceManager.getInstance().syncRegisteredResource(theEditor);

        Map<String, List<GenericResourceHolder>> rscMap = new ConcurrentHashMap<String, List<GenericResourceHolder>>();
        for (GenericResourceHolder emr : ensembleToolResources) {
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
    public List<GenericResourceHolder> getUserGeneratedRscs() {
        List<GenericResourceHolder> rscs = new CopyOnWriteArrayList<GenericResourceHolder>();
        for (GenericResourceHolder emr : ensembleToolResources) {
            if (emr.isGenerated) {
                rscs.add(emr);
            }
        }

        return rscs;
    }

    public List<GenericResourceHolder> getUserGeneratedRscs(boolean selected) {
        List<GenericResourceHolder> rscs = new CopyOnWriteArrayList<GenericResourceHolder>();
        for (GenericResourceHolder emr : ensembleToolResources) {
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
            Map<String, List<GenericResourceHolder>> rscMap, String model,
            GenericResourceHolder gr) {
        if (rscMap.containsKey(model)) {
            rscMap.get(model).add(gr);
        } else {
            List<GenericResourceHolder> rscList = new CopyOnWriteArrayList<GenericResourceHolder>();
            rscList.add(gr);
            rscMap.put(model, rscList);
        }
    }

    private final static String TIME_BASIS_EMPTY_STR = "<no associated time>";

    public boolean isEmpty() {
        return ensembleToolResources.isEmpty();
    }

    public static boolean isTimeEmpty(String timeBasisLegend) {
        return ((timeBasisLegend == null) || (timeBasisLegend.length() == 0) || (timeBasisLegend
                .compareTo(TIME_BASIS_EMPTY_STR) == 0));
    }

    // Currently get the first resource loaded to get time from
    public String getTimeBasisLegendTime() {

        EnsembleResourceManager.getInstance().syncRegisteredResource(theEditor);
        // for now this is the first resource name loaded into this editor
        String timeBasisLegendTime = null;

        if (ensembleToolResources.isEmpty()) {
            timeBasisLegendTime = TIME_BASIS_EMPTY_STR;
        } else {
            GenericResourceHolder g = ensembleToolResources.get(0);
            timeBasisLegendTime = g.getDataTime();
        }

        return timeBasisLegendTime;
    }

    public String getTimeBasisResourceName() {

        EnsembleResourceManager.getInstance().syncRegisteredResource(theEditor);
        // for now this is the first resource name loaded into this editor
        String timeBasisResourceName = null;

        if (ensembleToolResources.isEmpty()) {
            timeBasisResourceName = "";
        } else {
            GenericResourceHolder g = ensembleToolResources.get(0);
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
            Map<String, List<GenericResourceHolder>> resourceMap = getAllRscsAsMap();

            List<GenericResourceHolder> resources = null;
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

    public Map<String, List<GenericResourceHolder>> getCalculationLoadedRscs(
            IDescriptor descriptor, boolean selected) {

        EnsembleResourceManager.getInstance().syncRegisteredResource(theEditor);

        Map<String, List<GenericResourceHolder>> rscMap = new ConcurrentHashMap<String, List<GenericResourceHolder>>();
        for (GenericResourceHolder emr : ensembleToolResources) {
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

}
