package gov.noaa.gsd.viz.ensemble.display.common;

import gov.noaa.gsd.viz.ensemble.display.rsc.GeneratedEnsembleGridResource;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.grid.rsc.AbstractGridResource;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.viz.grid.rsc.general.D2DGridResource;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Utilities for the ensemble display.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 9, 2014    5056        jing     Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @version 1.0
 */
public class Utilities {

    private Utilities() {
        super();
    }

    /*
     * All ensemble resources of all ensemble model products.
     */
    public static List<D2DGridResource> getResourcesEnsembleModels() {
        ArrayList<D2DGridResource> ensembleResources = new ArrayList<D2DGridResource>();

        // check all resources
        IMapDescriptor desc = (IMapDescriptor) (getEditor()
                .getActiveDisplayPane().getDescriptor());
        ResourceList rscList = desc.getResourceList();
        for (ResourcePair rp : rscList) {

            AbstractVizResource<?, ?> rsc = rp.getResource();
            if (!(rsc instanceof AbstractGridResource))
                continue;
            if (rsc instanceof GeneratedEnsembleGridResource)
                continue;
            if (!isResourceEnsemble((D2DGridResource) rsc))
                continue;

            D2DGridResource ensembleResource = ((D2DGridResource) rsc);
            ensembleResources.add(ensembleResource);
        }

        return ensembleResources;
    }

    /*
     * All ensemble resources of one ensemble model products.
     */
    public static List<D2DGridResource> getResourcesEnsembleModel(String model) {
        ArrayList<D2DGridResource> ensembleResources = new ArrayList<D2DGridResource>();

        // check all resources
        IMapDescriptor desc = (IMapDescriptor) (getEditor()
                .getActiveDisplayPane().getDescriptor());
        ResourceList rscList = desc.getResourceList();
        for (ResourcePair rp : rscList) {

            AbstractVizResource<?, ?> rsc = rp.getResource();
            if (!(rsc instanceof AbstractGridResource))
                continue;
            if (rsc instanceof GeneratedEnsembleGridResource)
                continue;
            if (!isResourceEnsemble((D2DGridResource) rsc))
                continue;
            if (!((D2DGridResource) rsc).getLegendParameters().model
                    .equals(model))
                continue;
            D2DGridResource ensembleResource = ((D2DGridResource) rsc);
            ensembleResources.add(ensembleResource);

        }
        return ensembleResources;
    }

    /*
     * All resources of different models same time.
     */

    // get density base on the number of a ensemble products
    public static double getEnsembleDensityRsc(AbstractGridResource<?> rsc) {
        if (!isResourceEnsemble((D2DGridResource) rsc)) {
            return ((D2DGridResource) rsc).getCapability(
                    DensityCapability.class).getDensity();
        }
        // this rsc belong to which products
        String model = ((D2DGridResource) rsc).getLegendParameters().model;

        // How many member of the model
        List<String> members = Utilities.getEnsembleMembers(model);
        if (members == null)
            return ((D2DGridResource) rsc).getCapability(
                    DensityCapability.class).getDensity();

        // Which density is better, temp. table to assign densities
        double density = .33;
        if (members.size() < 10) {
            density = 2.0;
        } else if (members.size() < 20) {
            density = 1.5;
        } else if (members.size() < 40) {
            density = 1.0;
        } else if (members.size() < 60) {
            density = .75;
        } else if (members.size() < 80) {
            density = .5;
        }

        return density;

    }

    // Get loaded ensemble models. This function will be used by other without
    // have a object
    public static List<String> getEnsembleModels() {
        ArrayList<String> models = new ArrayList<String>();

        // check all resources
        IDescriptor desc = (getEditor().getActiveDisplayPane().getDescriptor());
        ResourceList rscList = desc.getResourceList();
        for (ResourcePair rp : rscList) {

            AbstractVizResource<?, ?> rsc = rp.getResource();
            if (!(rsc instanceof AbstractGridResource))
                continue;
            if (rsc instanceof GeneratedEnsembleGridResource)
                continue;
            if (!isResourceEnsemble((D2DGridResource) rsc))
                continue;

            String model = ((D2DGridResource) rsc).getLegendParameters().model;
            if (models.contains(model))
                continue;
            models.add(model);

        }
        return models;
    }

    // Get ensemble members of a model. This function will be used by other
    // without have a object
    public static List<String> getEnsembleMembers(String model) {
        ArrayList<String> members = new ArrayList<String>();

        // check all resources
        IMapDescriptor desc = (IMapDescriptor) (getEditor()
                .getActiveDisplayPane().getDescriptor());
        ResourceList rscList = desc.getResourceList();
        for (ResourcePair rp : rscList) {

            AbstractVizResource<?, ?> rsc = rp.getResource();
            if (!(rsc instanceof AbstractGridResource))
                continue;
            if (rsc instanceof GeneratedEnsembleGridResource)
                continue;
            if (!isResourceEnsemble((D2DGridResource) rsc))
                continue;
            if (!((D2DGridResource) rsc).getLegendParameters().model
                    .equals(model))
                continue;

            String member = ((D2DGridResource) rsc).getLegendParameters().ensembleId;
            if (members.contains(member))
                continue;
            members.add(member);

        }
        return members;
    }

    // If it is a contour ensemble resource
    public static boolean isResourceEnsemble(D2DGridResource rsc) {
        // What display type?
        // AbstractGriddedDisplay gridDisplay=
        // (AbstractGriddedDisplay) rsc.getDescriptor().getRenderableDisplay();
        if (!rsc.getDisplayType().equals(DisplayType.CONTOUR))
            return false;

        // Is it ensemble grid? Just use the getLegendParameters().ensembleId
        // now
        // Should check on grid data records later
        if (rsc.getLegendParameters().ensembleId == null
                || rsc.getLegendParameters().ensembleId.isEmpty())
            return false;

        return true;
    }

    /**
     * Get the current Editor.
     * 
     * @return
     */
    private static AbstractEditor getEditor() {
        AbstractEditor editor = (AbstractEditor) EditorUtil.getActiveEditor();
        return editor;
    }
}
