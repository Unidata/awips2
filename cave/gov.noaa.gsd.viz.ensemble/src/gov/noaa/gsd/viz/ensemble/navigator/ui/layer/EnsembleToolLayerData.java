package gov.noaa.gsd.viz.ensemble.navigator.ui.layer;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList.AddListener;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;

/**
 * * ResourceData for EnsembleToolLyaer, a resource group to handle the loaded
 * grid resources and Ensemble Tool generated resources .
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 25,  2017  19325        jing    Initial creation
 * 
 * </pre>
 * 
 * 
 * @author jing
 *
 */
public class EnsembleToolLayerData extends AbstractResourceData
        implements IResourceGroup {

    private NavigatorResourceList resourceList;

    private String mapName;

    public EnsembleToolLayerData() {
        this.nameGenerator = new AbstractNameGenerator() {

            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                return mapName;
            }

        };
    }

    @Override
    public EnsembleToolLayer construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {

        EnsembleToolLayer toolLayer = new EnsembleToolLayer(this,
                loadProperties);
        resourceList = new NavigatorResourceList(toolLayer);

        /*
         * Let the newly created tool layer know about when resources are added
         * or removed from the resource list.
         */
        addAddListener((AddListener) toolLayer);
        addRemoveListener((RemoveListener) toolLayer);

        ResourceProperties rp = new ResourceProperties();

        ResourcePair pair = new ResourcePair();
        pair.setResource(toolLayer);
        pair.setProperties(rp);
        descriptor.getResourceList().add(pair);
        return toolLayer;
    }

    /**
     * Call this method to make sure that the listener is notified whenever a
     * resource is added to the resource list.
     * 
     * @param al
     */
    public void addAddListener(AddListener al) {
        resourceList.addPostAddListener(al);
    }

    /**
     * Call this method to make sure that the listener is notified whenever a
     * resource is removed from the resource list.
     * 
     * @param al
     */
    public void addRemoveListener(RemoveListener rl) {
        resourceList.addPostRemoveListener(rl);
    }

    @Override
    public void update(Object updateData) {

    }

    @Override
    synchronized public NavigatorResourceList getResourceList() {
        return resourceList;
    }

    /**
     * @return the mapName
     */
    public String getMapName() {
        return mapName;
    }

    /**
     * @param mapName
     *            the mapName to set
     */
    public void setMapName(String mapName) {
        this.mapName = mapName;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || obj instanceof EnsembleToolLayerData == false) {
            return false;
        }
        EnsembleToolLayerData other = (EnsembleToolLayerData) obj;

        if (this.resourceList != null && other.resourceList == null) {
            return false;
        } else if (this.resourceList == null && other.resourceList != null) {
            return false;
        } else if (this.resourceList != null
                && this.resourceList.equals(other.resourceList) == false) {
            return false;
        }

        if (this.mapName != null && other.mapName == null) {
            return false;
        } else if (this.mapName == null && other.mapName != null) {
            return false;
        } else if (this.mapName != null
                && this.mapName.equals(other.mapName) == false) {
            return false;
        }

        return true;
    }

}
