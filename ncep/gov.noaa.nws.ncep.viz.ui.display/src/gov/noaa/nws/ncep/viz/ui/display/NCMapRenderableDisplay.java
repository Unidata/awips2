package gov.noaa.nws.ncep.viz.ui.display;

import gov.noaa.nws.ncep.viz.common.EditorManager;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.maps.display.MapRenderableDisplay;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.ResourceList;

/**
 * MapRenderableDisplay for NatlCntrs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/19/09                ghull        Initial creation
 * 01/28/10                ghull        Add predefinedAreaName
 * 04/01/10      238,239   archana      Altered the overloaded 
 *                                      constructor to accept the 
 *                                      NCMapDescriptor as its input
 *                                      parameter.  
 *  02/10/2011              Chin Chen   handle multiple editor copies dispose issue    
 *  03/07/2011   migration  ghull       call customizeResourceList
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "NC-MapRenderableDisplay")
@XmlRootElement
public class NCMapRenderableDisplay extends MapRenderableDisplay implements
        ISerializableObject {

    public static final GenericResourceData legendRscData = new GenericResourceData(
            NCLegendResource.class);

    public static final GenericResourceData selectedRscData = new GenericResourceData(
            NcSelectedPaneResource.class);

    @XmlElement
    private String displayName;

    @XmlElement
    private String predefinedAreaName;

    public NCMapRenderableDisplay() {
        super();
    }

    private int editorNum = 0;

    public int getEditorNum() {
        return editorNum;
    }

    public void setEditorNum(int editorNum) {
        this.editorNum = editorNum;
        System.out.println("NCMapRenderableDisplay setEditorNnum " + editorNum);

    }

    public NCMapRenderableDisplay(NCMapDescriptor desc) {
        super(desc);
        this.setDisplayName("myName = " + this.toString());
        // System.out.println("NCMapRenderableDisplay created "+
        // this.getDisplayName());
    }

    @Override
    public void dispose() {

        int editorInstanceNum = EditorManager.getNumOfEditorInstance(editorNum);
        // System.out.println("NCMapRenderableDisplay "+
        // this.getDisplayName()+" disposed  editor num " + editorNum
        // +" instance "+ editorInstanceNum);
        if (this.descriptor != null && editorInstanceNum <= 1) {
            descriptor.getResourceList().clear();
            this.descriptor.getResourceList().removePostAddListener(
                    this.listener);
            this.descriptor.getResourceList().removePostRemoveListener(
                    this.listener);
        }
    }

    // This string must match the type given in the
    // com.raytheon.uf.viz.core.graphicsTarget extension point
    // @Override
    // public String getDisplayType() {
    // return "NC-2D";
    // }

    public double[] getMapCenter() {
        return this.mapCenter;
    }

    public double getZoomLevel() {
        return zoomLevel;
    }

    public String getDisplayName() {
        return displayName;
    }

    public void setDisplayName(String n) {
        displayName = n;
    }

    public String getPredefinedAreaName() {
        return predefinedAreaName;
    }

    public NCMapDescriptor getDescriptor() {
        if (super.getDescriptor() instanceof NCMapDescriptor) {
            return (NCMapDescriptor) super.getDescriptor();
        }
        return null;
    }

    public void setPredefinedAreaName(String predefinedAreaName) {
        this.predefinedAreaName = predefinedAreaName;
    }

    @Override
    protected void customizeResourceList(ResourceList resourceList) {
        // resourceList. // check if already in the list???
        resourceList.add(ResourcePair
                .constructSystemResourcePair(legendRscData));
        resourceList.add(ResourcePair
                .constructSystemResourcePair(selectedRscData));
    }
}
