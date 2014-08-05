package gov.noaa.nws.ncep.viz.resources.manager;

import gov.noaa.nws.ncep.viz.common.display.INatlCntrsDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NCTimeSeriesRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.NcPaneID;
import gov.noaa.nws.ncep.viz.ui.display.NcPaneLayout;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

/**
 * Bundle for Natl Cntrs Resources
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    2014/03/26/   #      qzhou       Modified
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1
 */
@XmlRootElement
@XmlType(name = "GraphRBD")
@XmlAccessorType(XmlAccessType.NONE)
public class GraphRBD extends AbstractRBD<NCTimeSeriesRenderableDisplay> {

    public GraphRBD() {
        super();
    }

    public GraphRBD(NcPaneLayout paneLayout) {
        super(paneLayout);
    }

    @Override
    public boolean addDisplayPane(NCTimeSeriesRenderableDisplay dispPane,
            NcPaneID pid) {
        if (!paneLayout.containsPaneId(pid)) {
            System.out.println("NcMapRBD.getDisplayPane: pane id "
                    + pid.toString() + " is out of range.");
            return false;
        }

        displays[paneLayout.getPaneIndex(pid)] = dispPane;

        // sync the descriptor's auto update with the value of this RBD.
        ((INatlCntrsDescriptor) displays[paneLayout.getPaneIndex(pid)]
                .getDescriptor()).setAutoUpdate(isAutoUpdate());

        return true;
    }

}
