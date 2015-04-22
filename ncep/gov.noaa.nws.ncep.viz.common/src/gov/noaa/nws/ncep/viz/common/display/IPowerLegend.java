/*
 * IPowerLegend
 * 
 * Date created: 03 AUGUST 2014
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.common.display;

import com.raytheon.uf.viz.core.rsc.IResourceGroup;

/**
 * Interface for group resource, which can be expanded in the legend.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/14        ?           B. Yin  Initial Creation.
 * 
 * </pre>
 * 
 * @author byin
 * 
 */

public interface IPowerLegend extends IResourceGroup {

    public int getFuncKeyNum();

    public boolean isNameExpanded();

    public void setNameExpanded(boolean flag);

    public void setVisibleForAllResources(boolean visible);

}
