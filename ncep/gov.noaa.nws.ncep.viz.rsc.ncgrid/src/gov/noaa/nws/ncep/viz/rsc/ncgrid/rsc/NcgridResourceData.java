package gov.noaa.nws.ncep.viz.rsc.ncgrid.rsc;

import gov.noaa.nws.ncep.viz.gempak.util.GempakGrid;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefinition;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Resource data for grids from GribRecords
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb, 2010           		M. Li      Initial creation
 * Nov, 2010				M. Li	   add wind attribute
 * Nov,22 2010  352			X. Guo     Add HILO, HLSYM
 * 11/29/2010				mgamazaychikov	Added dataSource, getAvailableTimes
 * 12/22/1010               G Hull     replace dataSource with pluginName
 * Feb, 2011				           Add eventName to timeline query
 * 02/11/2011               G Hull     eventName now set in bndlTemplate and by RseourceDefnsMngr ;
 *                                     add constructor
 * 09/19/2011				mgamazaychikov	Made changes associated with removal of DatatypeTable class
 * 12/22/2011               G Hull     Updated getGdFile()
 * 12/06/2012   #538        Q. Zhou    Added skip and filter areas and implements. 
 * 03/28/2012               X. Guo     Don't need to convert gdfile toUppercase
 * 08/29/2012   #743        Archana    Added CLRBAR  
 * 09/14/2013   #1036       S. Gurung  Added TEXT  
 * 07/02/2014   ?           B. Yin     Handle grid analysis.
 * 
 * </pre>
 * 
 * @author mli
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "NC-NcgridResourceData")
public class NcgridResourceData extends
        AbstractNatlCntrsRequestableResourceData implements
        INatlCntrsResourceData {

    @XmlElement
    protected String type;

    @XmlElement
    protected String cint;

    @XmlElement
    protected String gdfile;

    @XmlElement
    protected String gvcord;

    @XmlElement
    protected String glevel;

    @XmlElement
    protected String gdpfun;

    @XmlElement
    protected String skip;

    @XmlElement
    protected String filter;

    @XmlElement
    protected String scale = "0";

    @XmlElement
    protected String wind = "18/1/1";

    @XmlElement
    protected String title;

    @XmlElement
    protected String lineAttributes;

    @XmlElement
    protected String colors;

    @XmlElement
    protected String marker;

    @XmlElement
    protected int grdlbl;

    @XmlElement
    protected String fint;

    @XmlElement
    protected String fline;

    @XmlElement
    protected String hilo;

    @XmlElement
    protected String hlsym;

    @XmlElement
    protected String clrbar;

    @XmlElement
    protected String text;

    public NcgridResourceData() {
        super();
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects) {
        return new NcgridResource(this, loadProperties);
    }

    @Override
    public boolean equals(Object obj) {
        if (!super.equals(obj)) {
            return false;
        }

        if (obj instanceof NcgridResourceData == false) {
            return false;
        }

        NcgridResourceData other = (NcgridResourceData) obj;

        if (this.resourceName != null && other.resourceName == null) {
            return false;
        } else if (this.resourceName == null && other.resourceName != null) {
            return false;
        } else if (this.resourceName != null
                && this.resourceName.equals(other.resourceName) == false) {
            return false;
        }

        if (this.type != null && other.type == null) {
            return false;
        } else if (this.type == null && other.type != null) {
            return false;
        } else if (this.type != null && this.type.equals(other.type) == false) {
            return false;
        }

        if (this.cint != null && other.cint == null) {
            return false;
        } else if (this.cint == null && other.cint != null) {
            return false;
        } else if (this.cint != null && this.cint.equals(other.cint) == false) {
            return false;
        }

        if (this.gdfile != null && other.gdfile == null) {
            return false;
        } else if (this.gdfile == null && other.gdfile != null) {
            return false;
        } else if (this.gdfile != null
                && this.gdfile.equals(other.gdfile) == false) {
            return false;
        }

        if (this.gvcord != null && other.gvcord == null) {
            return false;
        } else if (this.gvcord == null && other.gvcord != null) {
            return false;
        } else if (this.gvcord != null
                && this.gvcord.equals(other.gvcord) == false) {
            return false;
        }

        if (this.glevel != null && other.glevel == null) {
            return false;
        } else if (this.glevel == null && other.glevel != null) {
            return false;
        } else if (this.glevel != null
                && this.glevel.equals(other.glevel) == false) {
            return false;
        }

        if (this.gdpfun != null && other.gdpfun == null) {
            return false;
        } else if (this.gdpfun == null && other.gdpfun != null) {
            return false;
        } else if (this.gdpfun != null
                && this.gdpfun.equals(other.gdpfun) == false) {
            return false;
        }

        if (this.skip != null && other.skip == null) {
            return false;
        } else if (this.skip == null && other.skip != null) {
            return false;
        } else if (this.skip != null && this.skip.equals(other.skip) == false) {
            return false;
        }

        if (this.filter != null && other.filter == null) {
            return false;
        } else if (this.filter == null && other.filter != null) {
            return false;
        } else if (this.filter != null
                && this.filter.equals(other.filter) == false) {
            return false;
        }

        if (this.scale != null && other.scale == null) {
            return false;
        } else if (this.scale == null && other.scale != null) {
            return false;
        } else if (this.scale != null
                && this.scale.equals(other.scale) == false) {
            return false;
        }

        if (this.title != null && other.title == null) {
            return false;
        } else if (this.title == null && other.title != null) {
            return false;
        } else if (this.title != null
                && this.title.equals(other.title) == false) {
            return false;
        }

        if (this.lineAttributes != null && other.lineAttributes == null) {
            return false;
        } else if (this.lineAttributes == null && other.lineAttributes != null) {
            return false;
        } else if (this.lineAttributes != null
                && this.lineAttributes.equals(other.lineAttributes) == false) {
            return false;
        }

        if (this.colors != null && other.colors == null) {
            return false;
        } else if (this.colors == null && other.colors != null) {
            return false;
        } else if (this.colors != null
                && this.colors.equals(other.colors) == false) {
            return false;
        }

        if (this.marker != null && other.marker == null) {
            return false;
        } else if (this.marker == null && other.marker != null) {
            return false;
        } else if (this.marker != null
                && this.marker.equals(other.marker) == false) {
            return false;
        }

        if (this.grdlbl != other.grdlbl) {
            return false;
        }

        if (this.fint != null && other.fint == null) {
            return false;
        } else if (this.fint == null && other.fint != null) {
            return false;
        } else if (this.fint != null && this.fint.equals(other.fint) == false) {
            return false;
        }

        if (this.fline != null && other.fline == null) {
            return false;
        } else if (this.fline == null && other.fline != null) {
            return false;
        } else if (this.fline != null
                && this.fline.equals(other.fline) == false) {
            return false;
        }

        if (this.hilo != null && other.hilo == null) {
            return false;
        } else if (this.hilo == null && other.hilo != null) {
            return false;
        } else if (this.hilo != null && this.hilo.equals(other.hilo) == false) {
            return false;
        }

        if (this.hlsym != null && other.hlsym == null) {
            return false;
        } else if (this.hlsym == null && other.hlsym != null) {
            return false;
        } else if (this.hlsym != null
                && this.hlsym.equals(other.hlsym) == false) {
            return false;
        }

        if (this.clrbar != null && other.clrbar == null) {
            return false;
        } else if (this.clrbar == null && other.clrbar != null) {
            return false;
        } else if (this.clrbar != null
                && this.clrbar.equals(other.clrbar) == false) {
            return false;
        }

        if (this.text != null && other.text == null) {
            return false;
        } else if (this.text == null && other.text != null) {
            return false;
        } else if (this.text != null && this.text.equals(other.text) == false) {
            return false;
        }

        return true;
    }

    public String getCint() {
        return cint;
    }

    public void setCint(String cint) {
        this.cint = cint;
    }

    public String getGdfile() {
        return gdfile;
    }

    public void setGdfile(String gdfile) {
        this.gdfile = gdfile;
    }

    public String getGvcord() {
        return gvcord;
    }

    public void setGvcord(String gvcord) {
        this.gvcord = gvcord;
    }

    public String getGlevel() {
        return glevel;
    }

    public void setGlevel(String glevel) {
        this.glevel = glevel;
    }

    public String getGdpfun() {
        return gdpfun;
    }

    public void setGdpfun(String gdpfun) {
        this.gdpfun = gdpfun;
    }

    public String getSkip() {
        return skip;
    }

    public void setSkip(String skip) {
        this.skip = skip;
    }

    public String getFilter() {
        return filter;
    }

    public void setFilter(String filter) {
        this.filter = filter;
    }

    public String getScale() {
        return scale;
    }

    public void setScale(String scale) {
        this.scale = scale;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    /*
     * public DisplayType getVectorType() { if
     * (this.type.toUpperCase().contains("B")) { return DisplayType.BARB; } else
     * if (this.type.toUpperCase().contains("A") ||
     * this.type.toUpperCase().contains("D")) { return DisplayType.ARROW; } else
     * if (this.type.toUpperCase().contains("S")) { return
     * DisplayType.STREAMLINE; } else { return null; } }
     */

    public String getLineAttributes() {
        return lineAttributes;
    }

    public void setLineAttributes(String lineAttributes) {
        this.lineAttributes = lineAttributes;
    }

    public String getColors() {
        return colors;
    }

    public void setColors(String colors) {
        this.colors = colors;
    }

    public String getMarker() {
        return marker;
    }

    public void setMarker(String marker) {
        this.marker = marker;
    }

    public int getGrdlbl() {
        return grdlbl;
    }

    public void setGrdlbl(int grdlbl) {
        this.grdlbl = grdlbl;
    }

    public String getFint() {
        return fint;
    }

    public void setFint(String fint) {
        this.fint = fint;
    }

    public String getFline() {
        return fline;
    }

    public void setFline(String fline) {
        this.fline = fline;
    }

    public String getWind() {
        return wind;
    }

    public void setWind(String wind) {
        this.wind = wind;
    }

    public String getHilo() {
        return hilo;
    }

    public void setHilo(String hilo) {
        this.hilo = hilo;
    }

    public String getHlsym() {
        return hlsym;
    }

    public void setHlsym(String hlsym) {
        this.hlsym = hlsym;
    }

    /**
     * @return the clrbar
     */
    public String getClrbar() {
        return clrbar;
    }

    /**
     * @param clrbar
     *            the clrbar to set
     */
    public void setClrbar(String clrbar) {
        this.clrbar = clrbar;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public String getEventName() {
        if (getMetadataMap().containsKey("info.secondaryId")) {
            String eventName = getMetadataMap().get("info.secondaryId")
                    .getConstraintValue();

            return (eventName.equals("%") ? null : eventName);
        } else {
            return null;
        }
    }

    public String getEnsembelMember() {
        if (getMetadataMap().containsKey("info.ensembleId")) {
            String ensembleMember = getMetadataMap().get("info.ensembleId")
                    .getConstraintValue();

            return (ensembleMember.equals("%") ? null : ensembleMember);
        } else {
            return null;
        }
    }

    // set metadataMap with the modelName constraint and return it
    // (This is overridden by the NcEnsembleResourceData
    //
    // public HashMap<String, RequestConstraint> getMetadataMap() {
    // HashMap<String, RequestConstraint> queryList = super.getMetadataMap();
    //
    // queryList.put("modelInfo.modelName",
    // new RequestConstraint( getGdfile(), ConstraintType.EQUALS ) );
    //
    // return queryList;
    // }

    @Override
    public DataTime[] getAvailableTimes() throws VizException {

        if (getPluginName().equalsIgnoreCase(GempakGrid.gempakPluginName)) {
            try {
                String currentCycle = getResourceName().getCycleTime()
                        .toString();
                String dataLoc = null;
                try {
                    dataLoc = GempakGrid.getGempakGridPath(getGdfile());

                } catch (VizException e) {
                    throw new VizException(e);
                }
                String[] gridAvailableTimes = GempakGrid.getAvailableGridTimes(
                        dataLoc, currentCycle, getGdfile().toLowerCase());
                DataTime[] availableTimes = new DataTime[gridAvailableTimes.length];

                for (int ii = 0; ii < gridAvailableTimes.length; ii++) {
                    availableTimes[ii] = new DataTime(gridAvailableTimes[ii]);
                }
                return availableTimes;
            } catch (Exception e) {
                // TODO Auto-generated catch block
                throw new VizException();
            }
        } else {
            DataTime[] availableTimes = super.getAvailableTimes();
            return availableTimes;
        }
    }

    @Override
    public List<DataTime> getAvailableDataTimes() {
        if (getResourceName().getCycleTime() != null) {
            return super.getAvailableDataTimes();
        } else {
            // For grid analysis
            List<DataTime> availTimesList = null;

            try {
                ResourceDefinition rscDefn = ResourceDefnsMngr.getInstance()
                        .getResourceDefinition(getResourceName());

                if (rscDefn.getInventoryEnabled()
                        && rscDefn.isInventoryInitialized()) {
                    availTimesList = rscDefn.getDataTimes(getResourceName());
                } else {
                    try {
                        DataTime[] availTimes = null;
                        availTimes = getAvailableTimes();
                        if (availTimes == null) {
                            return new ArrayList<DataTime>();
                        }
                        availTimesList = Arrays.asList(availTimes);

                    } catch (VizException e) {
                        System.out.println("Error getting Available Times: "
                                + e.getMessage());
                        return null;
                    }
                }
            } catch (VizException e1) {
                return availTimesList;
            }

            // Sort the time by ref-time then by forecast hours.
            Collections.sort(availTimesList);
            Iterator<DataTime> it = availTimesList.iterator();

            // test dfltFrameTimes ="firstf00-lastf06" All f00 plus last f06;
            // test dfltFrameTimes ="firstf00 - lastf06";
            // test dfltFrameTimes ="allf00";
            // test dfltFrameTimes = null;
            // test dfltFrameTimes = "";
            // dfltFrameTimes ="allf000";

            List<DataTime> availAnlsList = new ArrayList<DataTime>();

            if (dfltFrameTimes == null || dfltFrameTimes.isEmpty()) { // Default
                                                                      // is
                                                                      // "AllF00"
                while (it.hasNext()) {
                    DataTime dt = (DataTime) it.next();
                    if (dt.getFcstTime() == 0) {
                        availAnlsList.add(dt);
                    }
                }
            } else {
                String dft = dfltFrameTimes.toUpperCase();

                if (dft.contains("ALLF")) { // For GDATTIM = "allfxx". Default
                                            // is "ALLF00"
                    int fxx = 0;
                    try {
                        int idx1 = dft.indexOf("ALLF");
                        fxx = Integer.parseInt(dft.substring(idx1 + 4));
                    } catch (NumberFormatException e) {
                        fxx = 0;
                    }
                    fxx *= 3600;
                    while (it.hasNext()) {
                        DataTime dt = (DataTime) it.next();
                        if (dt.getFcstTime() == fxx) {
                            availAnlsList.add(dt);
                        }
                    }
                } else if (dft.contains("FIRSTF") && dft.contains("LASTF")) { // For
                                                                              // GDATTIM
                                                                              // =
                                                                              // "firstfxx - lastfyy".
                    int fxx = 0;
                    int fyy = 0;

                    int idx1 = dft.indexOf("FIRSTF");
                    if (idx1 >= 0) {
                        int idx2 = dft.indexOf('-');
                        if (idx2 > 0 && idx2 > idx1 + 6) {
                            try {
                                fxx = Integer.parseInt(dft.substring(idx1 + 6,
                                        idx2));
                            } catch (NumberFormatException e) {
                                fxx = 0;
                            }
                        }
                    }

                    idx1 = dft.indexOf("LASTF");
                    if (idx1 >= 0) {
                        try {
                            fyy = Integer.parseInt(dft.substring(idx1 + 5));
                        } catch (NumberFormatException e) {
                            fyy = 0;
                        }
                    }

                    DataTime lastItem = null;
                    // find the last item
                    for (int ii = availTimesList.size() - 1; ii >= 0; ii--) {

                        DataTime dt = availTimesList.get(ii);
                        if (dt.getFcstTime() == fyy * 3600) {
                            lastItem = dt;
                            break;
                        }
                    }

                    for (int ii = availTimesList.size() - 1; ii >= 0; ii--) {
                        DataTime dt = availTimesList.get(ii);
                        System.out.println("Time: " + dt.getRefTime()
                                + "FCST: " + dt.getFcstTime() / 3600);
                    }
                    while (it.hasNext()) {
                        DataTime dt = (DataTime) it.next();
                        if (dt == lastItem) {
                            availAnlsList.add(dt);
                            break;
                        } else if (dt.getFcstTime() == fxx) {
                            availAnlsList.add(dt);
                        }
                    }

                } else { // for anything else in GDATTIM
                    while (it.hasNext()) {
                        DataTime dt = (DataTime) it.next();
                        if (dt.getFcstTime() == 0) {
                            availAnlsList.add(dt);
                        }
                    }
                }
            }

            return availAnlsList;
        }
    }

    /**
     * Checks if the GDATTIM is in format "ALLFXX" or "FIRSTFxx-LASTFxx"
     * 
     * @return
     */
    public boolean isGdattimForGridAnalysis() {
        if (dfltFrameTimes != null && !dfltFrameTimes.isEmpty()) {
            return (dfltFrameTimes.toUpperCase().contains("ALLF") || (dfltFrameTimes
                    .toUpperCase().contains("FIRSTF") && dfltFrameTimes
                    .toUpperCase().contains("LASTF")));
        } else {
            return false;
        }
    }
}
