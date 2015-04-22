/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.viz.rtkp.rsc;

import gov.noaa.nws.ncep.viz.common.ui.Markers.MarkerState;
import gov.noaa.nws.ncep.viz.common.ui.Markers.MarkerTextSize;
import gov.noaa.nws.ncep.viz.common.ui.Markers.MarkerType;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Resource data for RTKP world wide mag activity map.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#    Engineer    Description
 * ------------  ---------- ----------- --------------------------
 * May 5, 2014   1122       sgurung     Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class GeoMagWorldActivityResourceData extends AbstractResourceData {

    private RGB color = new RGB(0, 255, 0);

    private MarkerState markerState = MarkerState.MARKER_PLUS_TEXT;

    private MarkerType markerType = MarkerType.BOX;

    private RGB otherStncolor = new RGB(157, 114, 50);

    private MarkerState otherStnMarkerState = MarkerState.MARKER_PLUS_TEXT;

    private MarkerType otherStnMarkerType = MarkerType.FILLED_TRIANGLE;

    private Float markerSize = 1.3f;

    private Integer markerWidth = 2;

    private MarkerTextSize markerTextSize = MarkerTextSize.MEDIUM;

    private String mapName = "GEOMAGWORLDACTIVITY";

    public GeoMagWorldActivityResourceData() {
        super();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon
     * .uf.viz.core.comm.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public GeoMagWorldActivityResource construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        return new GeoMagWorldActivityResource(this, loadProperties);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#update(java.lang.Object
     * )
     */
    @Override
    public void update(Object updateData) {
        // do nothing
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null || !(obj instanceof GeoMagWorldActivityResourceData))
            return false;
        GeoMagWorldActivityResourceData rdata = (GeoMagWorldActivityResourceData) obj;
        if (this.markerState.equals(rdata.getMarkerState())
                && this.markerType.equals(rdata.getMarkerType())
                && this.markerSize.equals(rdata.getMarkerSize())
                && this.markerWidth.equals(rdata.getMarkerWidth())
                && this.markerTextSize.equals(rdata.getMarkerTextSize())
                && this.markerWidth.equals(rdata.getMarkerWidth())
                && this.otherStnMarkerState.equals(rdata
                        .getOtherStnMarkerState())
                && this.otherStnMarkerType
                        .equals(rdata.getOtherStnMarkerType())
                && this.otherStncolor.equals(rdata.getOtherStncolor())
                && this.color.equals(rdata.getColor()))
            return true;
        return false;
    }

    public RGB getColor() {
        return color;
    }

    public void setColor(RGB kStnColor) {
        this.color = kStnColor;
    }

    public MarkerState getMarkerState() {
        return markerState;
    }

    public void setMarkerState(MarkerState markerState) {
        this.markerState = markerState;
    }

    public MarkerType getMarkerType() {
        return markerType;
    }

    public void setMarkerType(MarkerType markerType) {
        this.markerType = markerType;
    }

    public Float getMarkerSize() {
        return markerSize;
    }

    public void setMarkerSize(Float markerSize) {
        this.markerSize = markerSize;
    }

    public Integer getMarkerWidth() {
        return markerWidth;
    }

    public void setMarkerWidth(Integer markerWidth) {
        this.markerWidth = markerWidth;
    }

    public MarkerTextSize getMarkerTextSize() {
        return markerTextSize;
    }

    public void setMarkerTextSize(MarkerTextSize markerTextSize) {
        this.markerTextSize = markerTextSize;
    }

    public String getMapName() {
        return mapName;
    }

    public void setMapName(String mapName) {
        this.mapName = mapName;
    }

    public MarkerState getkStnMarkerState() {
        return markerState;
    }

    public void setkStnMarkerState(MarkerState kStnMarkerState) {
        this.markerState = kStnMarkerState;
    }

    public MarkerType getkStnMarkerType() {
        return markerType;
    }

    public void setkStnMarkerType(MarkerType kStnMarkerType) {
        this.markerType = kStnMarkerType;
    }

    public RGB getOtherStncolor() {
        return otherStncolor;
    }

    public void setOtherStncolor(RGB otherStncolor) {
        this.otherStncolor = otherStncolor;
    }

    public MarkerState getOtherStnMarkerState() {
        return otherStnMarkerState;
    }

    public void setOtherStnMarkerState(MarkerState otherStnMarkerState) {
        this.otherStnMarkerState = otherStnMarkerState;
    }

    public MarkerType getOtherStnMarkerType() {
        return otherStnMarkerType;
    }

    public void setOtherStnMarkerType(MarkerType otherStnMarkerType) {
        this.otherStnMarkerType = otherStnMarkerType;
    }

}
