package gov.noaa.nws.ncep.viz.rsc.solarimage.display;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.swt.layout.FormData;

import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.d2d.ui.AbstractNonMapDisplay;
import com.raytheon.viz.core.graphing.GraphDescriptor;

@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class DefaultNonMapDisplay extends AbstractNonMapDisplay {

    public DefaultNonMapDisplay() {
        this(new PixelExtent(0, 1000, 0, 1000));
    }

    public DefaultNonMapDisplay(PixelExtent aPixelExtent) {
        super(aPixelExtent, new GraphDescriptor());
        // TODO Auto-generated constructor stub
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.d2d.ui.AbstractNonMapDisplay#getInsetMapLocation()
     */
    @Override
    public FormData getInsetMapLocation() {
        // No Map Inset
        return null;
    }

}
