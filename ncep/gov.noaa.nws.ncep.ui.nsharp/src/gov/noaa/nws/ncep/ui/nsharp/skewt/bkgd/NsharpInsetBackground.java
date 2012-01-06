package gov.noaa.nws.ncep.ui.nsharp.skewt.bkgd;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.skewt.bkgd.NsharpInsetBackground
 * 
 * This java class performs the NSHARP NsharpInsetBackground functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/23/2010	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
import org.eclipse.swt.graphics.Rectangle;
import com.raytheon.viz.core.graphing.WGraphics;

public class NsharpInsetBackground extends NsharpAbstractBackground {

	public NsharpInsetBackground(Rectangle rect) {
		super(); 
		this.rectangle = rect;
		world = new WGraphics(this.rectangle);

        world.setWorldCoordinates(rectangle.x,rectangle.y,
        		rectangle.x + rectangle.width, rectangle.y+rectangle.height);

        
	}
	@Override
	protected WGraphics computeWorld() {
        return world;
	}


}
