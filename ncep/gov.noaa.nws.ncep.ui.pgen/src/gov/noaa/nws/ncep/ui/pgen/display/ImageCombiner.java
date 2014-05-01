/*
 * ImageCombiner
 * 
 * Date created: 29 JULY 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.display;

import org.eclipse.jface.resource.CompositeImageDescriptor;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.Point;

/**
 * This class combines two SWT images together.  It adds the second image to the right
 * of the first image.
 * @author sgilbert
 *
 */
public class ImageCombiner extends CompositeImageDescriptor {

	ImageData image1, image2;

	/**
	 * Constructor specifying the two images
	 * @param image1
	 * @param image2
	 */
	public ImageCombiner(ImageData image1, ImageData image2) {
		this.image1 = image1;
		this.image2 = image2;
	}

	public void drawComposite() {
		// null implementation
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.jface.resource.CompositeImageDescriptor#drawCompositeImage(int, int)
	 */
	@Override
	protected void drawCompositeImage(int width, int height) {
		//System.out.println("IN drawCompositeImage...");
		/*
		 * add image2 to the right of image1
		 */
		this.drawImage(image1, 0, 0);
		this.drawImage(image2, image1.width, 0);
	}

	/* (non-Javadoc)
	 * Return the size of the new combined image
	 * @see org.eclipse.jface.resource.CompositeImageDescriptor#getSize()
	 */
	@Override
	protected Point getSize() {
		//System.out.println("IN getSIZE...");
		Point pt = new Point(image1.width+image2.width, image1.height);
		return pt;
	}

}
