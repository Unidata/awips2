/*
 * SymbolImageUtil
 * 
 * Date created: 29 JULY 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.display;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.Transparency;
import java.awt.geom.Path2D;
import java.awt.image.BufferedImage;
import java.awt.image.ComponentColorModel;
import java.awt.image.IndexColorModel;
import java.awt.image.WritableRaster;

import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.viz.ui.color.BackgroundColor;
import com.raytheon.viz.ui.color.IBackgroundColorChangedListener.BGColorMode;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * This class contains static utility methods for creating symbol/marker images and
 * icons.
 * @author sgilbert
 *
 */
public class SymbolImageUtil {
	
    public static final int INITIAL_IMAGE_SIZE = 12;

    /**
     * creates an SWT image from a symbol Identifier.  If the symbol Identifier
     * contains two symbol ids separated by a vertical bar "|", then the image
     * returned will contain both symbols.
     * @param device
     * @param symbolId
     * @return
     */
	public static Image createIcon(Device device, String symbolId) {
	
		Image icon = null;
		
		String[] ids = symbolId.split("\\|");
		
		if ( ids.length == 1 ) {
			/*
			 * create an image of a single symbol
			 */
			BufferedImage img = createBufferedImage(symbolId);
			if ( img != null ) {
				ImageData idata = convertToSWT(img);
				icon = new Image(device,idata);
			}
		} 
		else {
			/*
			 * create two images and combine them together
			 */
			BufferedImage img1 = createBufferedImage(ids[0]);
			BufferedImage img2 = createBufferedImage(ids[1]);
			if ( img1 != null && img2 != null ){
				ImageData idata1 = convertToSWT(img1);
				ImageData idata2 = convertToSWT(img2);
				ImageCombiner cid = new ImageCombiner(idata1,idata2);
				icon = new Image(device, cid.getImageData() );
			}
			
		}
		
		return icon;
		
	}
	
	/**
	 * create an AWT BufferedImage for a given symbol Identifier
	 * @param symbolId
	 * @return
	 */
	public static BufferedImage createBufferedImage(String symbolId) {
		return createBufferedImage(symbolId, 2.0, 1.0f, false, new Color(0,0,0));
	}

	/**
	 * create an AWT BufferedIMage for a given symbol Identifier
	 * @param symbolId unique identifer for the symbol
	 * @param scale scale factor to use when creating the symbol
	 * @param strokeWidth width of the lines used to create the symbol
	 * @param backgroundMask boolean indicating whether to create a background mask
	 * @param symColor color of symbol
	 * @return
	 */
	public static BufferedImage createBufferedImage(String symbolId, double scale, float strokeWidth,
			                                        boolean backgroundMask, Color symColor ) {
		
        /*
         * Find Symbol Pattern associated with this element
         */
        SymbolPattern pattern = null;
        SymbolPatternManager spl = SymbolPatternManager.getInstance();
        try {
                pattern = spl.getSymbolPattern(symbolId);
        }
        catch ( SymbolPatternException spe) {
                System.out.println(spe.getMessage());
                return null;
        }

        /*
         * Calculate raster Image size and create an initial buffered image
         */
        double sfactor = scale;
        int imageSize = INITIAL_IMAGE_SIZE * (int)Math.ceil(sfactor);
        double center = (double)imageSize * 0.5;
        BufferedImage image = new BufferedImage(imageSize,imageSize,BufferedImage.TYPE_4BYTE_ABGR);
        
        /*
         * Set graphics drawing options
         */
        Graphics2D g2d = image.createGraphics();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
        //float strokeWidth = 1.0f;

        /*
         * Draw background Mask, if needed
         */
        if ( backgroundMask ) {
            //  Set color and stroke width used for background mask
			RGB bg = BackgroundColor.getActivePerspectiveInstance().getColor(BGColorMode.EDITOR);
			Color bgColor = new Color(bg.red, bg.green, bg.blue);
            g2d.setColor(bgColor);
            g2d.setStroke(new BasicStroke(strokeWidth+5.0f,BasicStroke.CAP_ROUND,BasicStroke.JOIN_ROUND));
        	Path2D.Double mask = new Path2D.Double();
        	for ( SymbolPart spart : pattern.getParts() ) {
        		Coordinate[] coords = spart.getPath();
        		mask.reset();
        		mask.moveTo(center+sfactor*coords[0].x, center-sfactor*coords[0].y);
        		for (int i=1; i<coords.length; i++ ) {
        			mask.lineTo(center+sfactor*coords[i].x, center-sfactor*coords[i].y);
        		}
       			g2d.draw(mask);
        	}
        }

        //  Set color and stroke width used for symbol
        g2d.setColor(symColor);         //  for g2d.draw
        g2d.setPaint(symColor);         //  for g2d.fill
        g2d.setStroke(new BasicStroke(strokeWidth,BasicStroke.CAP_ROUND,BasicStroke.JOIN_ROUND));

        /*
         * draw raster image of symbol from pattern
         */
        Path2D.Double path = new Path2D.Double();
        for ( SymbolPart spart : pattern.getParts() ) {
        	Coordinate[] coords = spart.getPath();
        	path.reset();
        	path.moveTo(center+sfactor*coords[0].x, center-sfactor*coords[0].y);
        	for (int i=1; i<coords.length; i++ ) {
        		path.lineTo(center+sfactor*coords[i].x, center-sfactor*coords[i].y);
        	}
        	if ( spart.isFilled() ) {
        		//path.closePath();
        		g2d.fill(path);
        	}
        	else {
        		g2d.draw(path);
        	}
        }

        //   Clean up
        image.flush();
        g2d.dispose();

        return image;
	}
	
	/*
	 * The following method was "stolen" from com.raytheon.viz.ui.actions.PrintScreenAction,
	 * since it was not declared public.
	 */
    public static ImageData convertToSWT(BufferedImage bufferedImage) {
        if (bufferedImage.getColorModel() instanceof ComponentColorModel) {
            ComponentColorModel colorModel = (ComponentColorModel) bufferedImage
                    .getColorModel();

            PaletteData palette = new PaletteData(0x0000ff, 0x00ff00, 0xff0000);
            ImageData data = new ImageData(bufferedImage.getWidth(),
                    bufferedImage.getHeight(), colorModel.getPixelSize(),
                    palette);
            if ( colorModel.getTransparency() == Transparency.OPAQUE ) data.transparentPixel = 255;
            WritableRaster raster = bufferedImage.getRaster();
            int[] pixelArray = new int[4];
            for (int y = 0; y < data.height; y++) {
                for (int x = 0; x < data.width; x++) {
                    raster.getPixel(x, y, pixelArray);
                    int pixel = palette.getPixel(new RGB(pixelArray[0],
                            pixelArray[1], pixelArray[2]));
                    data.setPixel(x, y, pixel);
                    if ( colorModel.getTransparency() == Transparency.TRANSLUCENT ) data.setAlpha(x, y, pixelArray[3]);
                }
            }
            return data;
        } else if (bufferedImage.getColorModel() instanceof IndexColorModel) {
            IndexColorModel colorModel = (IndexColorModel) bufferedImage
                    .getColorModel();
            int size = colorModel.getMapSize();
            byte[] reds = new byte[size];
            byte[] greens = new byte[size];
            byte[] blues = new byte[size];
            colorModel.getReds(reds);
            colorModel.getGreens(greens);
            colorModel.getBlues(blues);
            RGB[] rgbs = new RGB[size];
            for (int i = 0; i < rgbs.length; i++) {
                rgbs[i] = new RGB(reds[i] & 0xFF, greens[i] & 0xFF,
                        blues[i] & 0xFF);
            }
            PaletteData palette = new PaletteData(rgbs);
            ImageData data = new ImageData(bufferedImage.getWidth(),
                    bufferedImage.getHeight(), colorModel.getPixelSize(),
                    palette);
            data.transparentPixel = colorModel.getTransparentPixel();
            WritableRaster raster = bufferedImage.getRaster();
            int[] pixelArray = new int[1];
            for (int y = 0; y < data.height; y++) {
                for (int x = 0; x < data.width; x++) {
                    raster.getPixel(x, y, pixelArray);
                    data.setPixel(x, y, pixelArray[0]);
                }
            }
            return data;
        }
        return null;
    }

	
}
