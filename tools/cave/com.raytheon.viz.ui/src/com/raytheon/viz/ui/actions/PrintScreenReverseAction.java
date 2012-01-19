/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.viz.ui.actions;

import java.awt.geom.AffineTransform;
import java.awt.image.AffineTransformOp;
import java.awt.image.BufferedImage;
import java.awt.image.ByteLookupTable;
import java.awt.image.ComponentColorModel;
import java.awt.image.IndexColorModel;
import java.awt.image.LookupOp;
import java.awt.image.LookupTable;
import java.awt.image.WritableRaster;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.graphics.Transform;
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Print the current map
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 26, 2006             chammack    Initial Creation.
 * Aug 08, 2008      #703   randerso    fixed bug, changed to scale to fit 
 *                                      paper and rotate if necessary
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class PrintScreenReverseAction extends AbstractHandler {

    LookupTable lookupTable;

    BufferedImage bi;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        AbstractEditor editor = null;
        IEditorPart part = EditorUtil.getActiveEditor();
        if (part instanceof AbstractEditor) {
            editor = (AbstractEditor) part;
        }
        if (editor == null) {
            return null;
        }
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        // display the printer dialog to get print options
        PrintDialog pd = new PrintDialog(shell);
        PrinterData printerData = pd.open();
        if (printerData == null) {
            return null;
        }

        bi = editor.screenshot();

        Display display = editor.getActiveDisplayPane().getDisplay();
        Printer printer = new Printer(printerData);
        Point screenDPI = display.getDPI();
        Point printerDPI = printer.getDPI();

        // Determine the bounds of the entire area of the printer
        Rectangle printArea = printer.getClientArea();

        float imageWidth = bi.getWidth() / (float) screenDPI.x;
        float imageHeight = bi.getHeight() / (float) screenDPI.y;
        float imageAspect = imageWidth / imageHeight;

        float printerWidth = printArea.width / (float) printerDPI.x;
        float printerHeight = printArea.height / (float) printerDPI.y;
        float printerAspect = printerWidth / printerHeight;

        // rotate image if necessary for best fit
        // NOTE: rotating the image since the SWT Transform appears to have an
        // error for exact 90 degree rotations
        boolean rotated = false;
        if ((imageAspect - 1) * (printerAspect - 1) < 0) {
            AffineTransform transform = AffineTransform
                    .getQuadrantRotateInstance(1);
            transform.concatenate(AffineTransform.getTranslateInstance(0.0,
                    -bi.getHeight()));
            AffineTransformOp transformOp = new AffineTransformOp(transform,
                    AffineTransformOp.TYPE_NEAREST_NEIGHBOR);
            bi = transformOp.filter(
                    bi,
                    new BufferedImage(bi.getHeight(), bi.getWidth(), bi
                            .getType()));
            rotated = true;
        }

        reverseLUT();
        applyFilter();
        ImageData imageData = convertToSWT(bi);
        Image image = new Image(printer, imageData);

        // scale to adjust for difference in pixel aspect
        float aspectScale = ((float) printerDPI.y / printerDPI.x)
                / ((float) screenDPI.y / screenDPI.x);

        // now scale to fill the page
        Rectangle imageBounds = image.getBounds();
        float hScale = (float) printArea.width / imageBounds.width;
        float vScale = printArea.height / (imageBounds.height * aspectScale);
        float scaleX = Math.min(hScale, vScale);
        float scaleY = scaleX * aspectScale;

        // if rotated shift image to right edge of page
        Rectangle trim = printer.computeTrim(0, 0, 0, 0);
        Point offset = new Point(-trim.x, -trim.y);
        if (rotated) {
            offset.x += printArea.width - (imageBounds.width) * scaleX;
        }

        if (printer.startJob("CAVE")) {
            if (printer.startPage()) {
                GC gc = new GC(printer);
                Transform transform = new Transform(gc.getDevice());
                transform.translate(offset.x, offset.y);
                transform.scale(scaleX, scaleY);
                gc.setTransform(transform);

                gc.drawImage(image, 0, 0);

                transform.dispose();
                gc.dispose();
                printer.endPage();
                printer.endJob();
            }
        }

        image.dispose();
        printer.dispose();

        return null;

    }

    static ImageData convertToSWT(BufferedImage bufferedImage) {
        if (bufferedImage.getColorModel() instanceof ComponentColorModel) {
            ComponentColorModel colorModel = (ComponentColorModel) bufferedImage
                    .getColorModel();

            PaletteData palette = new PaletteData(0x0000ff, 0x00ff00, 0xff0000);
            ImageData data = new ImageData(bufferedImage.getWidth(),
                    bufferedImage.getHeight(), colorModel.getPixelSize(),
                    palette);
            WritableRaster raster = bufferedImage.getRaster();
            int[] pixelArray = new int[3];
            for (int y = 0; y < data.height; y++) {
                for (int x = 0; x < data.width; x++) {
                    raster.getPixel(x, y, pixelArray);
                    int pixel = palette.getPixel(new RGB(pixelArray[0],
                            pixelArray[1], pixelArray[2]));
                    data.setPixel(x, y, pixel);
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

    /*
     * Create reverse lookup table for image
     */
    private void reverseLUT() {
        byte reverse[] = new byte[256];
        for (int i = 0; i < 256; i++) {
            reverse[i] = (byte) (255 - i);
        }
        lookupTable = new ByteLookupTable(0, reverse);
    }

    /*
     * Apply lookup table filter to image
     */
    private void applyFilter() {
        LookupOp lop = new LookupOp(lookupTable, null);
        lop.filter(bi, bi);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#dispose()
     */
    @Override
    public void dispose() {

    }

}
