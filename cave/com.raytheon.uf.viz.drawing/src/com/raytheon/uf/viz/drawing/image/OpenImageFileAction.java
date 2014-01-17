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

package com.raytheon.uf.viz.drawing.image;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;

import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory.ResourceOrder;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.core.ColorUtil;
import com.raytheon.viz.ui.EditorUtil;

/**
 * 
 * Action for loading an image onto the screen.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 15, 2014  2313     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class OpenImageFileAction extends AbstractHandler {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(OpenImageFileAction.class);

    private static final String BACKGROUND = "Background";

    @Override
    public Object execute(ExecutionEvent event) {
        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container == null) {
            return null;
        }
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        FileDialog fd = new FileDialog(shell, SWT.OPEN);
        fd.setText("Import Image");
        String[] suffixes = ImageIO.getReaderFileSuffixes();
        StringBuilder filter = new StringBuilder(suffixes.length * 7);
        for (String suffix : suffixes) {
            if (!suffix.isEmpty()) {
                if (filter.length() != 0) {
                    filter.append(';');
                }
                filter.append("*.").append(suffix);
            }
        }
        fd.setFilterExtensions(new String[] { filter.toString() });
        fd.setFilterNames(new String[] { "All Images" });

        if (fd.open() == null) {
            return null;
        }

        String fileName = fd.getFilterPath() + File.separator
                + fd.getFileName();

        IRenderableDisplay display = container.getActiveDisplayPane()
                .getRenderableDisplay();
        IDescriptor desc = display.getDescriptor();
        ImageFileResourceData data = new ImageFileResourceData(fileName);
        LoadProperties lProps = new LoadProperties();
        /* Get a new resource color */
        ColorableCapability cCap = lProps.getCapabilities().getCapability(data,
                ColorableCapability.class);
        RGB color = ColorUtil.getNewColor(desc);
        cCap.setColor(color);
        ResourceProperties rProps = new ResourceProperties();
        String importMode = event.getParameter("imageImportMode");
        if (BACKGROUND.equalsIgnoreCase(importMode)) {
            /* Render as background */
            rProps.setRenderingOrder(ResourceOrder.LOWEST.value);
            /* Fill the screen */
            data.setNorth(0);
            data.setWest(0);
            data.setSouth(100);
            data.setEast(100);
        } else {
            /* Render as foreground */
            rProps.setRenderingOrder(ResourceOrder.HIGHEST.value);
            /* Size the same as the image */
            Rectangle canvasBounds = display.getBounds();
            Rectangle imageBounds;
            try {
                imageBounds = getImageBounds(fileName);
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, "Error Loading Image.",
                        e);
                return null;
            }
            data.setNorth(0);
            data.setWest(0);
            data.setEast(imageBounds.width * 100.0 / canvasBounds.width);
            data.setSouth(imageBounds.height * 100.0 / canvasBounds.height);
        }
        try {
            ImageFileResource rsc = data.construct(lProps, desc);
            desc.getResourceList().add(rsc, rProps);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Error Loading Image.", e);
        }
        return null;
    }

    protected static Rectangle getImageBounds(String fileName)
            throws IOException {
        Rectangle imageBounds = null;
        ImageInputStream in = ImageIO
                .createImageInputStream(new File(fileName));
        ImageReader reader = null;
        try {
            Iterator<ImageReader> readers = ImageIO.getImageReaders(in);
            if (readers.hasNext()) {
                reader = readers.next();
                reader.setInput(in);
                imageBounds = new Rectangle(0, 0, reader.getWidth(0),
                        reader.getHeight(0));
            }
        } finally {
            in.close();
            if (reader != null) {
                reader.dispose();
            }
        }
        return imageBounds;
    }
}
