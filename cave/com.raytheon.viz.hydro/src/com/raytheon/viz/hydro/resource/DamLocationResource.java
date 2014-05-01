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
package com.raytheon.viz.hydro.resource;

import java.awt.image.RenderedImage;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.app.launcher.handlers.AppLauncherHandler;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.hydro.gagedisplay.HydroImageMaker;
import com.raytheon.viz.hydro.gagedisplay.HydroImageMaker.ImageSize;
import com.raytheon.viz.hydro.pointdatacontrol.PointDataControlManager;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.data.DamMaster;
import com.raytheon.viz.pointdata.drawables.IPointImageExtension;
import com.raytheon.viz.pointdata.drawables.IPointImageExtension.PointImage;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.index.strtree.STRtree;

/**
 * Resource to display the DamCrest icons.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2009            mpduff     Initial creation
 * Sep 23, 2009 3069       mpduff      Changed the parent class to HydroPointResource.
 * Jan 22, 2013 15553      wkwock      Correct DamCrest selection algorithm
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DamLocationResource extends
        AbstractVizResource<DamLocationResourceData, IDescriptor> implements
        IContextMenuContributor {

    private static final RGB LABEL_COLOR = RGBColors.getRGBColor("white");

    private PointDataControlManager pdcManager = PointDataControlManager
            .getInstance();

    private List<DamMaster> dams = new ArrayList<DamMaster>();

    private IImage damIcon;

    private IFont font;

    private int fontSize;

    private STRtree damStrTree = new STRtree();

    private double scaleWidthValue = 0.0;

    private double scaleHeightValue = 0.0;

    public DamLocationResource(DamLocationResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
    @Override
    public String getName() {
        return resourceData.getName();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        damIcon = target.initializeRaster(new IRenderedImageCallback() {
            @Override
            public RenderedImage getImage() throws VizException {
                return HydroImageMaker.getDamIcon();
            }
        });
        fontSize = 10;
        font = target.initializeFont("Dialog", fontSize, null);
        font.setSmoothing(false);

        populateDamList();
    }

    private void populateDamList() {
        HydroDisplayManager displayManager = HydroDisplayManager.getInstance();

        List<DamMaster> damList = displayManager.getDamList();
        if (damList == null) {
            damList = resourceData.getDamList();
        }
        if (damList != null) {
            displayManager.setDamList(damList);
            pdcManager.setDamLocationResource(this);
            for (DamMaster dm : damList) {
                addPoint(dm);
            }
        }
    }

    /**
     * Add a Dam site to the map.
     * 
     * @param master
     *            The DamMaster object
     */
    public void addPoint(DamMaster master) {
        if (dams.contains(master) == false) {
            dams.add(master);
            /* Create a small envelope around the point */
            Coordinate p1 = new Coordinate(master.getLongitudeDam() + .03,
                    master.getLatitudeDam() + .03);
            Coordinate p2 = new Coordinate(master.getLongitudeDam() - .03,
                    master.getLatitudeDam() - .03);
            Envelope env = new Envelope(p1, p2);
            damStrTree.insert(env, master);
        }
    }

    /**
     * Paint method called to display this resource.
     * 
     * @param target
     *            The IGraphicsTarget
     * @param paintProps
     *            The Paint Properties
     * @throws VizException
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        HydroDisplayManager manager = HydroDisplayManager.getInstance();

        // Check the font size
        font.setMagnification((manager.getFontSize() / (float) fontSize), true);

        populateDamList();

        List<PointImage> images = new ArrayList<PointImage>(dams.size());
        List<DrawableString> strings = new ArrayList<DrawableString>(
                dams.size() * 2);
        IExtent extent = paintProps.getView().getExtent();
        for (DamMaster dam : dams) {
            double[] pixel = descriptor.worldToPixel(new double[] {
                    dam.getLongitudeDam(), dam.getLatitudeDam() });
            if (pixel != null && extent.contains(pixel)) {
                setScaleWidth(paintProps);
                setScaleHeight(paintProps);

                /* Draw the icons */
                if (manager.isDisplayDamIcon()) {
                    PointImage image = new PointImage(damIcon, pixel[0],
                            pixel[1]);
                    image.setSiteId(dam.getDamName());
                    images.add(image);
                }

                /* Draw the labels */
                if (manager.isDisplayDamId()) {
                    Coordinate idCoor = new Coordinate(pixel[0]
                            - getScaleWidth(), pixel[1] - getScaleHeight());

                    DrawableString ds = new DrawableString(dam.getNidid(),
                            LABEL_COLOR);
                    ds.setCoordinates(idCoor.x, idCoor.y);
                    ds.font = font;
                    strings.add(ds);
                }
                if (manager.isDisplayDamName()) {
                    Coordinate nameCoor = new Coordinate(pixel[0]
                            - getScaleWidth(), pixel[1] + getScaleHeight());

                    DrawableString ds = new DrawableString(dam.getDamName(),
                            LABEL_COLOR);
                    ds.setCoordinates(nameCoor.x, nameCoor.y);
                    ds.font = font;
                    strings.add(ds);
                }
            }
        }
        target.getExtension(IPointImageExtension.class).drawPointImages(
                paintProps, images);
        target.drawStrings(strings);
    }

    /**
     * Set the width scalar
     * 
     * @param props
     * @return
     */
    private void setScaleWidth(PaintProperties props) {
        double screenToWorldWidthRatio = props.getCanvasBounds().width
                / props.getView().getExtent().getWidth();
        scaleWidthValue = (ImageSize.MEDIUM.getWidth() / 2.0)
                / screenToWorldWidthRatio;
    }

    /**
     * get the scale width value
     * 
     * @return
     */
    private double getScaleWidth() {
        return scaleWidthValue;
    }

    /**
     * Set the height scalar
     * 
     * @param props
     * @return
     */
    private void setScaleHeight(PaintProperties props) {
        double screenToWorldHeightRatio = props.getCanvasBounds().height
                / props.getView().getExtent().getHeight();
        scaleHeightValue = (ImageSize.MEDIUM.getHeight() / 2.0)
                / screenToWorldHeightRatio;
    }

    /**
     * Get the scalar height
     * 
     * @return
     */
    private double getScaleHeight() {
        return scaleHeightValue;
    }

    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        menuManager.add(new DamCatalogLaunchAction());
    }

    private class DamCatalogLaunchAction extends AbstractRightClickAction {
        private static final String BUTTON_TEXT = "DamCrest";

        private static final String DC_BUNDLE_LOC = "bundles/run-DamCREST.xml";

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#getText()
         */
        @Override
        public String getText() {
            return BUTTON_TEXT;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            IDisplayPaneContainer container = getResourceContainer();
            if (container != null) {
                IDisplayPane pane = container.getActiveDisplayPane();
                int x = pane.getLastMouseX();
                int y = pane.getLastMouseY();

                Coordinate coord = container.translateClick(x, y);
                Envelope env = new Envelope(coord);
                List<?> elements = damStrTree.query(env);
                double curDist=Double.MAX_VALUE;
                DamMaster foundDam = null;
                if (elements.size() > 0) {
                	//find the closest one
                	for (Object obj : elements) {
                        DamMaster dam = (DamMaster) obj;
                        double distance=Math.pow((dam.getLatitudeDam()-coord.y),2) + Math.pow((dam.getLongitudeDam()-coord.x), 2);
                        if (distance<curDist) {
                        	curDist=distance;
                        	foundDam=dam;
                        }
                	}
                    
                	try {
                        AppLauncherHandler alh = new AppLauncherHandler();
                        alh.execute(DC_BUNDLE_LOC, foundDam.getNidid());
                    } catch (ExecutionException e) {
                         e.printStackTrace();
                    }
                } else {
                    Shell shell = PlatformUI.getWorkbench()
                            .getActiveWorkbenchWindow().getShell();

                    MessageBox mb = new MessageBox(shell, SWT.ICON_WARNING
                            | SWT.OK);
                    mb.setText("Error");
                    mb.setMessage("The mouse pointer must be on a gage to use this feature.");
                    mb.open();

                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.map.rsc.PointResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        font.dispose();
        damIcon.dispose();
        dams.clear();

        HydroDisplayManager manager = HydroDisplayManager.getInstance();
        manager.setDamList(null);
    }

    /**
     * Clear the dam map.
     */
    public void resetDamMap() {
        dams.clear();
    }

    /**
     * @return the isDisposed
     */
    public boolean isDisposed() {
        return getStatus() == ResourceStatus.DISPOSED;
    }

}
