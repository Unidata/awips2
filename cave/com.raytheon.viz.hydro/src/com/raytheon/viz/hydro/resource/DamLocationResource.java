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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.app.launcher.handlers.AppLauncherHandler;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.data.prep.IODataPreparer;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.hydro.gagedisplay.HydroImageMaker;
import com.raytheon.viz.hydro.gagedisplay.HydroImageMaker.ImageSize;
import com.raytheon.viz.hydro.pointdatacontrol.PointDataControlManager;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.data.DamMaster;
import com.raytheon.viz.hydrocommon.resource.HydroPointResource;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.index.strtree.STRtree;

/**
 * Resource to dislpay the DamCrest icons.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2009            mpduff     Initial creation
 * Sep 23, 2009 3069       mpduff      Changed the parent class to HydroPointResource.
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DamLocationResource extends
		HydroPointResource<DamLocationResourceData> implements
        IContextMenuContributor {
    private final Map<Coordinate, DamMaster> damMap = new HashMap<Coordinate, DamMaster>();

    private HashMap<Coordinate, IImage> damRenderables = new HashMap<Coordinate, IImage>();

    private STRtree damStrTree = new STRtree();

    private IFont font = null;

    private IGraphicsTarget target;

    private double scaleWidthValue = 0.0;

    private double scaleHeightValue = 0.0;

    private final boolean isName = false;

    private boolean isDisposed = false;

    private final RGB labelColor = RGBColors.getRGBColor("white");

	public DamLocationResource(DamLocationResourceData resourceData,
			LoadProperties loadProperties) {
		super(resourceData, loadProperties);
		this.getCapability(ColorableCapability.class).setColor(
				resourceData.getColor());
    }

	@Override
	protected void initInternal(IGraphicsTarget target) throws VizException {
		populateDamList();
	}
	
	private void populateDamList() {
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        HydroDisplayManager displayManager = HydroDisplayManager.getInstance();

		List<DamMaster> damList = displayManager.getDamList();
		if (damList != null) {
			pdcManager.setDamLocationResource(this);
			damMap.clear();
			for (DamMaster dm : damList) {
				addPoint(dm);
			}
		} else {
			damList = getResourceData().getDamList();
			if (damList != null) {
				displayManager.setDamList(damList);
				pdcManager.setDamLocationResource(this);
				damMap.clear();
				for (DamMaster dm : damList) {
					addPoint(dm);
				}
			}
		}
	}

    public void resetStrTree() {
        damStrTree = null;
        damStrTree = new STRtree();
    }

    /**
     * Add a Dam site to the map.
     * 
     * @param master
     *            The DamMaster object
     */
    public void addPoint(DamMaster master) {
        Coordinate xy = new Coordinate(master.getLongitudeDam(),
                master.getLatitudeDam());
        damMap.put(xy, master);

        /* Create a small envelope around the point */
        Coordinate p1 = new Coordinate(master.getLongitudeDam() + .03,
                master.getLatitudeDam() + .03);
        Coordinate p2 = new Coordinate(master.getLongitudeDam() - .03,
                master.getLatitudeDam() - .03);
        Envelope env = new Envelope(p1, p2);
        ArrayList<Object> data = new ArrayList<Object>();
        data.add(xy);
        data.add("DAM:  " + master.getDamName());
        damStrTree.insert(env, data);
    }

    /**
     * Create the dam icons.
     * 
     * @return HashMap of Coordinate to Image objects
     */
    private HashMap<Coordinate, IImage> getDamRenderables() {
        Iterator<Coordinate> iter = damMap.keySet().iterator();
        damRenderables.clear();
        
        while (iter.hasNext()) {
            Coordinate c = iter.next();
            DamMaster master = damMap.get(c);

            try {
                IImage image = target.initializeRaster(
                        new IODataPreparer(HydroImageMaker.getDamIcon(),
                                master.getNidid(), 0), null);
                damRenderables.put(c, image);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        return damRenderables;
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

        int fontSize = manager.getFontSize();
        if (font != null) {
            font = target.initializeFont("Dialog", fontSize, null);
        }

        this.target = target;

        /* Should we display the dam icons? */
        if (manager.getDamList() != null) {
            populateDamList();
            damRenderables = getDamRenderables();
            Iterator<Coordinate> iter = damMap.keySet().iterator();

            while (iter.hasNext()) {
                Coordinate c = iter.next();
                double[] pixel = descriptor.worldToPixel(new double[] { c.x,
                        c.y });

                if (pixel != null) {
                    if (paintProps.getView().getExtent().contains(pixel)) {

                        setScaleWidth(paintProps);
                        setScaleHeight(paintProps);

                        /* Draw the icons */
                        if (manager.isDisplayDamIcon()) {
                            target.drawRaster(damRenderables.get(c),
                                    getPixelCoverage(c), paintProps);
                        }

                        /* Draw the labels */
                        drawPlotInfo(c, damMap.get(c));
                    }
                }
            }
        }
    }

    /**
     * Draw the text labels.
     * 
     * @param c
     *            coordinate value
     * @param data
     *            Dam Data
     * @throws VizException
     */
    private void drawPlotInfo(Coordinate c, DamMaster data) throws VizException {
        HydroDisplayManager displayManager = HydroDisplayManager.getInstance();
        boolean displayId = displayManager.isDisplayDamId();
        boolean displayName = displayManager.isDisplayDamName();

        if (displayId) {
            double[] centerpixels = descriptor.worldToPixel(new double[] { c.x,
                    c.y });
            Coordinate idCoor = new Coordinate(centerpixels[0]
                    - getScaleWidth(), centerpixels[1] - getScaleHeight());

            target.drawString(font, data.getNidid(), idCoor.x, idCoor.y, 0.0,
                    IGraphicsTarget.TextStyle.NORMAL, labelColor,
                    IGraphicsTarget.HorizontalAlignment.LEFT, 0.0);
        }

        if (displayName) {
            double[] centerpixels = descriptor.worldToPixel(new double[] { c.x,
                    c.y });
            Coordinate nameCoor = new Coordinate(centerpixels[0]
                    - getScaleWidth(), centerpixels[1] + getScaleHeight());

            target.drawString(font, data.getDamName(), nameCoor.x, nameCoor.y,
                    0.0, IGraphicsTarget.TextStyle.NORMAL, labelColor,
                    IGraphicsTarget.HorizontalAlignment.LEFT, 0.0);
        }
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

    /**
     * gets the pixel coverage for this image
     * 
     * @return
     */
    private PixelCoverage getPixelCoverage(Coordinate c) {

        double[] centerpixels = descriptor
                .worldToPixel(new double[] { c.x, c.y });
        Coordinate ul = new Coordinate(centerpixels[0] - getScaleWidth(),
                centerpixels[1] - getScaleHeight());
        Coordinate ur = new Coordinate(centerpixels[0] + getScaleWidth(),
                centerpixels[1] - getScaleHeight());
        Coordinate lr = new Coordinate(centerpixels[0] + getScaleWidth(),
                centerpixels[1] + getScaleHeight());
        Coordinate ll = new Coordinate(centerpixels[0] - getScaleWidth(),
                centerpixels[1] + getScaleHeight());

        return new PixelCoverage(ul, ur, lr, ll);
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
                if (elements.size() > 0) {
                    Iterator<?> iter = elements.iterator();
                    if (iter.hasNext()) {
                        ArrayList<?> data = (ArrayList<?>) iter.next();
                        String nadid = damMap.get(data.get(0)).getNidid();
                        try {
                            AppLauncherHandler alh = new AppLauncherHandler();
                            alh.execute(DC_BUNDLE_LOC, nadid);
                        } catch (ExecutionException e) {
                            e.printStackTrace();
                        }
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
        super.disposeInternal();

        if (damRenderables != null) {
            Iterator<IImage> iter = damRenderables.values().iterator();
            while (iter.hasNext()) {
                iter.next().dispose();
            }
        }
        damMap.clear();
        setDisposed(true);

        HydroDisplayManager manager = HydroDisplayManager.getInstance();
        manager.setDamList(null);

        if (font != null) {
            font.dispose();
        }
    }

    /**
     * Clear the data.
     */
    public void clearData() {
        ResourceList rl = descriptor.getResourceList();

        for (ResourcePair pair : rl) {
            AbstractVizResource<?, ?> rsc = pair.getResource();
            if (rsc instanceof DamLocationResource) {
                rl.removeRsc(rsc);
                break;
            }
        }
    }

    /**
     * Clear the dam map.
     */
    public void resetDamMap() {
        damMap.clear();
    }

    public Map<Coordinate, DamMaster> getDamMap() {
        return damMap;
    }

    /**
     * @return the isName
     */
    public boolean isName() {
        return isName;
    }

    /**
     * @param name
     *            the name to set
     */
    @Override
	public void setName(String name) {
		this.resourceData.setName(name);
    }

    /**
     * @return the isDisposed
     */
    public boolean isDisposed() {
        return isDisposed;
    }

    /**
     * @param isDisposed
     *            the isDisposed to set
     */
    public void setDisposed(boolean isDisposed) {
        this.isDisposed = isDisposed;
    }
}
