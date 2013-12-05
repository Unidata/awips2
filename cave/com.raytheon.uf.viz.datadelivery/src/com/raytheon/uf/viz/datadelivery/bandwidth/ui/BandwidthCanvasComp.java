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
package com.raytheon.uf.viz.datadelivery.bandwidth.ui;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TimerTask;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.events.MouseWheelListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Slider;

import com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthGraphData;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.service.SubscriptionNotificationResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.notification.INotificationObserver;
import com.raytheon.uf.viz.core.notification.NotificationException;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.uf.viz.datadelivery.bandwidth.ui.BandwidthImageMgr.CanvasImages;
import com.raytheon.uf.viz.datadelivery.bandwidth.ui.BandwidthImageMgr.GraphSection;
import com.raytheon.uf.viz.datadelivery.bandwidth.ui.BandwidthImageMgr.GraphType;
import com.raytheon.uf.viz.datadelivery.bandwidth.ui.BandwidthImageMgr.SortBy;
import com.raytheon.uf.viz.datadelivery.common.ui.IDialogClosed;
import com.raytheon.uf.viz.datadelivery.common.ui.SubscriptionViewer;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;

/**
 * 
 * This class contains all of the canvases for graphing data. There are
 * Subscription Graph, Bandwidth Graph, X Label, Y Label, X Header, and Y Header
 * canvases on the display. The Graph, X Label, and Y Label canvases can be
 * zoomed and panned.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 6, 2012    1269     lvenable    Initial creation.
 * Dec 13, 2012   1269     lvenable    Fixes and updates.
 * Jan 07, 2013   1451     djohnson    Use TimeUtil.newGmtCalendar().
 * Jan 28, 2013   1529     djohnson    Disable menu items if no subscriptions selected.
 * Oct 28, 2013   2430     mpduff      Add % of bandwidth utilized graph.
 * Nov 19, 2013   1531     mpduff      Made graph resizable.
 * Nov 25, 2013   2545     mpduff      Default to Opsnet if Network not available yet.
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class BandwidthCanvasComp extends Composite implements IDialogClosed,
        INotificationObserver, IDataUpdated {

    /** UFStatus handler. */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(BandwidthCanvasComp.class);

    /** Missing value */
    private final int MISSING = -999;

    /** x direction pixel buffer */
    private final int xSpaceBuffer = 20;

    /** y direction pixel buffer */
    private final int ySpaceBuffer = 10;

    /** Height without buffer */
    private final int heightNoBuffer = 420;

    /** Height with buffer */
    private final int heightWithBuffer = 420 + ySpaceBuffer * 2;

    /** y label width */
    private final int yLabelWidth = 140;

    /** Utilization header image height */
    private final int utilizationHeaderHeight = 40;

    /** Utilization graph image height */
    private final int utilizationGraphHeight = 60;

    /** Parent composite */
    private final Composite parentComp;

    /** Display reference */
    private final Display display;

    /** Vertical Scale (slider) */
    private Slider verticalSlider;

    /** Horizontal Scale (slider) */
    private Slider horizontalSlider;

    /** Flag indicating if the mouse is down */
    private boolean mouseDown = false;

    /** Map of canvas settings */
    private Map<CanvasImages, CanvasSettings> canvasSettingsMap;

    /** Map of Canvas objects */
    private final Map<CanvasImages, Canvas> canvasMap = new HashMap<CanvasImages, Canvas>();

    /** Map of Images */
    private Map<CanvasImages, Image> imgMap;

    /** Offset between the corner of the image and the corner of the canvas */
    private final Point cornerPointOffset = new Point(0, 0);

    /** Previous mouse location */
    private final Point previousMousePoint = new Point(0, 0);

    /** Graph canvas size */
    private final Point graphCanvasSize = new Point(0, 0);

    /** Graph canvas settings */
    private CanvasSettings graphCanvasSettings;

    /** The graph data object */
    private BandwidthGraphData bgd;

    /** The image manager */
    private BandwidthImageMgr imageMgr;

    /** The subscription names */
    private Collection<String> subscriptionNames;

    /** The live update timer */
    private ScheduledExecutorService timer;

    /** Current time object for the live update timer */
    private Calendar currentTime;

    /** The previous minute used in the live update timer */
    private int previousMinute;

    /** Graph data utility */
    private final GraphDataUtil graphDataUtil;

    /** Counts the minutes until the next full update. */
    private int fullUpdateMinuteCount = 0;

    /** Vertical line marking the mouse pointer's location */
    private int mouseMarker;

    /** Bandwidth popup menu */
    private Menu bandwidthPopupMenu;

    /** Initialized flag */
    protected boolean initialized = false;

    /** x header canvas object */
    private XHeaderCanvas xHeaderCanvas;

    /** y header canvas object */
    private YHeaderCanvas yHeaderCanvas;

    /** Point object holding graph size */
    private Point graphSize;

    /** Vertical slider composite */
    private Composite vSliderComp;

    /** Horizontal slider composite */
    private Composite hSliderComp;

    /** Utilization graph header canvas */
    private Canvas utilizationHeaderCanvas;

    /** Utilization graph label canvas */
    private Canvas utilizationLabelCanvas;

    /** Utilization graph canvas */
    private Canvas utilizationGraphCanvas;

    /** x label canvas */
    private Canvas xLabelCanvas;

    /** y label canvas */
    private Canvas yLabelCanvas;

    /** Graph canvas */
    private Canvas graphCanvas;

    /**
     * Constructor.
     * 
     * @param parentComp
     *            Parent composite.
     * @param graphDataUtil
     *            Bandwidth graph data object
     */
    public BandwidthCanvasComp(Composite parentComp, GraphDataUtil graphDataUtil) {
        super(parentComp, SWT.BORDER);

        this.parentComp = parentComp;
        this.display = this.parentComp.getDisplay();
        this.graphDataUtil = graphDataUtil;
        this.bgd = this.graphDataUtil.getGraphData(false);

        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        this.graphDataUtil.setDataUpdateCallback(this);
        NotificationManagerJob.addObserver("notify.msg", this);

        this.setBackground(display.getSystemColor(SWT.COLOR_WHITE));

        generateCanvasSettings();
        currentTime = TimeUtil.newGmtCalendar();

        imageMgr = new BandwidthImageMgr(parentComp, canvasSettingsMap, bgd,
                currentTime.getTimeInMillis());
        subscriptionNames = imageMgr.getSubscriptionNames();

        getAllCanvasImages();

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(4, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        this.setLayout(gl);
        this.setLayoutData(gd);

        createFillerSpace(2);
        createUtilizationHeaderCanvas();
        createFillerSpace(1);

        createFillerSpace(1);
        createUtilizationLabelCanvas();
        createUtilizationGraphCanvas();
        createFillerSpace(1);

        createFillerSpace(2);
        createXHeaderCanvas();
        createFillerSpace(1);

        createYHeaderCanvas();
        createYLabelCanvas();
        createGraphCanvas();
        createVerticalSlider();

        createFillerSpace(2);
        createXLabelCanvas();
        createFillerSpace(1);

        createFillerSpace(2);
        createHorizontalSlider();
        createFillerSpace(1);

        CanvasSettings cs = canvasSettingsMap.get(CanvasImages.GRAPH);
        cornerPointOffset.x = 0;
        cornerPointOffset.y = 0 - cs.getImageHeight() + cs.getCanvasHeight();

        Map<String, Boolean> checkMap = new HashMap<String, Boolean>();
        Iterator<String> iter = subscriptionNames.iterator();
        while (iter.hasNext()) {
            String name = iter.next();
            checkMap.put(name, false);
        }

        imageMgr.setCheckMap(checkMap);

        // Setup the live update
        previousMinute = currentTime.get(Calendar.MINUTE);
        createTimer();

        this.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                graphDataUtil.cancelThread();
                timer.shutdown();
                deregisterNotification();
            }
        });

        /*
         * Add a control resize listener to resize components
         */
        this.addControlListener(new ControlAdapter() {
            @Override
            public void controlResized(ControlEvent e) {
                if (initialized) {
                    updateCanvasSettings();
                    updateCanvases();
                    layout();
                }
            }
        });
    }

    /**
     * Deregister the notification.
     */
    private void deregisterNotification() {
        NotificationManagerJob.removeObserver("notify.msg", this);
    }

    /**
     * Create the live update timer.
     */
    private void createTimer() {
        long updateRate = TimeUtil.MILLIS_PER_SECOND * 5;
        timer = Executors.newSingleThreadScheduledExecutor();
        timer.scheduleAtFixedRate(new UpdateTask(), updateRate, updateRate,
                TimeUnit.MILLISECONDS);
    }

    /**
     * Create the canvas settings. These are the width and height of each canvas
     * and the buffer around the image.
     */
    private void generateCanvasSettings() {
        if (canvasSettingsMap == null) {
            canvasSettingsMap = new HashMap<CanvasImages, CanvasSettings>();
        } else {
            canvasSettingsMap.clear();
        }

        CanvasSettings cs;

        graphSize = calculateGraphImageSize(heightNoBuffer, xSpaceBuffer,
                ySpaceBuffer);

        // Create the Graph canvas settings
        cs = new CanvasSettings(740, heightWithBuffer, graphSize.x,
                graphSize.y, xSpaceBuffer, ySpaceBuffer);
        canvasSettingsMap.put(CanvasImages.GRAPH, cs);
        graphCanvasSize.x = cs.getCanvasWidth();
        graphCanvasSize.y = cs.getCanvasHeight();
        graphCanvasSettings = cs;

        // Create the X label canvas settings
        cs = new CanvasSettings(740, 60, graphSize.x, 60, xSpaceBuffer,
                ySpaceBuffer);
        canvasSettingsMap.put(CanvasImages.X_LABEL, cs);

        // Create the Y label canvas settings
        cs = new CanvasSettings(yLabelWidth, heightWithBuffer, yLabelWidth,
                graphSize.y, xSpaceBuffer, ySpaceBuffer);
        canvasSettingsMap.put(CanvasImages.Y_LABEL, cs);

        // Create the X header canvas settings
        cs = new CanvasSettings(740, 60, 740, 60, 20, 0);
        canvasSettingsMap.put(CanvasImages.X_HEADER, cs);

        // Create the y header canvas settings
        cs = new CanvasSettings(35, heightWithBuffer, 35, 440, 0, 0);
        canvasSettingsMap.put(CanvasImages.Y_HEADER, cs);

        // Create the bandwidth utilization header settings
        cs = new CanvasSettings(yLabelWidth, 60, yLabelWidth, 0, 20, 0);
        canvasSettingsMap.put(CanvasImages.UTILIZATION_LABEL, cs);

        // Create the bandwidth utilization graph settings
        cs = new CanvasSettings(740, 60, graphSize.x, 100, xSpaceBuffer, 0);
        canvasSettingsMap.put(CanvasImages.UTILIZATION_GRAPH, cs);

        // Create the Utilization header canvas settings
        cs = new CanvasSettings(740, utilizationHeaderHeight, 740,
                utilizationHeaderHeight, xSpaceBuffer, 0);
        canvasSettingsMap.put(CanvasImages.UTILIZATION_HEADER, cs);
    }

    /**
     * Calculate the graph image size.
     * 
     * @param origHeight
     *            Original Height
     * @param xBufferMargin
     *            Margin buffer in x direction
     * @param yBufferMargin
     *            Margin buffer in the y direction
     * @return Point object for the image size
     */
    private Point calculateGraphImageSize(int origHeight, int xBufferMargin,
            int yBufferMargin) {

        int newImageHeight = origHeight + yBufferMargin * 2;

        int numOfSubs = 0;
        int totalHeight = 0;

        if (bgd != null) {
            if (imageMgr == null) {
                // Default to OPSNET
                numOfSubs = bgd.getNumberOfSubscriptions(Network.OPSNET);
            } else {
                numOfSubs = bgd.getNumberOfSubscriptions(imageMgr.getNetwork());
            }
            totalHeight = AbstractCanvasImage.TEXT_OFFSET * numOfSubs;
        }

        if ((totalHeight + yBufferMargin * 2) > newImageHeight) {
            newImageHeight = totalHeight + yBufferMargin * 2;
        }

        Point graphSize = new Point(0, 0);

        graphSize.x = 2880 + xBufferMargin * 2;
        graphSize.y = newImageHeight;

        return graphSize;
    }

    /**
     * Create a filler space where the canvases are not occupying.
     */
    private void createFillerSpace(int span) {
        GridData gd = new GridData();
        gd.horizontalSpan = span;

        Label spaceLbl = new Label(this, SWT.NONE);
        spaceLbl.setLayoutData(gd);
        spaceLbl.setVisible(false);
    }

    /**
     * Create the utilization graph header canvas.
     */
    private void createUtilizationHeaderCanvas() {
        utilizationHeaderCanvas = new Canvas(this, SWT.DOUBLE_BUFFERED);
        CanvasSettings cs = canvasSettingsMap
                .get(CanvasImages.UTILIZATION_HEADER);

        GridData gd = new GridData(cs.getCanvasWidth(), cs.getCanvasHeight());
        utilizationHeaderCanvas.setLayoutData(gd);

        utilizationHeaderCanvas.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                drawUtilizationHeaderCanvas(e.gc);
            }
        });

        utilizationHeaderCanvas.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                if (e.button == 1) {
                    handleUtilizationHeaderMouseEvent(e);
                }
            }
        });

        canvasMap.put(CanvasImages.UTILIZATION_HEADER, utilizationHeaderCanvas);
    }

    /**
     * Create the utilization graph's label canvas.
     */
    private void createUtilizationLabelCanvas() {
        utilizationLabelCanvas = new Canvas(this, SWT.DOUBLE_BUFFERED);
        CanvasSettings cs = canvasSettingsMap
                .get(CanvasImages.UTILIZATION_LABEL);

        GridData gd = new GridData(cs.getCanvasWidth(), cs.getCanvasHeight());
        utilizationLabelCanvas.setLayoutData(gd);

        utilizationLabelCanvas.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                drawUtilizationLabelCanvas(e.gc);
            }
        });

        canvasMap.put(CanvasImages.UTILIZATION_LABEL, utilizationLabelCanvas);
    }

    /**
     * Create the utilization graph canvas.
     */
    private void createUtilizationGraphCanvas() {
        utilizationGraphCanvas = new Canvas(this, SWT.DOUBLE_BUFFERED);
        CanvasSettings cs = canvasSettingsMap
                .get(CanvasImages.UTILIZATION_GRAPH);

        GridData gd = new GridData(cs.getCanvasWidth(), cs.getCanvasHeight());
        utilizationGraphCanvas.setLayoutData(gd);

        utilizationGraphCanvas.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                drawUtilizationGraphCanvas(e.gc);
            }
        });

        /*
         * Add a mouse track listener to determine when the mouse hovers over
         * the canvas.
         */
        utilizationGraphCanvas.addMouseTrackListener(new MouseTrackAdapter() {
            @Override
            public void mouseExit(MouseEvent e) {
                // Remove mouse vertical line
                mouseMarker = MISSING;
                canvasMap.get(CanvasImages.GRAPH).redraw();
                canvasMap.get(CanvasImages.UTILIZATION_GRAPH).redraw();
            }
        });

        /*
         * Add a mouse move listener to determine when the mouse is moving over
         * the canvas.
         */
        utilizationGraphCanvas.addMouseMoveListener(new MouseMoveListener() {
            @Override
            public void mouseMove(MouseEvent e) {

                // If the mouse button is not pressed then set the previous
                // mouse x,y coordinates to the current mouse x,y position.
                if (!mouseDown) {
                    previousMousePoint.x = e.x;
                    previousMousePoint.y = e.y;
                    mouseMarker = e.x;
                    canvasMap.get(CanvasImages.GRAPH).redraw();
                    canvasMap.get(CanvasImages.UTILIZATION_GRAPH).redraw();
                    return;
                }

                mouseMarker = MISSING;
            }
        });

        utilizationGraphCanvas.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                if (e.button == 3) {
                    bandwidthUsedPopupMenu();
                }
            }
        });

        canvasMap.put(CanvasImages.UTILIZATION_GRAPH, utilizationGraphCanvas);
    }

    /**
     * Create the X header canvas.
     */
    private void createXHeaderCanvas() {
        xHeaderCanvas = new XHeaderCanvas(this, SWT.DOUBLE_BUFFERED);

        CanvasSettings cs = canvasSettingsMap.get(CanvasImages.X_HEADER);

        GridData gd = new GridData(cs.getCanvasWidth(), cs.getCanvasHeight());
        xHeaderCanvas.setLayoutData(gd);

        xHeaderCanvas.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                drawXHeaderCanvas(e.gc);
            }
        });

        /*
         * Add a mouse listener to perform and action when the mouse is clicked
         * down.
         */
        xHeaderCanvas.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                if (e.button == 1) {
                    handleXHeaderMouseEvent(e);
                }
            }
        });

        canvasMap.put(CanvasImages.X_HEADER, xHeaderCanvas);
    }

    /**
     * Create the Y header canvas.
     */
    private void createYHeaderCanvas() {
        yHeaderCanvas = new YHeaderCanvas(this, SWT.DOUBLE_BUFFERED);

        CanvasSettings cs = canvasSettingsMap.get(CanvasImages.Y_HEADER);

        GridData gd = new GridData(cs.getCanvasWidth(), cs.getCanvasHeight());
        yHeaderCanvas.setLayoutData(gd);

        yHeaderCanvas.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                drawYHeaderCanvas(e.gc);
            }
        });

        canvasMap.put(CanvasImages.Y_HEADER, yHeaderCanvas);
    }

    /**
     * Create the graph canvas.
     */
    private void createGraphCanvas() {
        graphCanvas = new Canvas(this, SWT.DOUBLE_BUFFERED);

        GridData gd = new GridData(graphCanvasSettings.getCanvasWidth(),
                graphCanvasSettings.getCanvasHeight());
        graphCanvas.setLayoutData(gd);

        /*
         * Add a paint listener to draw the graph image on the graph canvas.
         */
        graphCanvas.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                drawGraphCanvas(e.gc);
            }
        });

        /*
         * Add a mouse listener to determine if the left mouse button is up or
         * down.
         */
        graphCanvas.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                if (e.button == 1) {
                    mouseDown = true;
                    setGraphSelection(e);
                } else if (e.button == 3) {
                    graphPopupMenu();
                }
            }

            @Override
            public void mouseUp(MouseEvent e) {
                if (e.button == 1) {
                    mouseDown = false;
                }
            }
        });

        /*
         * Add a mouse track listener to determine when the mouse hovers over
         * the canvas.
         */
        graphCanvas.addMouseTrackListener(new MouseTrackAdapter() {
            @Override
            public void mouseExit(MouseEvent e) {
                // Remove mouse vertical line
                mouseMarker = MISSING;
                canvasMap.get(CanvasImages.GRAPH).redraw();
                canvasMap.get(CanvasImages.UTILIZATION_GRAPH).redraw();
            }

            @Override
            public void mouseHover(MouseEvent e) {
                displayToolTipText(e, CanvasImages.GRAPH);
            }
        });

        /*
         * Add a mouse move listener to determine when the mouse is moving over
         * the canvas.
         */
        graphCanvas.addMouseMoveListener(new MouseMoveListener() {
            @Override
            public void mouseMove(MouseEvent e) {

                // If the mouse button is not pressed then set the previous
                // mouse x,y coordinates to the current mouse x,y position.
                if (mouseDown == false) {
                    previousMousePoint.x = e.x;
                    previousMousePoint.y = e.y;
                    mouseMarker = e.x;
                    canvasMap.get(CanvasImages.GRAPH).redraw();
                    canvasMap.get(CanvasImages.UTILIZATION_GRAPH).redraw();
                    return;
                }

                mouseMarker = MISSING;

                cornerPointOffset.x -= previousMousePoint.x - e.x;
                cornerPointOffset.y -= previousMousePoint.y - e.y;

                if (cornerPointOffset.x < 0
                        - graphCanvasSettings.getImageWidth()
                        + graphCanvasSettings.getCanvasWidth()) {
                    cornerPointOffset.x = 0
                            - graphCanvasSettings.getImageWidth()
                            + graphCanvasSettings.getCanvasWidth();
                }

                if (cornerPointOffset.x > 0) {
                    cornerPointOffset.x = 0;
                }

                if (cornerPointOffset.y < 0
                        - graphCanvasSettings.getImageHeight()
                        + graphCanvasSettings.getCanvasHeight()) {
                    cornerPointOffset.y = 0
                            - graphCanvasSettings.getImageHeight()
                            + graphCanvasSettings.getCanvasHeight();
                }

                if (cornerPointOffset.y > 0) {
                    cornerPointOffset.y = 0;
                }

                verticalSlider.setSelection(cornerPointOffset.y * -1);
                horizontalSlider.setSelection(cornerPointOffset.x * -1);

                canvasMap.get(CanvasImages.GRAPH).redraw();
                canvasMap.get(CanvasImages.X_LABEL).redraw();
                canvasMap.get(CanvasImages.Y_LABEL).redraw();
                canvasMap.get(CanvasImages.UTILIZATION_GRAPH).redraw();

                previousMousePoint.x = e.x;
                previousMousePoint.y = e.y;
            }
        });

        graphCanvas.addMouseWheelListener(new MouseWheelListener() {
            @Override
            public void mouseScrolled(MouseEvent e) {
                handleScrollWheel(e);
            }
        });

        canvasMap.put(CanvasImages.GRAPH, graphCanvas);
    }

    /**
     * Set the graph selection.
     * 
     * @param me
     *            The Mouse Event object
     */
    private void setGraphSelection(MouseEvent me) {
        Point mouseCoord = new Point(0, 0);
        mouseCoord.x = Math.abs(cornerPointOffset.x) + me.x;
        mouseCoord.y = Math.abs(cornerPointOffset.y) + me.y;

        imageMgr.performAction(CanvasImages.GRAPH, mouseCoord);
        redrawImage(CanvasImages.GRAPH);
        redrawImage(CanvasImages.Y_LABEL);

    }

    /**
     * Create the X label canvas.
     */
    private void createXLabelCanvas() {
        xLabelCanvas = new Canvas(this, SWT.DOUBLE_BUFFERED);

        CanvasSettings cs = canvasSettingsMap.get(CanvasImages.X_LABEL);

        GridData gd = new GridData(cs.getCanvasWidth(), cs.getCanvasHeight());
        xLabelCanvas.setLayoutData(gd);

        xLabelCanvas.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                drawXLabelCanvas(e.gc);
            }
        });

        canvasMap.put(CanvasImages.X_LABEL, xLabelCanvas);
    }

    /**
     * Create the Y label canvas.
     */
    private void createYLabelCanvas() {
        yLabelCanvas = new Canvas(this, SWT.DOUBLE_BUFFERED);

        CanvasSettings cs = canvasSettingsMap.get(CanvasImages.Y_LABEL);

        GridData gd = new GridData(cs.getCanvasWidth(), cs.getCanvasHeight());
        yLabelCanvas.setLayoutData(gd);

        yLabelCanvas.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                drawYLabelCanvas(e.gc);
            }
        });

        yLabelCanvas.addMouseTrackListener(new MouseTrackAdapter() {
            @Override
            public void mouseHover(MouseEvent e) {
                displayToolTipText(e, CanvasImages.Y_LABEL);
            }
        });

        yLabelCanvas.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                if (e.button == 1) {
                    handleSubscriptionCheckboxClick(e);
                } else if (e.button == 3) {
                    yLabelPopupMenu();
                }
            }
        });

        yLabelCanvas.addMouseWheelListener(new MouseWheelListener() {
            @Override
            public void mouseScrolled(MouseEvent e) {
                handleScrollWheel(e);

            }
        });

        canvasMap.put(CanvasImages.Y_LABEL, yLabelCanvas);
    }

    /**
     * Create the vertical slider bar.
     */
    private void createVerticalSlider() {
        GridData gd = new GridData(SWT.CENTER, SWT.FILL, false, true);
        GridLayout gl = new GridLayout(1, false);
        vSliderComp = new Composite(this, SWT.NONE);
        vSliderComp.setLayout(gl);
        vSliderComp.setLayoutData(gd);
        vSliderComp.setBackground(display.getSystemColor(SWT.COLOR_WHITE));

        int canvasHeight = this.canvasSettingsMap.get(CanvasImages.GRAPH)
                .getCanvasHeight();
        int imageHeight = this.canvasSettingsMap.get(CanvasImages.GRAPH)
                .getImageHeight();

        if (imageHeight < canvasHeight) {
            imageHeight = canvasHeight;
        }

        gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        verticalSlider = new Slider(vSliderComp, SWT.VERTICAL);
        verticalSlider.setLayoutData(gd);
        verticalSlider.setMinimum(0);
        verticalSlider.setMaximum(imageHeight - canvasHeight
                + verticalSlider.getThumb());
        verticalSlider.setSelection(imageHeight - canvasHeight);
        verticalSlider.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleVerticalScaleChange();
            }
        });
    }

    /**
     * Create the horizontal slider bar.
     */
    private void createHorizontalSlider() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);
        hSliderComp = new Composite(this, SWT.NONE);
        hSliderComp.setLayout(gl);
        hSliderComp.setLayoutData(gd);
        hSliderComp.setBackground(display.getSystemColor(SWT.COLOR_WHITE));

        int canvasWidth = this.canvasSettingsMap.get(CanvasImages.GRAPH)
                .getCanvasWidth();
        int imageWidth = this.canvasSettingsMap.get(CanvasImages.GRAPH)
                .getImageWidth();
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        horizontalSlider = new Slider(hSliderComp, SWT.HORIZONTAL);
        horizontalSlider.setBackground(display.getSystemColor(SWT.COLOR_WHITE));
        horizontalSlider.setLayoutData(gd);
        horizontalSlider.setMinimum(0);
        horizontalSlider.setMaximum(imageWidth - canvasWidth
                + horizontalSlider.getThumb());
        horizontalSlider.setSelection(0);
        horizontalSlider.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleHorizontalScaleChange();
            }
        });
    }

    /**
     * Event handler for vertical slider changes.
     */
    private void handleVerticalScaleChange() {
        cornerPointOffset.y = 0 - verticalSlider.getSelection();

        if (cornerPointOffset.y > 0) {
            cornerPointOffset.y = 0;
        }

        canvasMap.get(CanvasImages.GRAPH).redraw();
        canvasMap.get(CanvasImages.Y_LABEL).redraw();
    }

    /**
     * Event handler for horizontal slider changes.
     */
    private void handleHorizontalScaleChange() {
        cornerPointOffset.x = 0 - horizontalSlider.getSelection();

        if (cornerPointOffset.x > 0) {
            cornerPointOffset.x = 0;
        }

        canvasMap.get(CanvasImages.GRAPH).redraw();
        canvasMap.get(CanvasImages.X_LABEL).redraw();
        canvasMap.get(CanvasImages.UTILIZATION_GRAPH).redraw();
    }

    private void drawUtilizationHeaderCanvas(GC gc) {
        gc.drawImage(imgMap.get(CanvasImages.UTILIZATION_HEADER), 0, 0);
    }

    private void drawUtilizationLabelCanvas(GC gc) {
        gc.drawImage(imgMap.get(CanvasImages.UTILIZATION_LABEL), 0, 0);

        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        CanvasSettings cs = canvasSettingsMap
                .get(CanvasImages.UTILIZATION_LABEL);
        gc.drawLine(0, 0, cs.getCanvasWidth(), 0);
        gc.drawLine(0, 0, 0, cs.getCanvasHeight());
        gc.drawLine(0, cs.getCanvasHeight() - 1, cs.getCanvasWidth(),
                cs.getCanvasHeight() - 1);
        gc.drawLine(cs.getCanvasWidth() - 1, 0, cs.getCanvasWidth() - 1,
                cs.getCanvasHeight() - 1);
    }

    private void drawUtilizationGraphCanvas(GC gc) {
        gc.drawImage(imgMap.get(CanvasImages.UTILIZATION_GRAPH),
                cornerPointOffset.x, 0);

        CanvasSettings cs = canvasSettingsMap
                .get(CanvasImages.UTILIZATION_GRAPH);

        // draw the mouse locator line
        if (mouseMarker != MISSING && mouseMarker > cs.getXSpaceBuffer()) {
            gc.drawLine(mouseMarker, 0, mouseMarker, cs.getCanvasHeight());
        }

        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        gc.drawLine(0, 0, cs.getCanvasWidth(), 0);
        gc.drawLine(0, 0, 0, cs.getCanvasHeight());
        gc.drawLine(0, cs.getCanvasHeight() - 1, cs.getCanvasWidth(),
                cs.getCanvasHeight() - 1);
        gc.drawLine(cs.getCanvasWidth() - 1, 0, cs.getCanvasWidth() - 1,
                cs.getCanvasHeight() - 1);

    }

    /**
     * Draw the X header image on the X header canvas.
     * 
     * @param gc
     *            Graphics context.
     */
    private void drawXHeaderCanvas(GC gc) {
        gc.drawImage(imgMap.get(CanvasImages.X_HEADER), 0, 0);
    }

    /**
     * Draw the Y header image on the Y header canvas.
     * 
     * @param gc
     *            Graphics context.
     */
    private void drawYHeaderCanvas(GC gc) {
        gc.drawImage(imgMap.get(CanvasImages.Y_HEADER), 0, 0);
    }

    /**
     * Draw the graph image on the graph canvas.
     * 
     * @param gc
     *            Graphics context.
     */
    private void drawGraphCanvas(GC gc) {
        gc.drawImage(imgMap.get(CanvasImages.GRAPH), cornerPointOffset.x,
                cornerPointOffset.y);

        // draw the mouse locator line
        if (mouseMarker != MISSING
                && mouseMarker > graphCanvasSettings.getXSpaceBuffer()) {
            gc.drawLine(mouseMarker, 0, mouseMarker,
                    graphCanvasSettings.getCanvasHeight());
        }

        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        gc.drawLine(0, 0, graphCanvasSettings.getCanvasWidth(), 0);
        gc.drawLine(0, 0, 0, graphCanvasSettings.getCanvasHeight());
        gc.drawLine(0, graphCanvasSettings.getCanvasHeight() - 1,
                graphCanvasSettings.getCanvasWidth(),
                graphCanvasSettings.getCanvasHeight() - 1);
        gc.drawLine(graphCanvasSettings.getCanvasWidth() - 1, 0,
                graphCanvasSettings.getCanvasWidth() - 1,
                graphCanvasSettings.getCanvasHeight() - 1);
    }

    /**
     * Draw the X label image on the X label canvas.
     * 
     * @param gc
     *            Graphics context.
     */
    private void drawXLabelCanvas(GC gc) {
        gc.drawImage(imgMap.get(CanvasImages.X_LABEL), cornerPointOffset.x, 0);
    }

    /**
     * Draw the Y label image on the Y label canvas.
     * 
     * @param gc
     *            Graphics context.
     */
    private void drawYLabelCanvas(GC gc) {
        CanvasSettings cs = canvasSettingsMap.get(CanvasImages.Y_LABEL);
        gc.drawImage(imgMap.get(CanvasImages.Y_LABEL), 0, cornerPointOffset.y);
        gc.setBackground(display.getSystemColor(SWT.COLOR_BLACK));
        gc.drawLine(0, 0, cs.getCanvasWidth(), 0);
        gc.drawLine(0, 0, 0, cs.getCanvasHeight());
        gc.drawLine(0, cs.getCanvasHeight() - 1, cs.getCanvasWidth(),
                cs.getCanvasHeight() - 1);

    }

    /**
     * Display the tool tip text for the data on the map when the mouse hovers
     * over the data.
     * 
     * @param me
     *            Mouse event
     * @param ci
     *            The canvas image.
     */
    private void displayToolTipText(MouseEvent me, CanvasImages ci) {
        Point mouseCoord = new Point(0, 0);
        mouseCoord.x = Math.abs(cornerPointOffset.x) + me.x;
        mouseCoord.y = Math.abs(cornerPointOffset.y) + me.y;
        canvasMap.get(ci).setToolTipText(
                imageMgr.getToolTipText(mouseCoord, ci));
    }

    /**
     * Get all of the images (graph, x label, y label, x header, and y header).
     */
    private void getAllCanvasImages() {
        imgMap = imageMgr.getAllImages();
    }

    /**
     * Action Handler for mouse mouse click in Y Label canvas
     * 
     * @param me
     *            Mouse Event
     */
    private void handleSubscriptionCheckboxClick(MouseEvent me) {
        Map<Rectangle, String> checkBoxMap = imageMgr.getCheckBoxMap();

        for (Rectangle checkBox : checkBoxMap.keySet()) {
            if (checkBox.contains(me.x, me.y + Math.abs(cornerPointOffset.y))) {
                String name = checkBoxMap.get(checkBox);
                imageMgr.setChecked(name, !imageMgr.isChecked(name));
                break;
            }
        }

        redrawImage(CanvasImages.Y_LABEL);
    }

    /**
     * Handle the mouse event on the X header canvas.
     * 
     * @param me
     *            Mouse event.
     */
    private void handleXHeaderMouseEvent(MouseEvent me) {
        imageMgr.performAction(CanvasImages.X_HEADER, new Point(me.x, me.y));
        redrawImage(CanvasImages.X_HEADER);
        redrawImage(CanvasImages.GRAPH);
    }

    /**
     * Handle the mouse event on the Utilization header canvas.
     * 
     * @param me
     *            Mouse Event
     */
    private void handleUtilizationHeaderMouseEvent(MouseEvent me) {
        imageMgr.performAction(CanvasImages.UTILIZATION_HEADER, new Point(me.x,
                me.y));
        redrawImage(CanvasImages.UTILIZATION_HEADER);
        redrawImage(CanvasImages.UTILIZATION_GRAPH);
    }

    /**
     * Scroll wheel event handler.
     * 
     * @param me
     *            The mouse event
     */
    private void handleScrollWheel(MouseEvent me) {
        if (this.verticalSlider.isEnabled()) {
            this.verticalSlider.setSelection(verticalSlider.getSelection()
                    - me.count * 2);
            handleVerticalScaleChange();
        }
    }

    /**
     * Display right click popup menu for the Y label canvas
     */
    private void yLabelPopupMenu() {
        Menu m = new Menu(this.getShell(), SWT.POP_UP);

        MenuItem selectAll = new MenuItem(m, SWT.NONE);
        selectAll.setText("Select All");
        selectAll.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(Event event) {
                handleSelectAll(true);
            }
        });

        MenuItem deselectAll = new MenuItem(m, SWT.NONE);
        deselectAll.setText("Deselect All");
        deselectAll.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(Event event) {
                handleSelectAll(false);
            }
        });

        new MenuItem(m, SWT.SEPARATOR);

        MenuItem viewSubs = new MenuItem(m, SWT.NONE);
        viewSubs.setText("View Selected Subscriptions...");
        viewSubs.setEnabled(imageMgr.hasSubscriptionNameChecked());
        viewSubs.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(Event event) {
                handleViewSubscriptions();
            }
        });

        // We need to make the menu visible
        m.setVisible(true);
    }

    /**
     * Display the graph right click pop up menu.
     */
    private void graphPopupMenu() {
        boolean hasSelection = imageMgr.hasSelection(CanvasImages.GRAPH);

        Menu m = new Menu(this.getShell(), SWT.POP_UP);

        MenuItem selectedMenu = new MenuItem(m, SWT.CASCADE);
        selectedMenu.setText("Sort by Selected Start Time");

        Menu selectSortMenu = new Menu(m);
        selectedMenu.setMenu(selectSortMenu);

        MenuItem sortSelectedIntersect = new MenuItem(selectSortMenu, SWT.NONE);
        sortSelectedIntersect.setText("Intersect Start Time");
        sortSelectedIntersect.setEnabled(hasSelection);
        sortSelectedIntersect.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(Event event) {
                handleSort(SortBy.SELECTED_INTERSECT);
            }
        });

        MenuItem sortSelectedStart = new MenuItem(selectSortMenu, SWT.NONE);
        sortSelectedStart.setText("Upcoming Start Time");
        sortSelectedStart.setEnabled(hasSelection);
        sortSelectedStart.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(Event event) {
                handleSort(SortBy.SELECTED_START);
            }
        });

        MenuItem sortCurrent = new MenuItem(m, SWT.NONE);
        sortCurrent.setText("Sort by Current Time");
        sortCurrent.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(Event event) {
                handleSort(SortBy.CURRENT_TIME);
            }
        });

        MenuItem nameMenu = new MenuItem(m, SWT.CASCADE);
        nameMenu.setText("Sort by Subscription Name");

        Menu nameSortMenu = new Menu(m);
        nameMenu.setMenu(nameSortMenu);

        MenuItem ascending = new MenuItem(nameSortMenu, SWT.NONE);
        ascending.setText("Ascending");
        ascending.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(Event event) {
                handleSort(SortBy.NAME_ASC);
            }
        });

        MenuItem descending = new MenuItem(nameSortMenu, SWT.NONE);
        descending.setText("Descending");
        descending.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(Event event) {
                handleSort(SortBy.NAME_DESC);
            }
        });

        // We need to make the menu visible
        m.setVisible(true);
    }

    private void bandwidthUsedPopupMenu() {
        if (bandwidthPopupMenu == null) {
            bandwidthPopupMenu = new Menu(this.getShell(), SWT.POP_UP);

            MenuItem lineMenu = new MenuItem(bandwidthPopupMenu, SWT.RADIO);
            lineMenu.setText("Display as Line Graph");
            lineMenu.setSelection(true);
            lineMenu.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    handleBandwidthGraphStyleSelection(GraphType.LINE);
                }
            });

            MenuItem barMenu = new MenuItem(bandwidthPopupMenu, SWT.RADIO);
            barMenu.setText("Display as Bar Graph");
            barMenu.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    handleBandwidthGraphStyleSelection(GraphType.BAR);
                }
            });
        }
        bandwidthPopupMenu.setVisible(true);
    }

    protected void handleBandwidthGraphStyleSelection(GraphType type) {
        imageMgr.setBandwidthGraphType(type);
        redrawImage(CanvasImages.UTILIZATION_GRAPH);
    }

    /**
     * Select/Deselect all event handler.
     * 
     * @param selectAll
     *            true to select all
     */
    private void handleSelectAll(boolean selectAll) {
        Map<Rectangle, String> checkBoxMap = imageMgr.getCheckBoxMap();

        for (Rectangle checkBox : checkBoxMap.keySet()) {
            String name = checkBoxMap.get(checkBox);
            imageMgr.setChecked(name, selectAll);
        }

        redrawImage(CanvasImages.Y_LABEL);
    }

    /**
     * Event handler for view subscription menu
     */
    private void handleViewSubscriptions() {
        List<String> viewList = new ArrayList<String>();
        Map<Rectangle, String> checkBoxMap = imageMgr.getCheckBoxMap();
        for (Rectangle checkBox : checkBoxMap.keySet()) {
            String name = checkBoxMap.get(checkBox);
            if (imageMgr.isChecked(name)) {
                viewList.add(name);
            }
        }

        if (!viewList.isEmpty()) {
            SubscriptionViewer viewer = new SubscriptionViewer(this.getShell(),
                    viewList, this);
            viewer.open();
        } else {
            DataDeliveryUtils
                    .showMessage(
                            this.getShell(),
                            SWT.ICON_INFORMATION,
                            "No Selections",
                            "No subscriptions selected.\n\nTo select a subscription click the checkbox next to the subscription name");
        }
    }

    /**
     * Event handler for the sort calls.
     * 
     * @param sortBy
     *            sort scheme
     */
    private void handleSort(SortBy sortBy) {
        if (sortBy != SortBy.SELECTED_INTERSECT
                && sortBy != SortBy.SELECTED_START) {
            imageMgr.clearCanvasSelection(CanvasImages.GRAPH);
        }
        imageMgr.setSortBy(sortBy);
        redrawImage(CanvasImages.GRAPH);
        redrawImage(CanvasImages.Y_LABEL);
        redrawImage(CanvasImages.X_LABEL);
        redrawImage(CanvasImages.X_HEADER);
    }

    /**
     * Redraw the image.
     * 
     * @param ci
     *            The image to redraw
     */
    protected void redrawImage(CanvasImages ci) {
        imageMgr.regenerateImage(ci);
        imgMap.put(ci, imageMgr.getImage(ci));
        canvasMap.get(ci).redraw();
    }

    /**
     * Set the live update flag.
     * 
     * @param liveUpdate
     *            true to auto-update
     */
    public void setLiveUpdate(boolean liveUpdate) {
        imageMgr.setLiveUpdate(liveUpdate);
        if (liveUpdate) {
            fullUpdateMinuteCount = 0;
            graphDataUtil.requestGraphDataUsingThread();
            if (timer.isShutdown() || timer.isTerminated()) {
                createTimer();
            }
        } else {
            timer.shutdown();
        }
        updateCanvases();
    }

    /**
     * Set the graph data, recalculate the graph sizes to compensate for any
     * subscription additions or deletions, and redraw the images.
     * 
     * @param graphData
     *            Bandwidth graph data.
     */
    public void setGraphData(BandwidthGraphData graphData) {
        this.bgd = graphData;

        generateCanvasSettings();

        for (Entry<CanvasImages, CanvasSettings> entry : canvasSettingsMap
                .entrySet()) {
            imageMgr.setCanvasSetting(entry.getKey(), entry.getValue());
        }

        imageMgr.generateImages(bgd);
        updateCanvases();
    }

    /**
     * Set the color by priority flag.
     * 
     * @param colorByPriority
     *            true to color by priority
     */
    public void setColorByPriority(boolean colorByPriority) {
        imageMgr.setColorByPriority(colorByPriority);
        redrawImage(CanvasImages.GRAPH);
        redrawImage(CanvasImages.UTILIZATION_GRAPH);
    }

    /**
     * Set the show subscription lines flag.
     * 
     * @param showSubLines
     *            True to show the subscription lines
     */
    public void setShowSubscriptionLines(boolean showSubLines) {
        imageMgr.setShowSubscriptionLines(showSubLines);
        imageMgr.regenerateImage(CanvasImages.GRAPH);
        redrawImage(CanvasImages.GRAPH);
        redrawImage(CanvasImages.X_LABEL);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void dialogClosed(String id) {
        // Do Nothing
    }

    /**
     * Update the canvases.
     */
    public void updateCanvases() {
        if (cornerPointOffset.x < 0 - graphCanvasSettings.getImageWidth()
                + graphCanvasSettings.getCanvasWidth()) {
            cornerPointOffset.x = 0 - graphCanvasSettings.getImageWidth()
                    + graphCanvasSettings.getCanvasWidth();
        }

        if (cornerPointOffset.x > 0) {
            cornerPointOffset.x = 0;
        }

        if (cornerPointOffset.y < 0 - graphCanvasSettings.getImageHeight()
                + graphCanvasSettings.getCanvasHeight()) {
            cornerPointOffset.y = 0 - graphCanvasSettings.getImageHeight()
                    + graphCanvasSettings.getCanvasHeight();
        }

        if (cornerPointOffset.y > 0) {
            cornerPointOffset.y = 0;
        }

        verticalSlider.setSelection(cornerPointOffset.y * -1);
        horizontalSlider.setSelection(cornerPointOffset.x * -1);

        canvasMap.get(CanvasImages.GRAPH).redraw();
        canvasMap.get(CanvasImages.X_LABEL).redraw();
        canvasMap.get(CanvasImages.Y_LABEL).redraw();
        canvasMap.get(CanvasImages.UTILIZATION_GRAPH).redraw();

        for (CanvasImages ci : CanvasImages.values()) {
            redrawImage(ci);
        }
    }

    /**
     * Live update inner class
     */
    private class UpdateTask extends TimerTask {
        @Override
        public void run() {
            currentTime.setTimeInMillis(TimeUtil.currentTimeMillis());
            if (currentTime.get(Calendar.MINUTE) == 0 && previousMinute != 0) {
                previousMinute = -1;
            }
            if (currentTime.get(Calendar.MINUTE) > previousMinute) {
                fullUpdateMinuteCount++;
                previousMinute = currentTime.get(Calendar.MINUTE);
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        imageMgr.setCurrentTimeMillis(currentTime
                                .getTimeInMillis());
                        redrawImage(CanvasImages.GRAPH);
                        redrawImage(CanvasImages.X_LABEL);
                        redrawImage(CanvasImages.UTILIZATION_GRAPH);
                        canvasMap.get(CanvasImages.GRAPH).redraw();
                        canvasMap.get(CanvasImages.X_LABEL).redraw();
                        canvasMap.get(CanvasImages.UTILIZATION_GRAPH);

                    }
                });

                // Do a full update every 10 minutes
                if (fullUpdateMinuteCount > 10) {
                    graphDataUtil.requestGraphDataUsingThread();
                }
            }
        }
    }

    /**
     * This method will update the subscription table with any updates,
     * deletions, or new subscriptions.
     */
    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        try {
            for (NotificationMessage msg : messages) {
                Object obj = msg.getMessagePayload();
                if (obj instanceof SubscriptionNotificationResponse) {
                    graphDataUtil.requestGraphDataUsingThread();
                }
            }
        } catch (NotificationException e) {
            statusHandler.error("Error when receiving notification", e);
        }
    }

    /**
     * This will update the graph as the data has been updated.
     */
    @Override
    public void dataUpdated() {
        fullUpdateMinuteCount = 0;
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                setGraphData(graphDataUtil.getGraphData(false));
            }
        });
    }

    /**
     * Set the network for display.
     * 
     * @param network
     *            The network to display
     */
    public void setGraphNetwork(Network network) {
        imageMgr.setGraphNetwork(network);
        redrawImage(CanvasImages.GRAPH);
        redrawImage(CanvasImages.UTILIZATION_GRAPH);
        redrawImage(CanvasImages.UTILIZATION_HEADER);
        redrawImage(CanvasImages.UTILIZATION_LABEL);
        redrawImage(CanvasImages.X_HEADER);
        redrawImage(CanvasImages.X_LABEL);
        redrawImage(CanvasImages.Y_HEADER);
        redrawImage(CanvasImages.Y_LABEL);
    }

    /**
     * Set the bandwidth used threshold values.
     * 
     * @param thresholdValues
     *            The threshold values
     */
    public void setBandwidthThresholdValues(int[] thresholdValues) {
        imageMgr.setBandwidthThreholdValues(thresholdValues);
    }

    /**
     * Get the bandwidth used threshold values.
     * 
     * @return thresholdValues The threshold values
     */
    public int[] getBandwidthThresholdValues() {
        return imageMgr.getBandwidthThreholdValues();
    }

    /**
     * Get the bandwidth threshold colors.
     * 
     * @return Threshold colors
     */
    public Map<GraphSection, RGB> getBandwidthThresholdColors() {
        return imageMgr.getPercentageColorMap();
    }

    /**
     * Update the canvas settings.
     */
    private void updateCanvasSettings() {
        if (this.getBounds().x != 0) {
            int compHeight = this.getBounds().height;
            int compWidth = this.getBounds().width;

            int graphCanvasWidth = compWidth - vSliderComp.getBounds().width
                    - getCanvasWidth(CanvasImages.Y_HEADER)
                    - getCanvasWidth(CanvasImages.Y_LABEL);

            int graphCanvasHeight = compHeight - hSliderComp.getBounds().height
                    - getCanvasHeight(CanvasImages.X_HEADER)
                    - getCanvasHeight(CanvasImages.X_LABEL)
                    - getCanvasHeight(CanvasImages.UTILIZATION_HEADER)
                    - getCanvasHeight(CanvasImages.UTILIZATION_GRAPH);

            // X Header canvas
            CanvasSettings settings = this
                    .getCanvasSettings(CanvasImages.X_HEADER);
            int xHeaderHeight = settings.getCanvasHeight();

            settings.updateCanvas(graphCanvasWidth, xHeaderHeight, graphSize.x,
                    graphSize.y);
            ((GridData) xHeaderCanvas.getLayoutData()).widthHint = graphCanvasWidth;
            ((GridData) xHeaderCanvas.getLayoutData()).heightHint = xHeaderHeight;
            xHeaderCanvas.setSize(graphCanvasWidth, xHeaderHeight);

            // X Label Canvas
            settings = this.getCanvasSettings(CanvasImages.X_LABEL);
            int xLabelHeight = settings.getCanvasHeight();
            settings.updateCanvas(graphCanvasWidth, xLabelHeight, graphSize.x,
                    graphSize.y);

            ((GridData) xLabelCanvas.getLayoutData()).widthHint = graphCanvasWidth;
            ((GridData) xLabelCanvas.getLayoutData()).heightHint = xLabelHeight;
            xLabelCanvas.setSize(graphCanvasWidth, graphCanvasHeight);

            imageMgr.setCanvasSetting(CanvasImages.X_LABEL, settings);

            // y Header Canvas
            settings = this.getCanvasSettings(CanvasImages.Y_HEADER);
            settings.updateCanvas(35, graphCanvasHeight, graphSize.x,
                    graphSize.y);

            ((GridData) yHeaderCanvas.getLayoutData()).widthHint = 35;
            ((GridData) yHeaderCanvas.getLayoutData()).heightHint = graphCanvasHeight;
            yHeaderCanvas.setSize(graphCanvasWidth, graphCanvasHeight);

            imageMgr.setCanvasSetting(CanvasImages.Y_HEADER, settings);

            // y Label Canvas
            settings = this.getCanvasSettings(CanvasImages.Y_LABEL);
            settings.updateCanvas(yLabelWidth, graphCanvasHeight, graphSize.x,
                    graphSize.y);

            ((GridData) yLabelCanvas.getLayoutData()).widthHint = yLabelWidth;
            ((GridData) yLabelCanvas.getLayoutData()).heightHint = graphCanvasHeight;
            yLabelCanvas.setSize(yLabelWidth, xHeaderHeight);

            imageMgr.setCanvasSetting(CanvasImages.Y_LABEL, settings);

            // Graph canvas
            settings = this.getCanvasSettings(CanvasImages.GRAPH);
            settings.updateCanvas(graphCanvasWidth, graphCanvasHeight,
                    graphSize.x, graphSize.y);

            ((GridData) graphCanvas.getLayoutData()).widthHint = graphCanvasWidth;
            ((GridData) graphCanvas.getLayoutData()).heightHint = graphCanvasHeight;
            graphCanvas.setSize(graphCanvasWidth, graphCanvasHeight);

            imageMgr.setCanvasSetting(CanvasImages.GRAPH, settings);

            horizontalSlider.setMaximum(settings.getImageWidth()
                    - graphCanvasWidth + horizontalSlider.getThumb());

            verticalSlider.setMaximum(settings.getImageHeight()
                    - graphCanvasHeight + verticalSlider.getThumb());

            // Utilization header
            settings = this.getCanvasSettings(CanvasImages.UTILIZATION_HEADER);
            settings.updateCanvas(graphCanvasWidth, utilizationHeaderHeight,
                    graphCanvasWidth, utilizationHeaderHeight);

            ((GridData) utilizationHeaderCanvas.getLayoutData()).widthHint = graphCanvasWidth;
            ((GridData) utilizationHeaderCanvas.getLayoutData()).heightHint = utilizationHeaderHeight;
            utilizationHeaderCanvas
                    .setSize(graphCanvasWidth, graphCanvasHeight);
            imageMgr.setCanvasSetting(CanvasImages.UTILIZATION_HEADER, settings);

            // Utilization Graph
            settings = this.getCanvasSettings(CanvasImages.UTILIZATION_GRAPH);
            settings.updateCanvas(graphCanvasWidth, utilizationGraphHeight,
                    graphSize.x, utilizationGraphHeight);

            ((GridData) utilizationGraphCanvas.getLayoutData()).widthHint = graphCanvasWidth;
            ((GridData) utilizationGraphCanvas.getLayoutData()).heightHint = utilizationGraphHeight;
            utilizationGraphCanvas.setSize(graphCanvasWidth,
                    utilizationGraphHeight);
            imageMgr.setCanvasSetting(CanvasImages.UTILIZATION_GRAPH, settings);

            imageMgr.updateImageMap(canvasSettingsMap);
        }
    }

    /**
     * Get the canvas width.
     * 
     * @param image
     *            the image
     * 
     * @return the canvas' width
     */
    private int getCanvasWidth(CanvasImages image) {
        return canvasSettingsMap.get(image).getCanvasWidth();
    }

    /**
     * Get the canvas height.
     * 
     * @param image
     *            the image
     * 
     * @return the canvas' height
     */
    private int getCanvasHeight(CanvasImages image) {
        return canvasSettingsMap.get(image).getCanvasHeight();
    }

    /**
     * Get the canvas settings.
     * 
     * @param image
     *            The CanvasImage
     * 
     * @return the CanvasSettings object for the image
     */
    private CanvasSettings getCanvasSettings(CanvasImages image) {
        return canvasSettingsMap.get(image);
    }
}
