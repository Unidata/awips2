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
package com.raytheon.viz.gfe.dialogs;

import java.text.DecimalFormat;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Coordinate Conversion Dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 13, 2014  #3069     randerso    Added to help debug GeoTools 10.5 issues.
 *                                     May partially address Dimensions DR 15463.
 *                                     plugin.xml menu entry comnmented out so can't be activated
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class CoordinateConversionDialog extends CaveJFACEDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CoordinateConversionDialog.class);

    private DataManager dataMgr;

    private GridLocation gloc;

    private ProjectionData awipsProj;

    private DecimalFormat df;

    private Text lonText;

    private Text latText;

    private Text awipsXText;

    private Text awipsYText;

    private Text gfeXText;

    private Text gfeYText;

    /**
     * @param parentShell
     * @param dataMgr
     */
    public CoordinateConversionDialog(Shell parentShell, DataManager dataMgr) {
        super(parentShell);
        this.dataMgr = dataMgr;

        gloc = this.dataMgr.getParmManager().compositeGridLocation();
        awipsProj = gloc.getProjection();

        df = new DecimalFormat("###0.####;-###0.####");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Coordinate Conversion");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        Composite comp = (Composite) super.createDialogArea(parent);
        ((GridLayout) comp.getLayout()).numColumns = 3;

        // listener to validate numeric entry and update computed fields
        KeyListener keyListener = new KeyListener() {

            @Override
            public void keyReleased(KeyEvent e) {
                if (e.character != '.') {
                    Object source = e.getSource();
                    if ((source == lonText) || (source == latText)) {
                        updateLonLat();
                    } else if ((source == awipsXText) || (source == awipsYText)) {
                        updateAwips();
                    } else if ((source == gfeXText) || (source == gfeYText)) {
                        updateGfe();
                    }
                }
            }

            @Override
            public void keyPressed(KeyEvent e) {
                if (!Character.isISOControl(e.character)) {
                    Text source = (Text) e.getSource();
                    String text = source.getText();
                    Point selection = source.getSelection();
                    text = text.substring(0, selection.x) + e.character
                            + text.substring(selection.y);
                    try {
                        Double.parseDouble(text);
                    } catch (NumberFormatException e1) {
                        e.doit = false;
                        getShell().getDisplay().beep();
                    }
                }
            }
        };

        Label label = new Label(comp, SWT.RIGHT);
        GridData layoutData = new GridData(SWT.FILL, SWT.CENTER, true, false);
        label.setLayoutData(layoutData);
        label.setText("Lon/Lat");

        lonText = new Text(comp, SWT.RIGHT | SWT.SINGLE | SWT.BORDER);
        layoutData = new GridData(SWT.FILL, SWT.CENTER, true, false);
        lonText.setLayoutData(layoutData);
        lonText.addKeyListener(keyListener);

        latText = new Text(comp, SWT.RIGHT | SWT.SINGLE | SWT.BORDER);
        layoutData = new GridData(SWT.FILL, SWT.CENTER, true, false);
        latText.setLayoutData(layoutData);
        latText.addKeyListener(keyListener);

        label = new Label(comp, SWT.RIGHT);
        layoutData = new GridData(SWT.FILL, SWT.CENTER, true, false);
        label.setLayoutData(layoutData);
        label.setText(gloc.getProjection().getProjectionID());

        awipsXText = new Text(comp, SWT.RIGHT | SWT.SINGLE | SWT.BORDER);
        layoutData = new GridData(SWT.FILL, SWT.CENTER, true, false);
        awipsXText.setLayoutData(layoutData);
        awipsXText.addKeyListener(keyListener);

        awipsYText = new Text(comp, SWT.RIGHT | SWT.SINGLE | SWT.BORDER);
        layoutData = new GridData(SWT.FILL, SWT.CENTER, true, false);
        awipsYText.setLayoutData(layoutData);
        awipsYText.addKeyListener(keyListener);

        label = new Label(comp, SWT.RIGHT);
        layoutData = new GridData(SWT.FILL, SWT.CENTER, true, false);
        label.setLayoutData(layoutData);
        label.setText(gloc.getSiteId());

        gfeXText = new Text(comp, SWT.RIGHT | SWT.SINGLE | SWT.BORDER);
        layoutData = new GridData(SWT.FILL, SWT.CENTER, true, false);
        gfeXText.setLayoutData(layoutData);
        gfeXText.addKeyListener(keyListener);

        gfeYText = new Text(comp, SWT.RIGHT | SWT.SINGLE | SWT.BORDER);
        layoutData = new GridData(SWT.FILL, SWT.CENTER, true, false);
        gfeYText.setLayoutData(layoutData);
        gfeYText.addKeyListener(keyListener);

        gfeXText.setText("0");
        gfeYText.setText("0");

        updateGfe();

        return comp;
    }

    private double getDoubleValue(Text text) {
        String s = text.getText();
        if (s.isEmpty()) {
            return 0;
        } else {
            return Double.parseDouble(s);
        }
    }

    private void updateLonLat() {
        double lon = getDoubleValue(lonText);
        double lat = getDoubleValue(latText);
        Coordinate lonLat = new Coordinate(lon, lat);
        // lonText.setText(df.format(lon));
        // latText.setText(df.format(lat));

        try {
            Coordinate gridCell = MapUtil.latLonToGridCoordinate(lonLat,
                    PixelOrientation.CENTER, gloc);
            gfeXText.setText(df.format(gridCell.x));
            gfeYText.setText(df.format(gloc.getNy() - gridCell.y - 1));
        } catch (Exception e) {
            statusHandler.error(e.getLocalizedMessage(), e);
            gfeXText.setText("");
            gfeYText.setText("");
        }

        try {
            Coordinate awips = awipsProj.latLonToGridCoordinate(lonLat);
            awipsXText.setText(df.format(awips.x));
            awipsYText.setText(df.format(awips.y));
        } catch (Exception e) {
            statusHandler.error(e.getLocalizedMessage(), e);
            awipsXText.setText("");
            awipsYText.setText("");
        }
    }

    private void updateAwips() {
        double x = getDoubleValue(awipsXText);
        double y = getDoubleValue(awipsYText);
        // awipsXText.setText(df.format(x));
        // awipsYText.setText(df.format(y));

        Coordinate awips = new Coordinate(x, y);
        Coordinate lonLat;
        try {
            lonLat = awipsProj.gridCoordinateToLatLon(awips);
            lonText.setText(df.format(lonLat.x));
            latText.setText(df.format(lonLat.y));

            try {
                Coordinate gridCell = MapUtil.latLonToGridCoordinate(lonLat,
                        PixelOrientation.CENTER, gloc);
                gfeXText.setText(df.format(gridCell.x));
                gfeYText.setText(df.format(gloc.getNy() - gridCell.y - 1));
            } catch (Exception e) {
                statusHandler.error(e.getLocalizedMessage(), e);
                gfeXText.setText("");
                gfeYText.setText("");
            }
        } catch (Exception e) {
            statusHandler.error(e.getLocalizedMessage(), e);
            lonText.setText("");
            latText.setText("");
            gfeXText.setText("");
            gfeYText.setText("");
        }
    }

    private void updateGfe() {
        double x = getDoubleValue(gfeXText);
        double y = getDoubleValue(gfeYText);
        Coordinate gridCell = new Coordinate(x, gloc.getNy() - y - 1);
        // gfeXText.setText(df.format(x));
        // gfeYText.setText(df.format(y));

        Coordinate lonLat = gloc.latLonCenter(gridCell);
        lonText.setText(df.format(lonLat.x));
        latText.setText(df.format(lonLat.y));

        try {
            Coordinate awips = awipsProj.latLonToGridCoordinate(lonLat);
            awipsXText.setText(df.format(awips.x));
            awipsYText.setText(df.format(awips.y));
        } catch (Exception e) {
            statusHandler.error(e.getLocalizedMessage(), e);
            awipsXText.setText("");
            awipsYText.setText("");
        }
    }
}
