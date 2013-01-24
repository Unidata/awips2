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
package com.raytheon.viz.radar.ui.xy;

import java.awt.Font;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.GSMBlock.GSMMessage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.viz.awipstools.capabilities.RangeRingsOverlayCapability;
import com.raytheon.viz.radar.RadarHelper;
import com.raytheon.viz.radar.interrogators.IRadarInterrogator;
import com.raytheon.viz.radar.rsc.AbstractRadarResource;
import com.raytheon.viz.radar.rsc.RadarResourceData;

/**
 * For the General Status Message product
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 13, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RadarGSMResource extends AbstractRadarResource<RadarXYDescriptor> {

    RGB color;

    /**
     * @param resourceData
     * @param loadProperties
     * @throws VizException
     */
    public RadarGSMResource(RadarResourceData resourceData,
            LoadProperties loadProperties, IRadarInterrogator interrogator)
            throws VizException {
        super(resourceData, loadProperties, interrogator);
        getCapability(ColorableCapability.class).setColor(new RGB(0, 255, 0));

        // remove the uneeded capabilities
        getCapabilities().removeCapability(ImagingCapability.class);
        getCapabilities().removeCapability(RangeRingsOverlayCapability.class);

        color = getCapability(ColorableCapability.class).getColor();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.ui.xy.RadarXYResource#paintInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        super.paintInternal(target, paintProps);
        color = getCapability(ColorableCapability.class).getColor();
        RadarRecord record = getRadarRecord(paintProps.getDataTime());
        if (record == null) {
            return;
        }
        GSMMessage message = record.getGsmMessage();
        if (message != null) {
            String rpgNarrow = "";
            String dedicatedComms = "";
            String rdaOpStatusString = "";
            String rdaAvailString = "";
            String rdaStatusString = "";
            int xOffset = 15;
            int yOffset = 30;
            int lineSpace = 25;
            int boxHeight = 120;
            int halfHeight = boxHeight / 2;
            boolean rdaDown = false, rpgDown = false;
            String temp = new String();
            descriptor.pixelToWorld(new double[] { 700, 700 });
            String title = "NEXRAD UNIT STATUS";
            String rpg_spg = "RPG";
            String rda_tdwr = "RDA";
            int vcp = message.getVolumeCoveragePattern();

            rdaStatusString = RadarHelper.formatBits(
                    (short) message.getRdaStatus(), RadarHelper.rdaStatusStr);

            if (vcp == 80 || vcp == 90) {
                title = "TDWR SPG UNIT STATUS";
                rpg_spg = "SPG";
                rda_tdwr = "TDWR";
            }
            target.drawString(null, title, 400, yOffset, 0, TextStyle.BOXED,
                    color, HorizontalAlignment.CENTER, VerticalAlignment.TOP,
                    0.0d);
            yOffset += 40;
            switch (message.getMode()) {
            case 0:
                temp = "Maintenance Mode";
                break;
            case 1:
                temp = "Clear Air Mode";
                break;
            case 2:
                temp = "Precipitation/Severe Weather Mode";
                break;
            }
            drawNexradString(
                    "Op Mode/VCP = " + temp + "/VCP"
                            + message.getVolumeCoveragePattern(), xOffset,
                    yOffset, target, color);

            yOffset += lineSpace;
            rdaAvailString = RadarHelper.formatBits(
                    (short) message.getProductAvail(),
                    RadarHelper.productAvailStr);
            drawNexradString("New Prod Status = " + rdaAvailString, xOffset,
                    yOffset, target, color);
            yOffset += lineSpace;
            drawNexradString(
                    "Base Data = "
                            + RadarHelper.formatBits(
                                    (short) message.getDataTransmissionEnable(),
                                    RadarHelper.dteStr), xOffset, yOffset,
                    target, color);
            yOffset += lineSpace;
            if (message.getCmdStatus() > 0)
                drawNexradString("CMD = Enabled", xOffset, yOffset, target,
                        color);
            else
                drawNexradString("CMD = Disabled", xOffset, yOffset, target,
                        color);
            yOffset += lineSpace;
            if ("Operate".equals(RadarHelper.formatBits(
                    (short) message.getRpgStatus(), RadarHelper.rpgStatusStr))) {
                dedicatedComms = "Connected";
            } else {
                dedicatedComms = "Disconnected";
            }
            drawNexradString("Ded RPG Comms = " + dedicatedComms, xOffset,
                    yOffset, target, color);
            yOffset += lineSpace;
            drawNexradString(
                    "RPG Avail = "
                            + RadarHelper.formatBits(
                                    (short) message.getRpgOperability(),
                                    RadarHelper.rpgOpStr), xOffset, yOffset,
                    target, color);
            yOffset += lineSpace;
            rpgNarrow = RadarHelper.formatBits(
                    (short) message.getRpgNarrowbandStatus(),
                    RadarHelper.rpgNarrowbandStatus);
            if ("".equals(temp)) {
                rpgNarrow = "Normal";
            }
            drawNexradString("RPG Narrowband = " + rpgNarrow, xOffset, yOffset,
                    target, color);
            yOffset += lineSpace;

            drawNexradString(
                    "RPG Software = "
                            + RadarHelper.formatBits(
                                    (short) message.getRpgStatus(),
                                    RadarHelper.rpgStatusStr), xOffset,
                    yOffset, target, color);
            yOffset += lineSpace;

            DecimalFormat df = new DecimalFormat("0.00");
            final int DUAL_POL_FLAG = 0x20;
            if ((message.getDataTransmissionEnable() & DUAL_POL_FLAG) != 0)
                drawNexradString(
                        "Delta Sys Cal: "
                                + df.format(message.getReflectCalibCorr())
                                + "H, "
                                + df.format(message.getReflectCalibCorr2())
                                + "V", xOffset, yOffset, target, color);
            else
                drawNexradString(
                        "Delta Sys Cal: "
                                + df.format(message.getReflectCalibCorr()),
                        xOffset, yOffset, target, color);
            yOffset += lineSpace;

            drawNexradString(
                    "RPG Alarm = "
                            + RadarHelper.formatBits(
                                    (short) message.getRpgAlarms(),
                                    RadarHelper.rpgAlarmStr), xOffset, yOffset,
                    target, color);
            yOffset += lineSpace;

            drawNexradString(
                    "RDA Avail = "
                            + RadarHelper.formatBits(
                                    (short) message.getOperabilityStatus(),
                                    RadarHelper.rdaOpStatusStr), xOffset,
                    yOffset, target, color);
            yOffset += lineSpace;
            rdaOpStatusString = RadarHelper.formatBits(
                    (short) message.getRdaStatus(), RadarHelper.rdaStatusStr);
            drawNexradString("RDA Software = " + rdaOpStatusString, xOffset,
                    yOffset, target, color);
            yOffset += lineSpace;
            temp = RadarHelper.formatBits((short) message.getRdaAlarms(),
                    RadarHelper.rdaAlarmStr);
            if ("".equals(temp)) {
                temp = "No Data";
            }

            drawNexradString("RDA Alarm = " + temp, xOffset, yOffset, target,
                    color);
            yOffset += lineSpace;
            drawNexradString(
                    "RDA Version = "
                            + String.valueOf((double) message.getRdaBuildNum() / 10),
                    xOffset, yOffset, target, color);
            yOffset += lineSpace;
            if (vcp == 80 || vcp == 90)
                drawNexradString(
                        "SPG Version = "
                                + String.valueOf((double) message
                                        .getBuildVersion() / 10), xOffset,
                        yOffset, target, color);
            else {
                drawNexradString(
                        "RDA Channel = "
                                + RadarHelper.getRdaChannelName(message
                                        .getRdaChannelNum()), xOffset, yOffset,
                        target, color);
                yOffset += lineSpace;
                drawNexradString(
                        "RPG Version = "
                                + String.valueOf((double) message
                                        .getBuildVersion() / 10), xOffset,
                        yOffset, target, color);
            }

            // Plot elevations
            double[] elevations = message.getElevation().clone();
            char[] charArray = Integer.toBinaryString(
                    message.getSuperResolutionCuts()).toCharArray();

            elevations = Arrays.copyOf(elevations, message.getNumCuts());
            Arrays.sort(elevations);
            for (int left = 0, right = elevations.length - 1; left < right; left++, right--) {
                double tmp = elevations[left];
                elevations[left] = elevations[right];
                elevations[right] = tmp;
            }

            int count = 0;
            for (int i = 0; i < elevations.length; i++) {
                if (elevations[i] == 0) {
                    count++;
                }
            }

            boolean[] superResElev = new boolean[elevations.length];
            for (int i = 0; i < charArray.length; i++) {
                if (charArray[i] == '1') {
                    superResElev[i] = true;
                } else {
                    superResElev[i] = false;
                }
            }

            List<String> theTemp = new ArrayList<String>();
            for (int i = 0; i < elevations.length; i++) {
                if (elevations[i] != 0) {
                    String s = "";
                    if (superResElev[elevations.length - i - count - 1] == true) {
                        s = "S";
                    } else {
                        s = "";
                    }
                    theTemp.add(Double.toString(elevations[i] / 10) + "     "
                            + s);
                } else {
                    theTemp.add(Double.toString(elevations[i] / 10));
                    break;
                }
            }

            int height = 780;
            for (int i = 0; i < theTemp.size(); i++) {
                target.drawLine(xOffset + 50, height, 0, 800, height - i
                        * lineSpace, 0, color, 1);
                drawNexradString(
                        String.valueOf(theTemp.get(theTemp.size() - 1 - i)),
                        800, height - i * lineSpace - 10, target, color);
            }
            yOffset = height - theTemp.size() * lineSpace - lineSpace;
            drawNexradString("Elevations", 780, yOffset, target, color);

            yOffset = height + lineSpace;
            // first box
            target.drawLine(xOffset, yOffset, 0, xOffset + 200, yOffset, 0,
                    color, 1);
            target.drawLine(xOffset, yOffset, 0, xOffset, yOffset + boxHeight,
                    0, color, 1);
            target.drawLine(xOffset + 200, yOffset, 0, xOffset + 200, yOffset
                    + boxHeight, 0, color, 1);
            target.drawLine(xOffset, yOffset + boxHeight, 0, xOffset + 200,
                    yOffset + boxHeight, 0, color, 1);
            drawNexradString(rda_tdwr, xOffset + 85, yOffset + halfHeight,
                    target, color);

            final int RDA_OP_STATUS_WIDEBAND_DISCONNECT = 1 << 7;
            final int RDA_STATUS_STANDBY = 1 << 2;
            final int RDA_STATUS_RESTART = 1 << 3;
            final int RDA_STATUS_OFFLINE = 1 << 6;
            if ((message.getOperabilityStatus() & RDA_OP_STATUS_WIDEBAND_DISCONNECT) != 0
                    || (message.getRdaStatus() & RDA_STATUS_RESTART) != 0
                    || (message.getRdaStatus() & RDA_STATUS_STANDBY) != 0
                    || (message.getRdaStatus() & RDA_STATUS_OFFLINE) != 0)
                rdaDown = true;
            if (!rdaDown) {
                target.drawLine(xOffset + 200, yOffset + halfHeight, 0,
                        xOffset + 300, yOffset + halfHeight, 0, color, 1);
                target.drawLine(xOffset + 300, yOffset + halfHeight, 0,
                        xOffset + 280, yOffset + halfHeight - 10, 0, color, 1);
                target.drawLine(xOffset + 300, yOffset + halfHeight, 0,
                        xOffset + 280, yOffset + halfHeight + 10, 0, color, 1);
            }

            xOffset += 300;
            // second box
            target.drawLine(xOffset, yOffset, 0, xOffset + 200, yOffset, 0,
                    color, 1);
            target.drawLine(xOffset, yOffset, 0, xOffset, yOffset + boxHeight,
                    0, color, 1);
            target.drawLine(xOffset + 200, yOffset, 0, xOffset + 200, yOffset
                    + boxHeight, 0, color, 1);
            target.drawLine(xOffset, yOffset + boxHeight, 0, xOffset + 200,
                    yOffset + boxHeight, 0, color, 1);
            drawNexradString(rpg_spg, xOffset + 85, yOffset + halfHeight,
                    target, color);

            if (rpgNarrow.equals("Commanded Disconnect")
                    || dedicatedComms.equals("Disconnected"))
                rpgDown = true;
            if (!rpgDown) {
                target.drawLine(xOffset + 200, yOffset + halfHeight, 0,
                        xOffset + 300, yOffset + halfHeight, 0, color, 1);
                target.drawLine(xOffset + 300, yOffset + halfHeight, 0,
                        xOffset + 280, yOffset + halfHeight - 10, 0, color, 1);
                target.drawLine(xOffset + 300, yOffset + halfHeight, 0,
                        xOffset + 280, yOffset + halfHeight + 10, 0, color, 1);

                target.drawLine(xOffset + 200, yOffset + halfHeight, 0,
                        xOffset + 220, yOffset + halfHeight - 10, 0, color, 1);
                target.drawLine(xOffset + 200, yOffset + halfHeight, 0,
                        xOffset + 220, yOffset + halfHeight + 10, 0, color, 1);
            }

            xOffset += 300;
            // third box
            target.drawLine(xOffset, yOffset, 0, xOffset + 200, yOffset, 0,
                    color, 1);
            target.drawLine(xOffset, yOffset, 0, xOffset, yOffset + boxHeight,
                    0, color, 1);
            target.drawLine(xOffset + 200, yOffset, 0, xOffset + 200, yOffset
                    + boxHeight, 0, color, 1);
            target.drawLine(xOffset, yOffset + boxHeight, 0, xOffset + 200,
                    yOffset + boxHeight, 0, color, 1);
            drawNexradString("WFO", xOffset + 85, yOffset + 58, target, color);
        }
    }

    /**
     * Convenience function to draw strings for the nexrad product
     * 
     * @param text
     * @param xOffset
     * @param yOffset
     * @param target
     * @throws VizException
     */
    public static void drawNexradString(String text, int xOffset, int yOffset,
            IGraphicsTarget target, RGB color) throws VizException {
        target.drawString((IFont) Font.getFont("serif"), text, xOffset,
                yOffset, 0, TextStyle.NORMAL, color, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, (Double) 0.0d);
    }
}
