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
package com.raytheon.uf.common.monitor.scan;

public class SiteMessage {
    
    public static double HIGHPCT = 70.0;
    public static double MIDPCT = 40.0;
    public static double LOWPCT = 10.0;
    
    public static ThreatReport getSiteMessage(ThreatReport threat) {

        // vil message portion
        if (threat.getAreaPct() == 0) {
            threat.setVilMessage("NO VIL DETECTED");
        } else {
            if (threat.getLgtPct() < LOWPCT) {
                if ((threat.getMdtPct() == 0.0) && (threat.getHvyPct() == 0.0)) {
                    threat.setVilMessage("ISOLATED LIGHT VIL");
                } else if (threat.getHvyPct() > 0.0) {
                    threat.setVilMessage("ISOLATED HEAVY VIL");
                } else if ((threat.getMdtPct() > 0.0)
                        && (threat.getHvyPct() == 0.0)) {
                    threat.setVilMessage("ISOLATED MODERATE VIL");
                }
            }

            else if (threat.getLgtPct() < MIDPCT) {
                if ((threat.getMdtPct() == 0.0) && (threat.getHvyPct() == 0.0)) {
                    threat.setVilMessage("WIDELY SCT LIGHT VIL");
                } else if ((threat.getHvyPct() > 0.0)
                        && (threat.getHvyPct() < LOWPCT)) {
                    threat.setVilMessage("ISOLATED HEAVY VIL");
                } else if (threat.getHvyPct() >= LOWPCT) {
                    threat.setVilMessage("WIDELY SCATTERED HEAVY VIL");
                } else if ((threat.getHvyPct() == 0.0)
                        && (threat.getMdtPct() > 0.0)
                        && (threat.getMdtPct() < LOWPCT)) {
                    threat.setVilMessage("ISOLATED MODERATE VIL");
                } else if ((threat.getHvyPct() == 0.0)
                        && (threat.getMdtPct() >= LOWPCT)) {
                    threat.setVilMessage("WIDELY SCATTERED MODERATE VIL");
                }
            }

            else if (threat.getLgtPct() < HIGHPCT) {
                if ((threat.getMdtPct() == 0.0) && (threat.getHvyPct() == 0.0)) {
                    threat.setVilMessage("SCT LIGHT VIL");
                } else if ((threat.getHvyPct() > 0.0)
                        && (threat.getHvyPct() < LOWPCT)) {
                    threat
                            .setVilMessage("ISOLD HVY VIL EMBDD WITHIN LGT VIL AREA");
                } else if ((threat.getHvyPct() >= LOWPCT)
                        && (threat.getHvyPct() < MIDPCT)) {
                    threat
                            .setVilMessage("WDLY SCT HVY VIL EMBDD WITHIN LGT VIL AREA");
                } else if (threat.getHvyPct() >= MIDPCT) {
                    threat
                            .setVilMessage("SCT HVY VIL EMBDD WITHIN LGT VIL AREA");
                } else if ((threat.getHvyPct() == 0.0)
                        && (threat.getMdtPct() > 0.0)
                        && (threat.getMdtPct() < LOWPCT)) {
                    threat
                            .setVilMessage("ISOLD MDT VIL EMBDD WITHIN LGT VIL AREA");
                } else if ((threat.getHvyPct() == 0.0)
                        && (threat.getMdtPct() >= LOWPCT)
                        && (threat.getMdtPct() < MIDPCT)) {
                    threat
                            .setVilMessage("WDLY SCT MDT VIL EMBDD WITHIN LGT VIL AREA");
                } else if ((threat.getHvyPct() == 0.0)
                        && (threat.getMdtPct() >= MIDPCT)) {
                    threat
                            .setVilMessage("SCT MDT VIL EMBDD WITHIN LGT VIL AREA");
                }
            }

            else {
                if ((threat.getMdtPct() == 0.0) && (threat.getHvyPct() == 0.0)) {
                    threat.setVilMessage("WIDESPREAD LIGHT VIL");
                } else if ((threat.getHvyPct() > 0.0)
                        && (threat.getHvyPct() < LOWPCT)) {
                    threat
                            .setVilMessage("ISOLD HVY VIL EMBDD IN WDSPRD LGT VIL AREA");
                } else if ((threat.getHvyPct() >= LOWPCT)
                        && (threat.getHvyPct() < MIDPCT)) {
                    threat
                            .setVilMessage("WDLY SCT HVY VIL EMBDD IN WDSPRD LGT VIL AREA");
                } else if ((threat.getHvyPct() >= MIDPCT)
                        && (threat.getHvyPct() < HIGHPCT)) {
                    threat
                            .setVilMessage("SCT HVY VIL EMBDD WITHIN WDSPRD LGT VIL AREA");
                } else if (threat.getHvyPct() >= HIGHPCT) {
                    threat.setVilMessage("WIDESPREAD HEAVY VIL");
                } else if ((threat.getHvyPct() == 0.0)
                        && (threat.getMdtPct() > 0.0)
                        && (threat.getMdtPct() < LOWPCT)) {
                    threat
                            .setVilMessage("ISOLD MDT VIL EMBDD IN WDSPRD LGT VIL AREA");
                } else if ((threat.getHvyPct() == 0.0)
                        && (threat.getMdtPct() >= LOWPCT)
                        && (threat.getMdtPct() < MIDPCT)) {
                    threat
                            .setVilMessage("WDLY SCT MDT VIL EMBDD IN WDSPRD LGT VIL AREA");
                } else if ((threat.getHvyPct() == 0.0)
                        && (threat.getMdtPct() >= MIDPCT)
                        && (threat.getMdtPct() < HIGHPCT)) {
                    threat
                            .setVilMessage("SCT MDT VIL EMBDD WITHIN WDSPRD LGT VIL AREA");
                } else if ((threat.getHvyPct() == 0.0)
                        && (threat.getMdtPct() > HIGHPCT)) {
                    threat.setVilMessage("WIDESPREAD MODERATE VIL");
                }
            }
        }

        if (threat.getCgRateTenNm() == 0) {
            threat.setLgtMessage("NO CG LTG");
        } else if (threat.getCgRateTenNm() > 0) {
            //     
            // Rules for strike frequency classification are set by
            // NWS guidelines as described by Stern[July 1994].
            //                                       
            if (threat.getCgRateTenNm() < 1.0) {
                threat.setLgtMessage(threat.getCgRateTenNm() + " OCNL CG LTG");
            }
            if ((threat.getCgRateTenNm() >= 1.0)
                    && (threat.getCgRateTenNm() <= 6.0)) {
                threat.setLgtMessage(threat.getCgRateTenNm() + " FREQ CG LTG");
            }
            if (threat.getCgRateTenNm() > 6.0) {
                threat.setLgtMessage(threat.getCgRateTenNm() + " CONT CG LTG");
            }
        } else {
            threat.setLgtMessage("LIGHTNING DATA N/A");
        }

        //
        // Choose the appropriate thunderstorm message based on the conditions
        // calculated by the evaluateThreat decision tree.
        //
        switch (threat.getCondition()) {
        case 1:
            threat.setTstormMessage("Thunderstorm threat:");
            threat.setThreatMessage("Cell with strong VIL & ltg within 10NM.");
            break;
        case 2:
            threat.setTstormMessage("Thunderstorm threat:\n");
            threat.setThreatMessage("Widespread VIL & ltg within 10NM.");
            break;
        case 3:
            threat.setTstormMessage("Thunderstorm threat:\n");
            threat
                    .setThreatMessage("Cell with strong VIL within 10NM;  No CG LTG detected.");
            break;
        case 4:
            threat.setTstormMessage("Thunderstorm threat:\n");
            threat
                    .setThreatMessage("40 dBZ+ return (and VIL or LTG) within 10NM.");
            break;
        case 5:
            threat.setTstormMessage("Thunderstorm threat:\n");
            threat.setThreatMessage("VIL and CG LTG within 10NM.");
            break;
        case 6:
            threat.setTstormMessage("No threat detected.");
            threat.setThreatMessage("(Ground clutter or AP detected).");
            break;
        case 7:
            threat.setTstormMessage("No threat detected.");
            threat.setThreatMessage("(Radar and lightning evaluation)");
            break;
        case 8:
            threat.setTstormMessage("Thunderstorm threat:");
            threat
                    .setThreatMessage("Cell with strong VIL within 10NM;  Lightning data N/A.");
            break;
        case 9:
            threat.setTstormMessage("Thunderstorm threat:");
            threat
                    .setThreatMessage("40 dBZ+ return (and VIL) within 10NM;  Lightning data N/A.");
            break;
        case 10:
            threat.setTstormMessage("No threat detected.");
            threat
                    .setThreatMessage("(Ground clutter or AP detected;  Lightning data N/A).");
            break;
        case 11:
            threat.setTstormMessage("No threat detected.");
            threat.setThreatMessage("(Lightning data N/A).");
            break;
        case 12:
            threat.setTstormMessage("Thunderstorm threat:");
            threat
                    .setThreatMessage("CG LTG within 10NM;  Radar data missing or corrupt.");
            break;
        case 13:
            threat.setTstormMessage("No threat detected.");
            threat
                    .setThreatMessage("(Lightning-only evaluation;  Radar data missing or corrupt).");
            break;
        case 14:
            threat.setTstormMessage("Threat status UNKNOWN.");
            threat
                    .setThreatMessage("(Lightning data N/A & radar data missing or corrupt).");
            break;
        case -1:
        default:
            threat.setTstormMessage("???. ");
            threat.setThreatMessage("TRW threat evaluation not performed.");
            break;

        }
        return threat;
    }

}
